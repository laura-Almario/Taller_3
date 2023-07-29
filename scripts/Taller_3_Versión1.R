#Taller 3 - gracias a Dios!

install.packages('pacman')
require(pacman)
p_load(rio)
p_load(tidyverse)
p_load(e1071)
p_load(tidymodels)
p_load(ggplot2)
p_load(scales)
p_load(ggpubr)
p_load(skimr,
       caret,
       rvest,
       stringr,
       dplyr,
       robotstxt
)

#1. IDENTIFICACIÓN DE LA INFORMACIÓN A ANALIZAR
#Unión de la base de datos
train <- left_join(train_hogares, train_personas, by='id')
test  <- merge(test_hogares, test_personas, by='id')

#Identificación de datos
summary(train)
glimpse(train)

#Convertimos las variables categoricas por factores
train <- train %>% mutate_at(.vars= c("Clase.x", "Dominio.x","P5090", "Pobre", "Indigente",
                                      "Depto.x", "Clase.y", "Dominio.y", "Estrato1", "P6020",
                                      "P6050", "P6090", "P6100", "P6210", "P6240", "Oficio",
                                      "P6430", "P6510", "P6510s2", "P6545", "P6545s2", "P6580",
                                      "P6580s2", "P6585s1", "P6585s1a2", "P6585s2", "P6585s2a2",
                                      "P6585s3", "P6585s3a2", "P6585s4", "P6585s4a2", "P6590",
                                      "P6600", "P6610", "P6620", "P6630s1", "P6630s4","P6630s2",
                                      "P6630s3","P6870", "P6920", "P7040", "P7050", "P7090", "P7110",
                                      "P7120", "P7140s1","P7140s2","P7150","P7160","P7310","P7150",
                                      "P7350","P7422","P7472","P7495","P7500s1","P7500s2","P7500s3",
                                      "P7505","P7510s1","P7510s2","P7510s3", "P7510s5","P7510s6","P7510s7",
                                      "Pet", "Oc", "Des", "Ina", "Depto.y" ),.funs = factor)

test <- test %>% mutate_at(.vars = c("Clase.x", "Dominio.x","P5090","Depto.x", "Clase.y", 
                      "Dominio.y", "P6020","P6050", "P6090", "P6100",
                      "P6210", "P6240", "Oficio","P6430", "P6510",
                      "P6545", "P6580","P6585s1", "P6585s2", "P6585s3", 
                      "P6585s4", "P6590","P6600", "P6610", "P6620", "P6630s1",
                      "P6630s4","P6630s2","P6630s3","P6870", "P6920", "P7040",
                      "P7050", "P7090", "P7110","P7120", "P7150","P7160",
                      "P7310","P7150","P7350","P7422","P7472","P7495",
                      "P7500s2","P7500s3","P7505","P7510s1","P7510s2",
                      "P7510s3", "P7510s5","P7510s6","P7510s7",
                      "Pet", "Oc", "Des", "Ina", "Depto.y" ),.funs = factor)

#Analisis y eliminación de valores nulos para poder depurar las bases de datos
#Para train
nulos_train <- sapply(train, function(x) sum(is.na(x)))
nulos_train <- data.frame(nulos_train)
nulos_train <- data.frame(nulos_train)%>% rownames_to_column('variable')
nulos_train$nulos_porcentual <- nulos_train$nulos_train/nrow(train)
nulos_train$nulos_porcentual
drop <- nulos_train$nulos_porcentual > 0.80
eliminar <- nulos_train$varible[drop]
train <- train %>% select(-eliminar)

#Variable compartidas entre train y test
x <- ls(train)
y <- ls(test)
variables_compartidas <- list()

for(i in 1:length(y)){
  for(j in 1:length(x)){
    if (y[i]==x[j]){
      variables_compartidas <- append(variables_compartidas, y[i])
    }
  }
}
variables_compartidas

#2. DEPURACIÓN DE LA BASE DE DATOS TEST

#Para test
nulos_test <- sapply(test, function(x) sum(is.na(x)))
nulos_test <- data.frame(nulos_test)
nulos_test<- data.frame(nulos_test)%>% rownames_to_column("variable")
nulos_test$nulos_porcentual <- nulos_test$nulos_test/nrow(test)
nulos_test$nulos_porcentual 
drop <- nulos_test$nulos_porcentual > 0.80
eliminar_test <- nulos_test$varible[drop]
test <- test %>% select(-eliminar_test)

#Definir las variables para el modelo
#DEJAR COMENTARIO DE CUALES SON LAS VARIABLES QUE QUERMOS USAR EN EL MODELO

#Filtro de mujer
Mujer_test<-ifelse(test$P6020==2,1,0)
#Filtro de jefe de hogar
jefehogar_test<-ifelse(test$P6050==1,1,0)
#Filtro trabajo formal
Jefehogar_tipoEmpleo <- ifelse(test$P6920 == 1, 1, 0)
#Fitro id
id<-test$id
#Creación del dataframe
df_test<-data_frame(id,Mujer_test,jefehogar_test, Jefehogar_tipoEmpleo)
#Definición de mujer como jefe de hogar
df_test$Mujer_jefehogar<-ifelse(df_test$jefehogar_test==1,df_test$Mujer_test,0)
#Fitro de ocupación del jefe de hogar
df_test$ocupado <-ifelse(is.na(test$Oc),0,1)
df_test$jefehogar_ocupado <-ifelse(df_test$jefehogar_test==1,df_test$ocupado,0)
#Fitro de edad
df_test$edad<-test$P6040
df_test$edad_jefehogar<-ifelse(df_test$jefehogar_test==1,test$P6040,0)
#Filtro de menores de edad
df_test$menores_18 <-ifelse(df_test$edad<18,1,0)
#Filtro de educación del jefe de hogar
df_test$educ_jefehogar <-ifelse(df_test$jefehogar_test==1,test$P6210s1,0)
#Filtro afiliado a salud
df_test$afiliado<-ifelse(test$P6090!=1 | is.na(test$P6090),0,1)
#Filtro ahorro del jefe de hogar y definición de su valor
df_test$ahorro_jefehogar<-(ifelse(test$P7510s5!=1 | is.na(test$P7510s5),0,1))
df_test$ahorro_jefehogar<-(ifelse(df_test$jefehogar_test==1 & df_test$ahorro_jefehogar==1,1,0))

#Revisión y unión de la base de datos
df2_test<-df_test %>% group_by(id) %>% summarise(Total_mujeres = sum(Mujer_test),
                                                 Mujer_jefehogar = sum(Mujer_jefehogar),
                                                 Jefehogar_tipoEmpleo = sum(Jefehogar_tipoEmpleo),
                                                 num_ocupado = sum(ocupado),
                                                 edad_jefehogar = sum(edad_jefehogar),
                                                 menores_18= sum(menores_18),
                                                 educ_jefehogar= sum(educ_jefehogar), 
                                                 jefehogar_ocupado= sum(jefehogar_ocupado),
                                                  afiliadosalud = sum(afiliado),
                                                 ahorro_jefehogar=sum(ahorro_jefehogar)
)
#Unión de las bases de datos de test hogares con el df2_test
test_hogares_final <-test_hogares %>% left_join(df2_test,by="id")
#Fitro vivienda propia como factor
test_hogares_final$viviendapropia <-as.factor(ifelse (test_hogares$P5090==1 | test_hogares$P5090==2,1,0))
#Eliminar nulos
sum(is.na(test_hogares_final$educ_jefehogar))
#Base de datos final de test
test_final <- test_hogares_final[!is.na(test_hogares_final$educ_jefehogar),]

#4. DEPURACIÓN DE LA BASE DE DATOS TRAIN

#Definir las variables para el modelo
#DEJAR COMENTARIO DE CUALES SON LAS VARIABLES QUE QUERMOS USAR EN EL MODELO

#Filtro de mujer
Mujer_train<-ifelse(train$P6020==2,1,0)
#Filtro de jefe de hogar
jefehogar_train<-ifelse(train$P6050==1,1,0)
#Filtro trabajo formal
Jefehogar_tipoEmpleo <- ifelse(train$P6920 == 1, 1, 0)
#Fitro id
id<-train$id
#Creación del dataframe
df_train<-data_frame(id,Mujer_train,jefehogar_train)
#Definición de mujer como jefe de hogar
df_train$Mujer_jefehogar<-ifelse(df_train$jefehogar_train==1,df_train$Mujer_train,0)
#Fitro de ocupación del jefe de hogar
df_train$ocupado <-ifelse(is.na(train$Oc),0,1)
df_train$jefehogar_ocupado <-ifelse(df_train$jefehogar_train==1,df_train$ocupado,0)
#Fitro de edad
df_train$edad<-train$P6040
df_train$edad_jefehogar<-ifelse(df_train$jefehogar_train==1,train$P6040,0)
#Filtro de menores de edad
df_train$menores_18 <-ifelse(df_train$edad<18,1,0)
#Filtro de educación del jefe de hogar
df_train$educ_jefehogar <-ifelse(df_train$jefehogar_train==1,train$P6210s1,0)
#Filtro afiliado a salud
df_train$afiliado<-ifelse(train$P6090!=1 | is.na(train$P6090),0,1)
#filtro ahorro del jefe de hogar y definición de su valor
df_train$ahorro_jefehogar<-(ifelse(train$P7510s5!=1 | is.na(train$P7510s5),0,1))
df_train$ahorro_jefehogar<-(ifelse(df_train$jefehogar_train==1 & df_train$ahorro_jefehogar==1,1,0))
#Filtro Estrato
df_train$Estrato_jefehogar<-ifelse(df_train$jefehogar_train==1,train$Estrato1,0)
#Filtro ingresos
df_train$Ingtot<- train$Ingtot
df_train$Ingtot_jefehogar<-ifelse(df_train$jefehogar_train==1,df_train$Ingtot, 0)

#Revisión y unión de la base de datos
df2_train<-df_train %>% group_by(id) %>% summarise(Total_mujeres = sum(Mujer_train),
                                                   Mujer_jefehogar = sum(Mujer_jefehogar),
                                                   Jefehogar_tipoEmpleo = sum(Jefehogar_tipoEmpleo),
                                                   num_ocupado = sum(ocupado),
                                                   edad_jefehogar = sum(edad_jefehogar),
                                                   menores_18= sum(menores_18),
                                                   educ_jefehogar= sum(educ_jefehogar), 
                                                   jefehogar_ocupado= sum(jefehogar_ocupado),
                                                   afiliadosalud = sum(afiliado),
                                                   ahorro_jefehogar=sum(ahorro_jefehogar),
                                                   Estrato_jefehogar=sum(Estrato_jefehogar),
                                                  Ingtot_jefehogar = sum(Ingtot_jefehogar)
)
#Base de datos final de train
train_final <-train_hogares %>% left_join(df2_train,by="id")

#5. GUARDAR BASES DE DATOS DEPURADAS
save(test_final, file = "test_final.Rda")
save(train_final, file = "train_final.Rda")