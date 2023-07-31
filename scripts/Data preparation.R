#Revisión y unión de la base de datos
df2_train<-df_train %>% group_by(id) %>% summarise(Total_mujeres = sum(Mujer_train),
                                                   Mujer_jefehogar = sum(Mujer_jefehogar),
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
train_hogares_final <-train_hogares %>% left_join(df2_train,by="id")
##Creacion de variable Vivienda Propia
train_hogares_final$viviendapropia <-as.factor(ifelse (train_hogares$P5090==1 | train_hogares$P5090==2,1,0))
#Eliminar nulos
sum(is.na(train_hogares_final$educ_jefehogar))
#Base de datos final de train
train_final <- train_hogares_final[!is.na(train_hogares_final$educ_jefehogar),]

#4. GUARDAR BASES DE DATOS DEPURADAS
save(test_final, file = "test_final.Rda")
save(train_final, file = "train_final.Rda")
