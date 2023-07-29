#Estadisticas descriptivas
load("~/GitHub/Taller_3/stores/test_final.Rda")
load("~/GitHub/Taller_3/stores/train_final.Rda")

install.packages('stargazer')
install.packages('skimr')
install.packages("gtsummary")
library(gtsummary)
library(stargazer)
library(skimr)
library(pacman)
p_load(rio)
p_load(tidyverse)
p_load(e1071)
p_load(tidymodels)
p_load(ggplot2)
p_load(scales)
p_load(ggpubr)
p_load(xlsx)
p_load(skimr,
       caret,
       rvest,
       stringr,
       dplyr,
       robotstxt
)

train_final <- train_final %>% mutate_at(.vars = c("Clase", "Dominio","P5090", "Pobre", "Indigente",
                      "Depto", "Mujer_jefehogar", "num_ocupado", "Estrato_jefehogar" ),.funs = factor)
skim_data <- skim(train_final)
write.xlsx(skim_data, file = "tabla_estadisticasskim.xlsx")

#Verificación de missing values
sum(is.na(train_final$Ingtot_jefehogar))
sum(is.na(train_final$educ_jefehogar))

#Imputación de datos
train_final = train_final %>%
  mutate(Ingtot_jefehogar = ifelse(is.na(Ingtot_jefehogar),
                            yes = Ingtotugarr,
                            no = Ingtot_jefehogar))

#Eliminar NA en max_edu_jh
train_final <- train_final[!is.na(train_final$educ_jefehogar),]

##Creacion de variable Vivienda Propia
train_final$viviendapropia <-as.factor(ifelse (train_final$P5090==1 | train_final$P5090==2,1,0))

data_subset1 <- train_final[c("Nper", "Ingtotugarr", "Total_mujeres", "num_ocupado")] 
data_subset1 <- summary(data_subset1)

data_subset2 <- train_final[c("menores_18", "Ingtot_jefehogar", "educ_jefehogar", "afiliadosalud")]
data_subset2 <- summary(data_subset2)

write.xlsx(data_subset1, file = "tabla_estadisticas1.xlsx")
write.xlsx(data_subset2, file = "tabla_estadisticas2.xlsx")

#Graficas de estadistica descriptiva

#Analisis de ingresos vs pobre
graf_ingresos <- ggplot(data=train_final , mapping = aes(as.factor(Pobre) , Ingtotugarr)) + 
  geom_boxplot()

graf_ingresos <- graf_ingresos +
  scale_fill_grey() + theme_classic()+
  labs(x= "Pobres (1 - Probre 0 - No pobre)", y ="Ingresos Totales de los Hogares") 

graf_ingresos

#Analisis ingresos log vs pobre
train_final$log_ingh <- log(train_final$Ingtotugarr+1)

graf_ingresos2 <- ggplot(data=train_final , mapping = aes(as.factor(Pobre) , log_ingh)) + 
  geom_boxplot()

graf_ingresos2 <- graf_ingresos2 +
  scale_fill_grey() + theme_classic()+
  labs(x= "Pobres (1 - Probre 0 - No pobre)", y =" Logaritmo Ingresos Totales de los Hogares")

graf_ingresos2

#Grafico comparativo de ingresos / ingreso log vs pobre
ingreso_comparativo <- ggarrange(graf_ingresos, graf_ingresos2)

ingreso_comparativo

#Grafica de analisis de la variable pobre
pobre <- ggplot(data = train_final, aes(x = Pobre, fill=Pobre)) +
  geom_bar() +
  labs (subtitle="Base train",
        x= "Pobre = 1", y = "Cantidad de Pobres")
pobre

#Grafica de analisis de la variable indigente
indigente <- ggplot(data = train_final, aes(x = Indigente, fill=Indigente)) +
  geom_bar() +
  labs (subtitle="Base train",
        x= "indigente = 1", y = "Cantidad de indigentes")
indigente

#Grafico analisis de la variable mujer
filtered_data <- train_final[train_final$Mujer_jefehogar == 1, ]
grafica_mujeres <- ggplot(data = train_final, aes(x = Mujer_jefehogar, fill = "Cantidad de Mujeres")) +
  geom_bar() +
  geom_bar(data = filtered_data, aes(x = jefehogar_ocupado, fill = "Canditad de Mujeres Trabajando")) +
  labs(title = "Analisis de Mujeres vs Mujeres jefes de hogar trabajando",
       x = "Variable",
       y = "Count") +
  scale_x_discrete(labels = c("Total Mujeres", "Jefehogar Ocupado")) +
  theme_minimal()

grafica_mujeres


grafica_mujeres <- ggplot() +
  geom_bar(data = train_final, aes(x = ifelse(Mujer_jefehogar == 1, "Mujer jefes de hogares", "Otro"), fill = "Cantidad de Mujeres")) +
  geom_bar(data = filtered_data, aes(x = ifelse(jefehogar_ocupado == 1, "Mujeres jefes de hogares trabajando", "Otro"), fill = "Cantidad de Mujeres Trabajando")) +
  labs(title = "Analysis of Women Household Heads vs. Working Women Household Heads",
       x = "Variable",
       y = "Count") +
  theme_minimal()

grafica_mujeres