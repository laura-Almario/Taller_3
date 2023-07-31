#5. MODELOS DE PREDICCIÓN DE POBREZA

#Probabilidad de hogar pobre
train_final$Pobre <-as.factor(train_final$Pobre)
set.seed(1899)

#Modelo 1

model_log_1 <- glm(Pobre ~ Total_mujeres + Mujer_jefehogar +
                     num_ocupado + edad_jefehogar + menores_18 + educ_jefehogar + 
                     jefehogar_ocupado + afiliadosalud + ahorro_jefehogar + viviendapropia,
                   data= train_final,
                   family=binomial(link="logit"))

summary(model_log_1)

#Modelo 2 - Caracteristicas del Hogar

model_log_2 <- glm(Pobre ~ Total_mujeres  + num_ocupado  + menores_18  +  
                     afiliadosalud + viviendapropia,
                   family=binomial(link="logit"),
                   data= train_final)

summary(model_log_2)

#Modelo 3 - Caracteristicas del Jefe de Hogar

model_log_3 <- glm( Pobre ~  Mujer_jefehogar + edad_jefehogar + educ_jefehogar + 
                      jefehogar_ocupado + ahorro_jefehogar + Ingtot_jefehogar + Estrato_jefehogar,
                    family=binomial(link="logit"),
                    data= train_final)

summary(model_log_3)


#Modelo 4 - Mujeres

model_log_4 <- glm(Pobre ~ Total_mujeres + Mujer_jefehogar + edad_jefehogar + educ_jefehogar + 
                     jefehogar_ocupado + ahorro_jefehogar,
                   data= train_final,
                   family=binomial(link="logit"))


summary(model_log_4)

install.packages("stargazer")
library(stargazer)

stargazer(model_log_1, model_log_2, model_log_3, model_log_4, type = "text")

#Predicciones Logit

library("dplyr") 
library("gamlr") 

train_final$y_hat_1 <- predict(model_log_1, newdata=train_final, type="response")
train_final$y_hat_2 <- predict(model_log_2, newdata=train_final, type="response")
train_final$y_hat_3 <- predict(model_log_3, newdata=train_final, type="response")
train_final$y_hat_4 <- predict(model_log_4, newdata=train_final, type="response")

summary(train_final$y_hat_1)
summary(train_final$y_hat_2)
summary(train_final$y_hat_3)
summary(train_final$y_hat_4)


# Definir regla

rule=0.5
train_final$pobre_prob1 = ifelse(train_final$y_hat_1>rule,1,0)
train_final$pobre_prob2 = ifelse(train_final$y_hat_2>rule,1,0)
train_final$pobre_prob3 = ifelse(train_final$y_hat_3>rule,1,0)
train_final$pobre_prob4 = ifelse(train_final$y_hat_4>rule,1,0)

# Matriz clasificaci?n

cm_logit1 = confusionMatrix(data=factor(train_final$pobre_prob1) , 
                            reference=factor(train_final$Pobre) , 
                            mode="sens_spec" , positive="1")

cm_logit2 = confusionMatrix(data=factor(train_final$pobre_prob2) , 
                            reference=factor(train_final$Pobre) , 
                            mode="sens_spec" , positive="1")

cm_logit3 = confusionMatrix(data=factor(train_final$pobre_prob3) , 
                            reference=factor(train_final$Pobre) , 
                            mode="sens_spec" , positive="1")

cm_logit4 = confusionMatrix(data=factor(train_final$pobre_prob4) , 
                            reference=factor(train_final$Pobre) , 
                            mode="sens_spec" , positive="1")

##Matriz de confusión para cada modelo
cm1 <- cm_logit1$table
cm2 <- cm_logit2$table
cm3 <- cm_logit3$table
cm4 <- cm_logit4$table

##Metricas para cada modelo
metricas_cm1 <- cm_logit1$byClass
metricas_cm2 <- cm_logit2$byClass
metricas_cm3 <- cm_logit3$byClass
metricas_cm4 <- cm_logit4$byClass

Metricas_modelos <-  rbind(metricas_cm1, metricas_cm2, metricas_cm3, metricas_cm4)
Metricas_modelos <- Metricas_modelos[,-8]

require(xtable)

analisis_metricas <- xtable(Metricas_modelos)

install.packages("openxlsx")
library(openxlsx)

write.xlsx(analisis_metricas, file = "analisis_metricas.xlsx")

# Graficas ROC y AUC 

install.packages("plotROC")
library(plotROC)
library(ggplot2)
library(pROC)

roc1 <- roc(train_final$Pobre, train_final$y_hat_1)
auc1 <- round(auc(train_final$Pobre, train_final$y_hat_1),4)
model1 <- paste('Model 1 (AUC=',toString(round(auc1,2)),')',sep = '')

roc2 <- roc(train_final$Pobre, train_final$y_hat_2)
auc2 <- round(auc(train_final$Pobre, train_final$y_hat_2),4)
model2 <- paste('Model 2 (AUC=',toString(round(auc2,2)),')',sep = '')

roc3 <- roc(train_final$Pobre, train_final$y_hat_3)
auc3 <- round(auc(train_final$Pobre, train_final$y_hat_3),4)
model3 <- paste('Model 3 (AUC=',toString(round(auc3,2)),')',sep = '')

roc4 <- roc(train_final$Pobre, train_final$y_hat_4)
auc4 <- round(auc(train_final$Pobre, train_final$y_hat_4),4)
model4 <- paste('Model 4 (AUC=',toString(round(auc4,2)),')',sep = '')