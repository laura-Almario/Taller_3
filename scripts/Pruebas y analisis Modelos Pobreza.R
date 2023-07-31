install.packages("plotly")
install.packages("tidymodels")
Install.packages("fastDummies")
library(plotly)
library(tidymodels)
library(fastDummies)

fig <- plot_ly()%>%
  add_segments(x = 0, xend = 1, y = 0, yend = 1, line = list(dash = "dash", color = 'black'), showlegend = FALSE) %>%
  add_trace(data = roc1, x = (1-roc1$specificities), y = roc1$sensitivities, mode = 'lines', name = model1, type = 'scatter')%>%
  add_trace(data = roc2 ,x = (1-roc2$specificities), y = roc2$sensitivities, mode = 'lines', name = model2, type = 'scatter')%>%
  add_trace(data = roc3 ,x = (1-roc3$specificities), y = roc3$sensitivities, mode = 'lines', name = model3, type = 'scatter')%>%
  add_trace(data = roc4 ,x = (1-roc4$specificities), y = roc4$sensitivities, mode = 'lines', name = model4, type = 'scatter')%>%
  layout(xaxis = list(
    title = "False Positive Rate"
  ), yaxis = list(
    title = "True Positive Rate"
  ),legend = list(x = 100, y = 0.5))
fig

require(caret)

train_final$Pobre<- factor((train_final$Pobre), 
                           levels = c(0, 1), 
                           labels = c("No", "Si"))


#Entrenamiento
set.seed(1899)
division1 <- createDataPartition(train_final$Pobre, p = .7)[[1]]
length(division1)

## Prueba y evaluación
other <- train_final[-division1,]
training <- train_final[ division1,]
set.seed(1899)
division2 <- createDataPartition(other$Pobre, p = 1/3)[[1]]
evaluation <- other[ division2,]
testing <- other[-division2,]

dim(training)
dim(evaluation)
dim(testing)

summary(training$Pobre)

##Modelo seleccionado: Modelo 2

#Modelo logit Cross validation
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

ctrl_pobre <- trainControl(method = "cv",
                           number = 5,
                           summaryFunction = fiveStats,
                           classProbs = TRUE,
                           verbose=FALSE,
                           savePredictions = T)

logit_caret_pob <- train(Pobre ~ factor(viviendapropia) + Total_mujeres + num_ocupado 
                         + afiliadosalud + menores_18,
                         data = training,
                         method = "glm",
                         trControl = ctrl_pobre,
                         family = "binomial",
                         preProcess = c("center", "scale")
)

logit_caret_pob

#Modelo logit lasso Cross validation

lambda_grid <- 10^seq(-4, 0.01, length = 100)
lambda_grid
set.seed(1899)
logit_lasso<- train(Pobre ~ factor(viviendapropia) + Total_mujeres + num_ocupado 
                    + afiliadosalud + menores_18,
                    data = training,
                    method = "glmnet",
                    trControl = ctrl_pobre,
                    family = "binomial",
                    metric = "Accuracy",
                    tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
                    preProcess = c("center", "scale")
)
logit_lasso
logit_lasso[["bestTune"]]
### Posiciòn 54 - Mejor lambda = 0.01402063

# Modelo lasso Roc
logit_lasso_ROC<- train(Pobre ~ factor(viviendapropia) + Total_mujeres + num_ocupado 
                        + afiliadosalud + menores_18,
                        data = training,
                        method = "glmnet",
                        trControl = ctrl_pobre,
                        family = "binomial",
                        metric = "ROC",
                        tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
                        preProcess = c("center", "scale")
)
logit_lasso_ROC
logit_lasso_ROC[["bestTune"]]
### Posiciòn 54 - Mejor lambda = 0.01402063

# Modelo lasso Sens
logit_lasso_sens<- train(Pobre ~ factor(viviendapropia) + Total_mujeres + num_ocupado 
                         + afiliadosalud + menores_18,
                         data = training,
                         method = "glmnet",
                         trControl = ctrl_pobre,
                         family = "binomial",
                         metric = "Sens",
                         tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
                         preProcess = c("center", "scale")
)
logit_lasso_sens
logit_lasso_sens[["bestTune"]]
### Posiciòn 100 - Mejor lambda = 1.023293

# Modelo Lasso Upsampling
set.seed(1899)
upSampledTrain <- upSample(x = training,
                           y = training$Pobre,
                           yname = "Pobre")

dim(training)
dim(upSampledTrain)
table(upSampledTrain$Pobre)

set.seed(1899)
logit_lasso_upsample <- train(Pobre ~ factor(viviendapropia) + Total_mujeres + num_ocupado 
                              + afiliadosalud + menores_18,
                              data = upSampledTrain,
                              method = "glmnet",
                              trControl = ctrl_pobre,
                              family = "binomial",
                              metric = "ROC",
                              tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
                              preProcess = c("center", "scale")
)
logit_lasso_upsample
logit_lasso_upsample[["bestTune"]]
### Posiciòn 57 - Mejor lambda = 0.01854739

#Modelo Lasso Downsampling
set.seed(1899)
downSampledTrain <- downSample(x = training,
                               y = training$Pobre,
                               yname = "Pobre")
dim(training)
dim(downSampledTrain)
table(downSampledTrain$Pobre)

set.seed(1899)
logit_lasso_downsample <- train(Pobre ~ factor(viviendapropia) + Total_mujeres + num_ocupado 
                                + afiliadosalud + menores_18,
                                data = downSampledTrain,
                                method = "glmnet",
                                trControl = ctrl_pobre,
                                family = "binomial",
                                metric = "ROC",
                                tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
                                preProcess = c("center", "scale")
)
logit_lasso_downsample
logit_lasso_downsample[["bestTune"]]
### Posiciòn 61 - Mejor lambda = 0.02693413

###Resultados de las métricas 
result_logitcv <- logit_caret_pob[["results"]]
colnames(result_logitcv)[1]<-"lambda"
result_lassoacc <- logit_lasso[["results"]][55,-1]
result_lassoroc<- logit_lasso_ROC[["results"]][54,-1]
result_lassosens<- logit_lasso_sens[["results"]][100,-1]
result_lassoupsample <- logit_lasso_upsample[["results"]][57,-1]
result_lassodownsample <- logit_lasso_downsample[["results"]][57,-1]

results<-rbind(result_logitcv,result_lassoacc,result_lassoroc, result_lassosens, result_lassoupsample,result_lassodownsample  )
xtable(results)
analisis_modelos <- xtable(results)
write.xlsx(analisis_modelos, file = "analisis_modelos.xlsx")

#Evaluación en test

testResults <- data.frame(Pobre = testing$Pobre)

summary(testing$Pobre)

testResults$logit<- predict(logit_caret_pob,
                            newdata = testing,
                            type = "prob")[,1]

hist(testResults$logit)

testResults$lasso<- predict(logit_lasso,
                            newdata = testing,
                            type = "prob")[,1]

testResults$lasso_roc<- predict(logit_lasso_ROC,
                                newdata = testing,
                                type = "prob")[,1]

testResults$lasso_sens<- predict(logit_lasso_sens,
                                 newdata = testing,
                                 type = "prob")[,1]

testResults$lasso_upsample<- predict(logit_lasso_upsample,
                                     newdata = testing,
                                     type = "prob")[,1]

testResults$lasso_downsample<- predict(logit_lasso_downsample,
                                       newdata = testing,
                                       type = "prob")[,1]

testResults<-testResults %>%
  mutate(logit=ifelse(logit>rfThresh$threshold,"Si","No"),
         lasso=ifelse(lasso>rfThresh$threshold,"Si","No"),
         lasso_roc =ifelse(lasso_roc>rfThresh$threshold,"Si","No"),
         lasso_sens=ifelse(lasso_sens>rfThresh$threshold,"Si","No"),
         lasso_upsample=ifelse(lasso_upsample>rfThresh$threshold,"Si","No"),
         lasso_downsample=ifelse(lasso_downsample>rfThresh$threshold,"Si","No")
  )

with(testResults,table(Pobre,logit))
with(testResults,table(Pobre,lasso))
with(testResults,table(Pobre,lasso_roc))
with(testResults,table(Pobre,lasso_sens))
with(testResults,table(Pobre,lasso_upsample))
with(testResults,table(Pobre,lasso_downsample))

#De acuerdo con los resultados arrojados escojemos el modelo Lasso ROC el cual arroja el menor ROC con 0,76769247

test_final$Pobre_predicho_final<-predict(logit_lasso_downsample,newdata=test_final)

#Prediccion final Modelo Clasificación

summary(test_final$Pobre_predicho_final)
