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
