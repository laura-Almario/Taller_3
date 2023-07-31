#MODELOS DE REGRESIÓN LINEAL

p_load(glmnet)
p_load(corrr)
p_load(pls)
p_load(EnvStats)

# Modelo OLS
model1_ols<- lm(Ingtotugarr ~ Dominio + viviendapropia
                + Nper + Total_mujeres + Mujer_jefehogar + num_ocupado + 
                  edad_jefehogar + menores_18 + educ_jefehogar +
                  jefehogar_ocupado + ahorro_jefehogar + afiliadosalud, 
                data= train_final
)
summary(model1_ols)

# Predicciones de entrenamiento

predicciones_ols <- predict(model1_ols, newdata = train_final)
summary(predicciones_ols)

# MAE de entrenamiento

mae_ols <- mean(abs((predicciones_ols - train_final$Ingtotugarr)))
paste("Error (mae) de ols:", mae_ols)

p_load(faraway)
p_load(tidyverse)
p_load(skimr)
p_load(DataExplorer)
p_load(scales)
p_load(corrr)
p_load(glmnet)
p_load(pls)

# Matrices 

x_train <- model.matrix(Ingtotugarr~ Nper + Npersug + P5000 + 
                          Total_mujeres + Mujer_jefehogar + num_ocupado + 
                          edad_jefehogar + menores_18 + educ_jefehogar +
                          jefehogar_ocupado + ahorro_jefehogar + afiliadosalud + 
                          viviendapropia,
                        data = train_final)[, -1]
y_train <- train_final$Ingtotugarr

#Modelo Ridge

# Evolución del error en función de lambda

set.seed(1899)
cv_error_ridge <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

cv_error_ridge
plot(cv_error_ridge)

paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error_ridge$lambda.1se)

# Mejor modelo según lambda

modelo_ridge <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  lambda      = cv_error_ridge$lambda.1se,
  standardize = TRUE
)

modelo_ridge

# Coeficientes

df_coeficientes_ridge <- coef(modelo_ridge) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

# Predicciones de entrenamiento

predicciones_train_ridge <- predict(modelo_ridge, newx = x_train)

# MAE de entrenamiento

mae_ridge <- mean(abs(predicciones_train_ridge - y_train))
paste("Error (mae) de ridge", mae_ridge)

# Modelo 3 - Lasso

# Creación y entrenamiento del modelo

modelo_lasso <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 100,
  standardize = TRUE
)

# Evolución de los coeficientes en función de lambda

regularizacion_lasso <- modelo_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo_lasso$lambda)

regularizacion_lasso <- regularizacion_lasso %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion_lasso %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")

# Evolución del error en función de lambda

set.seed(1899)
cv_error_lasso <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error_lasso)
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error_lasso$lambda.1se)

# Mejor modelo según el lambda

modelo_lasso <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error_lasso$lambda.1se,
  standardize = TRUE
)

# Coeficientes del modelo

df_coeficientes_lasso <- coef(modelo_lasso) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes_lasso %>%
  filter(
    predictor != "(Intercept)",
    coeficiente != 0
  ) 

# Predicciones de entrenamiento

predicciones_train_lasso <- predict(modelo_lasso, newx = x_train)

# MAE de entrenamiento

mae_lasso <- mean(abs(predicciones_train_lasso - y_train))
print(paste("Error (mae) de lasso", mae_lasso))

##GENERA ERROR, valor NULL##
X <-model.matrix(~Nper + Npersug + P5000 + 
                   Total_mujeres + Mujer_jefehogar + num_ocupado + 
                   edad_jefehogar + menores_18 + educ_jefehogar +
                   jefehogar_ocupado + ahorro_jefehogar + afiliadosalud + 
                   viviendapropia, train_final)
X <- X[,-1]
train_final$Ingreso_predicho_hogares<-predict(modelo_lasso,newx = X)

#Prediccion final Modelo Clasificaci?n

summary(train_final$Ingreso_predicho_hogares)

train_final$Ing_Pred_test_hogares<-factor(ifelse(train_final$Ingreso_predicho_hogares<=train_final$Lp,1,0))
train_final$Ing_Pred_test_hogares<- factor((train_final$Ing_Pred_test_hogares), 
                                              levels = c(0, 1)
                                             )

train_final$Ing_Pred_test_hogares

#Archivo de predicciones
id_test_hogares<-test_hogares$id
Pobre_Pred_test_hogares<-test_hogares$Pobre_predicho_final
Pobre_Pred_test_hogares
Ing_Pred_test_hogares<-test_hogares$Ing_Pred_test_hogares
test_hog<-data_frame(id_test_hogares,Pobre_Pred_test_hogares,Ing_Pred_test_hogares)
summary(test_hog)
