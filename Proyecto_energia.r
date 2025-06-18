# Proyevto estudio de la base de datos energeticos de varios paises 
library(fastDummies)
library(leaps)
library(glmnet)


# cargamos los datos 
data <- read.csv("C:/Users/Anselmo Daniel/Documents/Cómputo Estádistico/Proyecto Armando Anselmo/complete_renewable_energy_dataset.csv")
#print(colnames(data))
#print(sum(is.na(data)))

# Aplicamso regresión lineal con la variable producción como respuesta 

# Primero transformamos las variables Coutry y Energy_types a variables dummy
data_new <- fastDummies::dummy_cols(
  data, 
  select_columns = c("Country", "Energy.Type"),
  remove_selected_columns = TRUE  
)


#print(dim(data_new))

mod.reg <- lm(Production..GWh. ~ ., data = data_new)
print("ya pasé la primera regresion")
# Resumen del modelo
print(summary(mod.reg))

# Selección de subconjuntos para la base de datos  energeticos de varios paises 

#Método Forward stepwise y backward stepwise
# Primero calculamos la matriz de diseño 

X <- model.matrix(Production..GWh. ~ ., data_new)[, -1]
print("ya pase la x")
Y <- data_new$Production..GWh
#print(head(X))

# Dividimos los datos en prueba y entrenamiento
set.seed(1) #fijamos la semilla para reproducir los resultados
train<-sample (1:nrow(X), 0.8*nrow(X)) # se selecciona aleatoriamente 80 % de obs para el
#conjunto de entrenamiento
test<-(-train) #El 20 % restante sera para probar el modelo (conjunto de validacion)

y.test<-Y[test]

print("voy  a entrar a la selección de subconjuntos")
ajuste_modelofs <- regsubsets(x = X[train ,], y = Y[train], datos = data_new, method =  "forward", nvmax = 68)
#ajuste_modeloback <- regsubsets(x = X[train ,], y = Y[train], datos = data_new, method =  "backward", nvmax = 68)

resumen_subsetsfs <- summary(ajuste_modelofs)

# Encontramos el mejor modelo según R² ajustado, AIC y BIC
mejor_r2 <- which.max(resumen_subsetsfs$adjr2)
mejor_bic <- which.min(resumen_subsetsfs$bic)
mejor_cp <- which.min(resumen_subsetsfs$cp)




cat("El mejor modelo según el R² ajustado tiene", mejor_r2, "predictores.\n")
cat("El mejor modelo según el BIC tiene", mejor_bic, "predictores.\n")
cat("El mejor modelo según el AIC tiene", mejor_cp, "predictores.\n")
#print(resumen_subsets$which)

# Variables seleccionadas en el modelo con mejor R² ajustado
variables_mejor_r2 <- coef(ajuste_modelofs, mejor_r2)
print("Variables seleccionadas según mejor R² ajustado:")
print(names(variables_mejor_r2))

# Variables seleccionadas en el modelo con mejor BIC
variables_mejor_bic <- coef(ajuste_modelofs, mejor_bic)
print("Variables seleccionadas según mejor BIC:")
print(names(variables_mejor_bic))

# Variables seleccionadas en el modelo con mejor Cp
variables_mejor_cp <- coef(ajuste_modelofs, mejor_cp)
print("Variables seleccionadas según mejor Cp:")
print(names(variables_mejor_cp))







#Garfica de R^2 con respecto al número de predictores
png("C:/Users/Anselmo Daniel/Documents/Cómputo Estádistico/Proyecto Armando Anselmo/graf1pAA.png", width=900, height=600)
plot(resumen_subsetsfs$adjr2,
     type = "p", pch = 16, col = rgb(0, 114/255, 178/255, 0.7),  
     xlab = "Número de predictores", 
     ylab = expression(R^2),
     main = expression(bold("Método Forward. Relación entre el número de predictores y"~R^2)),
     cex = 1.8, cex.lab = 1.5, cex.main = 1.8, cex.sub = 1.2)
lines(resumen_subsetsfs$adjr2, col = "red", lwd = 1)
grid(col = "lightgray", lty = "dotted")
dev.off() 


#Garfica de BIC con respecto al número de predictores
png("C:/Users/Anselmo Daniel/Documents/Cómputo Estádistico/Proyecto Armando Anselmo/graf2pAA.png", width=900, height=600)
plot(resumen_subsetsfs$bic,
     type = "p", pch = 16, col = rgb(0, 114/255, 178/255, 0.7),  
     xlab = "Número de predictores", 
     ylab = "BIC",
     main = "Método Forward. Relación entre el número de predictores y BIC",
     cex = 1.8, cex.lab = 1.5, cex.main = 1.8, cex.sub = 1.2)
lines(resumen_subsetsfs$bic, col = "red", lwd = 1)
grid(col = "lightgray", lty = "dotted")
dev.off() 



#Garfica de AIC con respecto al número de predictores
png("C:/Users/Anselmo Daniel/Documents/Cómputo Estádistico/Proyecto Armando Anselmo/graf3pAA.png", width=900, height=600)
plot(resumen_subsetsfs$cp,
     type = "p", pch = 16, col = rgb(0, 114/255, 178/255, 0.7),  
     xlab = "Número de predictores", 
     ylab = "AIC",
     main = "Método Forward. Relación entre el número de predictores y AIC",
     cex = 1.8, cex.lab = 1.5, cex.main = 1.8, cex.sub = 1.2)
lines(resumen_subsetsfs$cp, col = "red", lwd = 1)
grid(col = "lightgray", lty = "dotted")
dev.off()


ajuste_modeloback <- regsubsets(x = X[train, ], y = Y[train], datos = data_new, method =  "backward", nvmax = 68)

resumen_subsetsback <- summary(ajuste_modeloback)

# Encontramos el mejor modelo según R² ajustado, AIC y BIC
mejor_r2b <- which.max(resumen_subsetsback$adjr2)
mejor_bicb <- which.min(resumen_subsetsback$bic)
mejor_cpb <- which.min(resumen_subsetsback$cp)


# Variables seleccionadas en el modelo con mejor R² ajustado
variables_mejor_r2b <- coef(ajuste_modelofs, mejor_r2b)
print("Variables seleccionadas según mejor R² ajustado:")
print(names(variables_mejor_r2b))

# Variables seleccionadas en el modelo con mejor BIC
variables_mejor_bicb <- coef(ajuste_modelofs, mejor_bicb)
print("Variables seleccionadas según mejor BIC:")
print(names(variables_mejor_bicb))

# Variables seleccionadas en el modelo con mejor Cp
variables_mejor_cpb <- coef(ajuste_modelofs, mejor_cpb)
print("Variables seleccionadas según mejor Cp:")
print(names(variables_mejor_cpb))






cat("El mejor modelo según el R² ajustado tiene", mejor_r2b, "predictores.\n")
cat("El mejor modelo según el BIC tiene", mejor_bicb, "predictores.\n")
cat("El mejor modelo según el AIC tiene", mejor_cpb, "predictores.\n")
#print(resumen_subsets$which)


#Garfica de R^2 con respecto al número de predictores
png("C:/Users/Anselmo Daniel/Documents/Cómputo Estádistico/Proyecto Armando Anselmo/graf4pAA.png", width=900, height=600)
plot(resumen_subsetsback$adjr2,
     type = "p", pch = 16, col = rgb(0, 114/255, 178/255, 0.7),  
     xlab = "Número de predictores", 
     ylab = expression(R^2),
     main = expression(bold("Método Backward.Relación entre el número de predictores y"~R^2)),
     cex = 1.8, cex.lab = 1.5, cex.main = 1.8, cex.sub = 1.2)
lines(resumen_subsetsback$adjr2, col = "red", lwd = 1)
grid(col = "lightgray", lty = "dotted")
dev.off() 


#Garfica de BIC con respecto al número de predictores
png("C:/Users/Anselmo Daniel/Documents/Cómputo Estádistico/Proyecto Armando Anselmo/graf5pAA.png", width=900, height=600)
plot(resumen_subsetsback$bic,
     type = "p", pch = 16, col = rgb(0, 114/255, 178/255, 0.7),  
     xlab = "Número de predictores", 
     ylab = "BIC",
     main = "Método Backward. Relación entre el número de predictores y BIC",
     cex = 1.8, cex.lab = 1.5, cex.main = 1.8, cex.sub = 1.2)
lines(resumen_subsetsback$bic, col = "red", lwd = 1)
grid(col = "lightgray", lty = "dotted")
dev.off() 



#Garfica de AIC con respecto al número de predictores
png("C:/Users/Anselmo Daniel/Documents/Cómputo Estádistico/Proyecto Armando Anselmo/graf6pAA.png", width=900, height=600)
plot(resumen_subsetsback$cp,
     type = "p", pch = 16, col = rgb(0, 114/255, 178/255, 0.7),  
     xlab = "Número de predictores", 
     ylab = "AIC",
     main = "Método Backward. Relación entre el número de predictores y AIC",
     cex = 1.8, cex.lab = 1.5, cex.main = 1.8, cex.sub = 1.2)
lines(resumen_subsetsback$cp, col = "red", lwd = 1)
grid(col = "lightgray", lty = "dotted")
dev.off() 

# procedemos a hacer regresión lasso
grid<-10^seq(10,-2,length =100) # ajustamos un umbral para el valor de lambda


mod_lasso <- glmnet(X[train, ], Y[train], alpha = 1 , lambda =grid)
cv.out<-cv.glmnet(X[train, ], Y[train], alpha = 1 ,lambda =grid)
#cv.out<-cv.glmnet(x[train,],y[train],alpha =0,lambda =grid) 
png("C:/Users/Anselmo Daniel/Documents/Cómputo Estádistico/Proyecto Armando Anselmo/graf7pAA.png", width=900, height=600)
plot(cv.out) #grafica MSE de prueba para cada valor de lamda
dev.off()
# Mostrar coeficientes del modelo Lasso
#print(coef(mod_lasso))
cv.error1<- rep (0,100)
for (i in 1:100){


#se predicen los valores del conjunto de prueba considerando el modelo ajustado con cada
#valor de lamda, usando los valores x del conjunto de prueba
  lasso.pred<-predict(mod_lasso,s=grid[i],newx=X[test,]) 

#se calcula el error de prueba MSE para cada valor de lamda
  cv.error1[i]<-mean((lasso.pred-y.test)^2) 

}

minMSE1<-min(cv.error1) #valor minimo del error de prueba
minMSE1
lamda.opt<-grid[which.min(cv.error1)] # valor del lamda optimo (con el que se obtuvo el 
#valor minimo del error )
print(lamda.opt)

mod_lasso1 <- glmnet(X, Y, alpha = 1 , lambda = lamda.opt)
#plot(grid,cv.error1)
png("C:/Users/Anselmo Daniel/Documents/Cómputo Estádistico/Proyecto Armando Anselmo/graf8pAA.png", width=900, height=600)
plot(mod_lasso)
dev.off()

png("C:/Users/Anselmo Daniel/Documents/Cómputo Estádistico/Proyecto Armando Anselmo/graf9pAA.png", width=900, height=600)
plot(mod_lasso, xvar = "lambda", label = TRUE) #####################
dev.off()

print(coef(mod_lasso1))


#################################################################
# procedemos a hacer regresión Ridge
grid<-10^seq(10,-2,length =100) # ajustamos un umbral para el valor de lambda

mod_ridge <- glmnet(X[train, ], Y[train], alpha = 0 , lambda =grid)
cv.out<-cv.glmnet(X[train, ], Y[train], alpha = 0 ,lambda =grid)
#cv.out<-cv.glmnet(x[train,],y[train],alpha =0,lambda =grid) 
#png("C:/Users/Anselmo Daniel/Documents/Cómputo Estádistico/Proyecto Armando Anselmo/graf7pAA.png", width=900, height=600)

png("C:/Users/Anselmo Daniel/Documents/Cómputo Estádistico/Proyecto Armando Anselmo/graf10pAA.png", width=900, height=600)
plot(cv.out) #grafica MSE de prueba para cada valor de lamda
dev.off()

# Mostrar coeficientes del modelo Lasso
#print(coef(mod_lasso))
cv.error2<- rep (0,100)
for (i in 1:100){
  #se predicen los valores del conjunto de prueba considerando el modelo ajustado con cada
  #valor de lamda, usando los valores x del conjunto de prueba
  ridge.pred <- predict(mod_ridge,s=grid[i],newx=X[test,]) 
  
  #se calcula el error de prueba MSE para cada valor de lamda
  cv.error2[i]<-mean((ridge.pred-y.test)^2) 
}

minMSE2<-min(cv.error2) #valor minimo del error de prueba
minMSE2
lamda.opt2<-grid[which.min(cv.error2)] # valor del lamda optimo (con el que se obtuvo el 
#valor minimo del error )
print("El lambda óptimo para ridgge es:\n")
print(lamda.opt2)

mod_ridge1 <- glmnet(X, Y, alpha = 0 , lambda = lamda.opt2)
#plot(grid,cv.error1)

png("C:/Users/Anselmo Daniel/Documents/Cómputo Estádistico/Proyecto Armando Anselmo/graf11pAA.png", width=900, height=600)
plot(mod_ridge, xvar = "lambda", label = TRUE,) ############
dev.off()

print(coef(mod_ridge1))
