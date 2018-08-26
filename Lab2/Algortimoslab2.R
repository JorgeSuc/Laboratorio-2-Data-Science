#Universidad del Valle de Guatemala
#Jorge Suchite 15293
#laboratorio 2 de Data Science

library(foreign)
library(readr)
library(class)
library(caret)




precio<- read_csv("C:/Users/jorge/Desktop/goku/Lab2/Data/sample_submission.csv")
View(sample_submission)
 
test <- read_csv("C:/Users/jorge/Desktop/goku/Lab2/Data/test.csv")
View(test)

train <- read_csv("C:/Users/jorge/Desktop/goku/Lab2/Data/train.csv")
View(train)




set.seed(1460)

#se parte la dataseed

porciento <- 60/100 #Porciento en el que se partirÃ¡n la data

################ siempre se tiene que encontrar las numericas y las no porque siempre da error
numericTrain <- train[,(unlist(lapply(train, is.numeric)))]
#se obtienen las   NO numericas
noNumericTrain <- train[,!(unlist(lapply(train, is.numeric)))]

muestra<-sample(1:nrow(noNumericTrain),porciento*nrow(numericTrain))#Muestra aleatoria de numeros de un vector

View(numericTrain)
View(noNumericTrain)

trainSet<-numericTrain[muestra,] #Obtengo las filas de los elementos que estan en el sector de muestra
testSet<-numericTrain[-muestra,] #Obtengo las filas de los elementos que no estÃ¡n en el vector de muestra

View(trainSet)
View(testSet)

###################################################

# ahora que la tengo separada tratare de trabajar en ella


############### Regresion Lineal#############

#Generación del modelo

#############################################################
# Las predicciones se hicieron a 13 variables contra el precio estimado
# debido al codigo excesivo, solo se mostrara una ejemplificacion en uno de
# los casos. Todo lo demas, estara en el pdf 

colnames(testSet)[14] <- "Unpiso"
colnames(testSet)[15] <- "dospiso"

modeloLinealSimple<-lm(SalePrice~PoolArea, data = testSet)
summary(modeloLinealSimple)

#predicción
prediccion<-predict(modeloLinealSimple,newdata = testSet[,2:ncol(testSet)])
 
#Ver la diferencia entre lo real y lo predicho
dif<-abs(prediccion-testSet$SalePrice)
porcentaje <- ((prediccion - testSet$SalePrice ) /(testSet$SalePrice)*100)
sinna <- na.omit(porcentaje)
mean(sinna)
View(testSet) 



############################################################################################
###################### Modelo KNN ##########################################################
# Las predicciones se hicieron a 13 variables contra el precio estimado
# debido al codigo excesivo, solo se mostrara una ejemplificacion en uno de
# los casos. Todo lo demas, estara en el pdf .
#Para predecir el precio
#Con class

library(class)
predKnn<-knn(trainSet[,c(1:876,10:11)],testSet[,c(1:584,10:11)],as.factor(trainSet$am),k=24)
cfm<-confusionMatrix(as.factor(testSet$SalePrice),predKnn)
cfm

View(trainSet)



 