#Universidad del Valle de Guatemala
#Jorge Suchite 15293
#laboratorio 2 de Data Science (algo sad)

library(foreign)
library(readr)
library(class)
library(caret)


################################################################################
############################### leyendo las cosas para poder utilizarlas########

precio<- read_csv("C:/Users/jorge/Desktop/goku/Lab2/Data/sample_submission.csv")
View(sample_submission)
 
test <- read_csv("C:/Users/jorge/Desktop/goku/Lab2/Data/test.csv")
View(test)

train <- read_csv("C:/Users/jorge/Desktop/goku/Lab2/Data/train.csv")
View(train)

###############################################################################

datause<- train[,(unlist(lapply(train, is.numeric)))]
datause$Id <- NULL #eliminamos el ID, que no representa una escala
View(datause)

################Particionando los datos en conjunto de entrenamiento y prueba###

set.seed(55)
porciento <- 60/100 #Porciento en el que se partira la poblacion
thanos<-sample(1:nrow(datause),porciento*nrow(datause))#hacemos la thanos para elegir a los datos

trainSet<-datause[thanos,] #Obtengo las filas de los elementos que estan en el sector de muestra
testestSet<-datause[-thanos,] #Obtengo las filas de los elementos que no est�n en el vector de muestra

 
View(trainSet)
View(testestSet)

###################################################

# ahora que la tengo separada tratare de trabajar en ella


############### Regresion Lineal#############

#Generaci�n del modelo

#############################################################
# Las predicciones se hicieron a 13 variables contra el precio estimado
# debido al codigo excesivo, solo se mostrara una ejemplificacion en uno de
# los casos. Todo lo demas, estara en el pdf 

modelineal<-lm(SalePrice~., data = trainSet)
summary(modelineal)

View(testestSet)

#Calculamos la prediccion
prediccion<-predict(modelineal,newdata = testestSet[1:36])
testestSet$PricePred<-prediccion
dif<-abs(testestSet$PricePred-testestSet$SalePrice)

testestSet$mpgPred <-NULL


