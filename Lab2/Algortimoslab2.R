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
testestSet<-datause[-thanos,] #Obtengo las filas de los elementos que no están en el vector de muestra

 


###################################################

# ahora que la tengo separada tratare de trabajar en ella


############### Regresion Lineal#############

#Generación del modelo

#############################################################
# Las predicciones se hicieron a 13 variables contra el precio estimado
# debido al codigo excesivo, solo se mostrara una ejemplificacion en uno de
# los casos. Todo lo demas, estara en el pdf 

modelineal<-lm(SalePrice~., data = trainSet)
summary(modelineal)



#Calculamos la prediccion
prediccion<-predict(modelineal,newdata = testestSet[1:36])
testestSet$PricePred<-prediccion
dif<-abs(testestSet$PricePred-testestSet$SalePrice)

testestSet$mpgPred <-NULL

#########################################################

######### acá viene la parte de decision del analista####

View(trainSet)
View(testestSet)


############################################################################
#### luego de obtener las correlaciones de nuestra data ahora obtengo
#### las columnas que tengan mayor significancia para ver que pasa


datito <- trainSet[,c("YearRemodAdd","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF",
                      "LowQualFinSF","BsmtHalfBath","FullBath","HalfBath","GarageArea",         
                      "WoodDeckSF","OpenPorchSF","EnclosedPorch","3SsnPorch","ScreenPorch", "SalePrice")]

testdatito <- testestSet[,c("YearRemodAdd","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF",
                            "LowQualFinSF","BsmtHalfBath","FullBath","HalfBath","GarageArea",         
                            "WoodDeckSF","OpenPorchSF","EnclosedPorch","3SsnPorch","ScreenPorch","SalePrice")]

###########################################################################
### ahora he de hacer la prediccion pero ahora con las que tienen mayor peso


modelodura<-lm(SalePrice~., data = datito)
summary(modelodura)

#Calculamos la prediccion
prediccion<-predict(modelodura,newdata = testdatito[2:ncol(testdatito)])
testdatito$PricePred<-prediccion
difI<-abs(testdatito$PricePred-testdatito$SalePrice)
 


