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



#Calculamos la prediccion
prediccion<-predict(modelineal,newdata = testestSet[1:36])
testestSet$PricePred<-prediccion
dif<-abs(testestSet$PricePred-testestSet$SalePrice)
View(dif
     )

 

#########################################################

######### ac� viene la parte de decision del analista####

View(trainSet)
View(testestSet)


############################################################################
#### luego de obtener las correlaciones de nuestra data ahora obtengo
#### las columnas que tengan mayor significancia para ver que pasa


datito <- trainSet[,c( "SalePrice","YearRemodAdd","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF",
                      "LowQualFinSF","BsmtHalfBath","FullBath","HalfBath","GarageArea",         
                      "WoodDeckSF","OpenPorchSF","EnclosedPorch","3SsnPorch","ScreenPorch")]

testdatito <- testestSet[,c("SalePrice","YearRemodAdd","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF",
                            "LowQualFinSF","BsmtHalfBath","FullBath","HalfBath","GarageArea",         
                            "WoodDeckSF","OpenPorchSF","EnclosedPorch","3SsnPorch","ScreenPorch")]

###########################################################################
### ahora he de hacer la prediccion pero ahora con las que tienen mayor peso





modelodura<-lm(SalePrice~., data = datito)
summary(modelodura)

#Calculamos la prediccion
# se toma las columnas desde la columna 2 (dejando afuera el precio por el momento)
prediccion<-predict(modelodura,newdata = testdatito[2:ncol(testdatito)])
testdatito$PricePred<-prediccion
difdura<-abs(testdatito$PricePred-testdatito$SalePrice)

##################################################################################################
########################## KNN####################################################################




# nota: Las categoricas no se les puede hacer la knn por lo mismo, se tratar� de convertirla a numerica 

# 1 - 
# 2 - 
# 3 - 
# 4 - 
# 5 

# rango de valores



# valor mas grande 755000
# valor mas pequenio 3400

 



###################

### prediccion knn raiz de toda la data da 24.16 entonces se utiliza la k de 23 
# todos los NA en 0s

predKnn<-knn(datito[,c(2:15)], testdatito[,c(2:15)], as.factor(datito$Prec),k=23)
 

View(datito)
View(testdatito)
cfm<-confusionMatrix(as.factor(testdatito$Prec), predKnn)
cfm



#Con caret usando validacion cruzada
set.seed(55)
lrx8 <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 23)


knnTrain <- train(Prec~., data = testdatito, method = "knn",
                  trControl=lrx8,
                  preProcess = c("center", "scale"), tuneLength=10)

predknn<-predict(knnTrain,newdata = testdatito[,c(1:16)])
summary(knnTrain)
cfm<-confusionMatrix(as.factor(testdatito$Prec), predKnn)
cfm


 


