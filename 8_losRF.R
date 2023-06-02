library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(recipes)

### Split

indiciVal<-sample(1:length(data$lengthofstay), round(length(data$lengthofstay)/5, digits=0), replace = FALSE)
dataTrain<-data[!1:length(data$lengthofstay)%in%indiciVal, ]
dataTest<-data[indiciVal, ]
#dim(dataTest)
#dim(dataTrain)

rec <- 
  recipe(lengthofstay ~ ., data = dataTrain) %>% 
  step_other("Extremiteitentrauma (niet operatief)") %>% # This is where we handle new categories
  prep()

dataTrain <- bake(rec, new_data = dataTrain)
dataTest  <- bake(rec, new_data = dataTest)

dataTrainM = dataTrain[dataTrain$lengthofstay<quantile(dataTrain$lengthofstay, 0.950), ]
prune.control <- rpart.control(xmin = 15, cp = -1, minsplit=200, minbucket = 50)
model = rpart(dataTrainM$lengthofstay ~.,  data = dataTrainM[, 2:361], control = prune.control)
resultTrain <- predict(model, dataTrainM)
resultTest <- predict(model, dataTest)

#plot(model)
#table(prova)

cor(resultTrain, dataTrainM$lengthofstay)
cor(resultTest, dataTest$lengthofstay)

plot(pROC::roc(resultTrain, dataTrainM$lengthofstay))

cor(resultTrain, dataTrainM$lengthofstay)
cor(resultTest>8*24, dataTest$lengthofstay>8*24)

dfdf<-data.frame(resultTrain, dataTrainM$lengthofstay)
str(dfdf)
ggplot(dfdf) +
geom_point(aes(x = dfdf$resultTrain, y = dfdf$"dataTrainM.lengthofstay"))+
geom_abline(intercept = 0, slope = 1, size = 1, colour = "red")+
ggpubr::theme_pubr()
ggsave("prova.png")

MAE<- function(actual, predict){
    mean(abs(actual-predict))
}

MSE<- function(actual, predict){
    sum((actual-predict)^2)
}

RMSE<- function(actual, predict){
    sqrt(sum((actual-predict)^2))
}

MAE(dataTest$lengthofstay, resultTest)/mean(resultTest)
MAE(dataTest$lengthofstay, resultTest)
MSE(dataTest$lengthofstay, resultTest)
RMSE(dataTest$lengthofstay, resultTest)

#Modelli differenti per giorno

names(data[, 332:336])<-c("value.pao1", "value.pao2", "value.pao3", "value.pao4", "value.pao5")
names(data[grep("paco2", names(data))])<-c("value.paco1", "value.paco2", "value.paco3", "value.paco4", "value.paco5")


dim(subset(data, !grepl("2", names(data))))

table(!grepl("2", names(data)))

dim(data)