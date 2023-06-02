#LOS model

#Distribution of LOS - Density probability

hist(admission$lengthofstay[admission$lengthofstay<quantile(admission$lengthofstay, 0.99)], breaks = seq(min(admission$lengthofstay), quantile(admission$lengthofstay, 0.99), length.out = 100), prob = TRUE,
xlab = "Length of Stay (hours)", main = "Probability Distribution of LOS (quantile 0.00-0.99)")
lines(density(admission$lengthofstay[admission$lengthofstay<quantile(admission$lengthofstay, 0.99)]), lwd = 2, col = "black")
lines(dpois(x=0:1200, lambda=25), col="orange",lwd=4)

dispersion_test <- function(x) {
  res <- 1-2 * abs((1 - pchisq((sum((x - mean(x))^2)/mean(x)), length(x) - 1))-0.5)

  cat("Dispersion test of count data:\n",
      length(x), " data points.\n",
      "Mean: ",mean(x),"\n",
      "Variance: ",var(x),"\n",
      "Probability of being drawn from Poisson distribution: ", 
      round(res, 3),"\n", sep = "")

  invisible(res)
}
#This allows you to reject the null hypothesis that your data are Poisson distributed if the p value is <0.05. If the p value is above 0.05, you could accept that the data followed a Poisson distribution.
dispersion_test(admission$lengthofstay)

summary(vcd::goodfit(admission$lengthofstay, type="poisson"))
plot(vcd::goodfit(admission$lengthofstay, type="poisson"))

fitdistrplus::descdist(admission$lengthofstay, discrete = FALSE, boot = 1000)

MASS::fitdistr(admission$lengthofstay[admission$lengthofstay>0], "gamma")
ks.test (admission$lengthofstay, "pgamma", shape=6.620056e-01, rate=5.953276e-03)

# Non gamma
# Non weibull
# No poison

gammafit  <-  fitdistrplus::fitdist(admission$lengthofstay[admission$lengthofstay>0], "gamma")
weibullfit  <-  fitdistrplus::fitdist(admission$lengthofstay[admission$lengthofstay>0], "weibull")
lnormfit  <-  fitdistrplus::fitdist(admission$lengthofstay[admission$lengthofstay>0], "lnorm")  # not supported?
plot(gengammafit)
library(flexsurv) # on CRAN

gengammafit  <-  fitdistrplus::fitdist(admission$lengthofstay[admission$lengthofstay>0], "gengamma",
                                       start=function(d) list(mu=mean(d),
                                                              sigma=sd(d),
                                                              Q=0))

fitdistrplus::ppcomp(list(gammafit, lnormfit, weibullfit),
       legendtext=c("gamma", "lnorm", "weibull") )


shapiro.test(log(admission$lengthofstay))
qqnorm(log(admission$lengthofstay), ylim = c(-1,15))
qqline(log(admission$lengthofstay))
ks.test(log(admission$lengthofstay), 'pnorm')

#No logNorm

# Random forest

library(randomForest)
library(dplyr)

dataClean<-data[, names(data)[c(5, 10, 12, 13, 15, 17, 19:21)]]
dataClean<-dataClean[complete.cases(dataClean), ]
dataClean$id<-rep(1, length(dataClean$lengthofstay))
conteggio<- dataClean %>% group_by(diagnosis) %>% summarise(sum = sum(id)) %>% arrange(sum)
diagnosis53<-conteggio$diagnosis[(length(conteggio$diagnosis)-52):length(conteggio$diagnosis)]
dataClean<-dataClean[dataClean$diagnosis %in% diagnosis53, ]
dataClean<-dataClean[, 1:9]

dataClean$gender<-as.factor(dataClean$gender)
dataClean$agegroup<-as.factor(dataClean$agegroup)
dataClean$weightgroup<-as.factor(dataClean$weightgroup)
dataClean$heightgroup<-as.factor(dataClean$heightgroup)
dataClean$specialty<-as.factor(dataClean$specialty)
dataClean$readmission<-as.factor(dataClean$readmission)
dataClean$diagnosis<-factor(dataClean$diagnosis)
dataClean$urgency<-as.factor(dataClean$urgency)

indiciVal<-sample(1:length(dataClean$lengthofstay), round(length(dataClean$lengthofstay)/5, digits=0))
dataCleanTrain<-dataClean[!1:length(dataClean$lengthofstay)%in%indiciVal, ]
dataCleanTest<-dataClean[indiciVal, ]
#dim(dataCleanTest)
#dim(dataCleanTrain)

model <- randomForest(
  formula = lengthofstay ~ .,
  data = dataCleanTrain
)

#display fitted model
model
which.min(model$mse)
sqrt(model$mse[which.min(model$mse)]) 
plot(model)
varImpPlot(model) 

#Test
library(ggplot2)

LOSstim<-predict(model, dataCleanTest[, -2])
plotres<-data.frame(dataCleanTest$lengthofstay, LOSstim, dataCleanTest$urgency)
plotres$id<-c(1:length(plotres$dataCleanTest.lengthofstay))
ggplot2::ggplot(plotres)+
geom_point(aes(y=id, x=dataCleanTest.lengthofstay-LOSstim)) +
ggpubr::theme_pubr()

median(plotres$dataCleanTest.lengthofstay-plotres$LOSstim)
IQR(plotres$dataCleanTest.lengthofstay-plotres$LOSstim)

ggplot() +
#geom_density(aes(x = lengthofstay, linetype = urgency)) +
stat_ecdf(data = plotres, aes(x = dataCleanTest.lengthofstay/24, linetype = dataCleanTest.urgency), geom = "step") +
stat_ecdf(data = plotres, aes(x = LOSstim/24, linetype = dataCleanTest.urgency), colour = "red", geom = "step") +
xlim(0,30) +
ggpubr::theme_pubr()

#Poisson distribution
indiciVal<-sample(1:length(dataClean$lengthofstay), round(length(dataClean$lengthofstay)/5, digits=0))
dataCleanTrain<-dataClean[!1:length(dataClean$lengthofstay)%in%indiciVal, ]
dataCleanTest<-dataClean[indiciVal, ]

modelloPois<-glm(lengthofstay ~ gender + agegroup + weightgroup + heightgroup + specialty + readmission + diagnosis + urgency, data = dataCleanTest, family = "poisson")
#summary(modelloPois)

LOSstim<-predict(model, dataCleanTest[, -2])
plotres<-data.frame(dataCleanTest$lengthofstay, LOSstim, dataCleanTest$urgency)
plotres$id<-c(1:length(plotres$dataCleanTest.lengthofstay))
ggplot2::ggplot(plotres)+
geom_point(aes(y=id, x=dataCleanTest.lengthofstay-LOSstim)) +
ggpubr::theme_pubr()
median(plotres$dataCleanTest.lengthofstay-plotres$LOSstim)
IQR(plotres$dataCleanTest.lengthofstay-plotres$LOSstim)

ggplot() +
#geom_density(aes(x = lengthofstay, linetype = urgency)) +
stat_ecdf(data = plotres, aes(x = dataCleanTest.lengthofstay/24, linetype = dataCleanTest.urgency), geom = "step") +
stat_ecdf(data = plotres, aes(x = LOSstim/24, linetype = dataCleanTest.urgency), colour = "red", geom = "step") +
xlim(0,30) +
ggpubr::theme_pubr()

#Gamma distribution
indiciVal<-sample(1:length(dataClean$lengthofstay), round(length(dataClean$lengthofstay)/5, digits=0))
dataCleanTrain<-dataClean[!1:length(dataClean$lengthofstay)%in%indiciVal, ]
dataCleanTest<-dataClean[indiciVal, ]

dataCleanTrain$lengthofstay = dataCleanTrain$lengthofstay+0.1
dataCleanTest$lengthofstay = dataCleanTest$lengthofstay+0.1

modelloGam<-glm(lengthofstay ~ gender + agegroup + weightgroup + heightgroup + specialty + readmission + diagnosis + urgency, data = dataCleanTest, family = Gamma(link='log'))
#summary(modelloGam)

LOSstim<-predict(model, dataCleanTest[, -2])
plotres<-data.frame(dataCleanTest$lengthofstay, LOSstim, dataCleanTest$urgency)
plotres$id<-c(1:length(plotres$dataCleanTest.lengthofstay))
ggplot(plotres)+
geom_point(aes(y=id, x=dataCleanTest.lengthofstay-LOSstim)) +
ggpubr::theme_pubr()
median(plotres$dataCleanTest.lengthofstay-plotres$LOSstim)
IQR(plotres$dataCleanTest.lengthofstay-plotres$LOSstim)

ggplot() +
#geom_density(aes(x = lengthofstay, linetype = urgency)) +
stat_ecdf(data = plotres, aes(x = dataCleanTest.lengthofstay/24, linetype = dataCleanTest.urgency), geom = "step") +
stat_ecdf(data = plotres, aes(x = LOSstim/24, linetype = dataCleanTest.urgency), colour = "red", geom = "step") +
xlim(0,30) +
ggpubr::theme_pubr()

#Lognorm
hist(log(admission$lengthofstay[admission$lengthofstay<quantile(admission$lengthofstay, 0.99)]), breaks = seq(-1, 20, length.out = 100), prob = TRUE,
xlab = "LOG(Length of Stay (hours))", main = "Probability Distribution of LOS - LOG")

indiciVal<-sample(1:length(dataClean$lengthofstay), round(length(dataClean$lengthofstay)/5, digits=0))
dataCleanTrain<-dataClean[!1:length(dataClean$lengthofstay)%in%indiciVal, ]
dataCleanTest<-dataClean[indiciVal, ]

modelloNorm<-lm(log(lengthofstay) ~ gender + agegroup + weightgroup + heightgroup + specialty + readmission + diagnosis + urgency, data = dataCleanTest)
#summary(modelloNorm)

LOSstim<-predict(model, dataCleanTest[, -2])
plotres<-data.frame(dataCleanTest$lengthofstay, LOSstim, dataCleanTest$urgency)
plotres$id<-c(1:length(plotres$dataCleanTest.lengthofstay))
ggplot2::ggplot(plotres)+
geom_point(aes(y=id, x=dataCleanTest.lengthofstay-LOSstim)) +
ggpubr::theme_pubr()
median(plotres$dataCleanTest.lengthofstay-plotres$LOSstim)
IQR(plotres$dataCleanTest.lengthofstay-plotres$LOSstim)

ggplot() +
#geom_density(aes(x = lengthofstay, linetype = urgency)) +
stat_ecdf(data = plotres, aes(x = dataCleanTest.lengthofstay/24, linetype = dataCleanTest.urgency), geom = "step") +
stat_ecdf(data = plotres, aes(x = LOSstim/24, linetype = dataCleanTest.urgency), colour = "red", geom = "step") +
xlim(0,30) +
ggpubr::theme_pubr()

#dataClean$id<-rep(1, length(dataClean$lengthofstay))
#patologie<-dataClean %>% group_by(diagnosis) %>% summarise(cont = sum(id)) %>% arrange(cont)
#pat<-gridExtra::tableGrob(patologie)
#gridExtra::grid.arrange(pat)
#dev.off()


admission$urgency<-factor(admission$urgency)

ggplot(admission, (aes(x = lengthofstay/24, linetype = urgency))) +
#geom_density(aes(x = lengthofstay, linetype = urgency)) +
stat_ecdf(geom = "step") +
xlim(0,30) +
ggpubr::theme_pubr()

#Stochastic LOS approach

result<- list()
for(i in 1:1000){
  type = sample(levels(admission$urgency), 1, prob = prop.table(table(admission$urgency)))
  los = sample(admission$lengthofstay[admission$urgency%in%type], 1)
  result$type[i] = type
  result$los[i] = los
}

result<-as.data.frame(result)
result$type<-factor(result$type)

ggplot() +
#geom_density(aes(x = lengthofstay, linetype = urgency)) +
stat_ecdf(data = admission, aes(x = lengthofstay/24, linetype = urgency), geom = "step") +
stat_ecdf(data = result, aes(x = los/24, linetype = type), colour = "red", geom = "step") +
xlim(0,30) +
ggpubr::theme_pubr()