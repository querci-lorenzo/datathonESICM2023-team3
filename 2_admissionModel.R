# Admission model

library(ggplot2)

length(admission$admissionyeargroup[admission$admissionyeargroup %in% "2003-2009"]) # 9870
length(admission$admissionyeargroup[admission$admissionyeargroup %in% "2010-2016"]) # 13236
#34 bed capacity (???32-44???) -->
(9870 + 13236)/14 #1650 pazienti anno
1650/34 #48.52 pazienti/letto/anno
1650/44 #37.5 pazienti/letto/anno
1650/365 #4.5 pazienti al giorno
24/4.5 #1 paziente/5.33h
1/5.33 # 0.19 pazienti/ora

ggplot(admission) + geom_bar(aes(x = urgency, fill = urgency%in%0)) + ggpubr::theme_pubr()

table(admission$urgency)[1]/table(admission$urgency)[2]
#ratio elezione:urgenze = 2.70
table(admission$urgency)[1]/(table(admission$urgency)[1] + table(admission$urgency)[2])
# elezione 0.73

arrivingProb = c(0.73, 1-0.73)*0.19*1.25

dE<-rpois(2400, lambda = arrivingProb[1])
dU<-rpois(2400, lambda = arrivingProb[2])

dEDATA = as.data.frame(dE)
ggplot(dEDATA) + geom_density(aes(x = dE)) + ggpubr::theme_pubr()
dUDATA = as.data.frame(dU)
ggplot(dUDATA) + geom_density(aes(x = dU)) + ggpubr::theme_pubr()

dEd<-matrix(dE, ncol = 24)
dUd<-matrix(dU, ncol = 24)

dEd<-apply(dEd, 1, sum)
dUd<-apply(dUd, 1, sum)

dEd<-data.frame(dEd, rep("Election", length(dEd)))
names(dEd)<-c("Daily Admission rate", "Urgency")
dUd<-data.frame(dUd, rep("Urgency", length(dUd)))
names(dUd)<-c("Daily Admission rate", "Urgency")
dd<-rbind(dEd, dUd)
dd$"Daily Admission rate"<- factor(dd$"Daily Admission rate")
dd$Urgency <-factor(dd$Urgency)
ggplot(dd) +
geom_bar(aes(x = dd$"Daily Admission rate", fill = Urgency)) + 
xlab("Daily Admission rate") +
ylab("Probability (%)") +
ggpubr::theme_pubr()

# Add complexity
#   Variazioni ingressi rispetto alle ore del giorno (fasce 6-12, 12-18, 18-24, 24-6)
#   Variazioni rispetto agli interventi chirurgici in elezione rispetto ai giorni della settimana (lun-ven Vs sab-dom)

arrivingProb = c(0.73, 1-0.73)*0.19*1.25 #-- 0.1735 / 0.0641

# Inizio simulazione ore 6 di monday
arrivingProbMatrix<-matrix(rep(arrivingProb, 4), ncol = 2, byrow = T)
coefE = c(0.5, 2.4 , 1, 0.1)
coefU = c(1, 1, 1.1, 0.9)
arrivingProbMatrix[,1]<-arrivingProbMatrix[,1]*coefE
arrivingProbMatrix[,2]<-arrivingProbMatrix[,2]*coefU

hour<-rep(c(rep(1,6), rep(2, 6), rep(3, 6), rep(4, 6)), 100)
dET<-c(rep(NA, 2400))
dUT<-c(rep(NA, 2400))
for(i in 1:2400){
    dET[i]<-rpois(1, lambda = arrivingProbMatrix[hour[i], 1])
    dUT[i]<-rpois(1, lambda = arrivingProbMatrix[hour[i], 2])
}

dETh<-matrix(dET, ncol = 6)
dUTh<-matrix(dUT, ncol = 6)

dETh<-apply(dETh, 1, sum)
dUTh<-apply(dUTh, 1, sum)

dETh<-matrix(dETh, ncol = 4)
dUTh<-matrix(dUTh, ncol = 4)

dETh<-data.frame(dETh, rep("Election", length(dETh)))
names(dETh)<-c("Admission rate 0-6", "Admission rate 6-12", "Admission rate 12-18", "Admission rate 18-24", "Urgency")
dUTh<-data.frame(dUTh, rep("Urgency", length(dUTh)))
names(dUTh)<-c("Admission rate 0-6", "Admission rate 6-12", "Admission rate 12-18", "Admission rate 18-24", "Urgency")
dd<-as.data.frame(rbind(dETh, dUTh))

ggplot(as.data.frame(dd)) +
geom_bar(aes(x = dd$"Admission rate 0-6", fill = Urgency)) + 
xlab("Daily Admission rate") +
ylab("Probability (%)") +
ggpubr::theme_pubr()

ddLong <- tidyr::gather(dd, Time, value, "Admission rate 0-6":"Admission rate 18-24", factor_key=TRUE)

ggplot(ddLong, aes(x=value,fill=Time))+
geom_bar(stat = "count",position = position_dodge(0.9))+
xlab("Daily Admission rate") +
ylab("Probability (%)") +
scale_y_continuous(name = "Admission Counts",
                     limit = c(0,500),
                     breaks = seq(0,1000,100), 
                     sec.axis = sec_axis(~ (./24), 
                                         breaks = seq(0,100,5),
                                         name = "Probability (%)")) +
ggpubr::theme_pubr() +
scale_fill_grey(start = .9, end = 0)

#Tengo conto di weekdays Vs weekend days

arrivingProb = c(0.73, 1-0.73)*0.19*1.25 #-- 0.1735 / 0.0641

# Inizio simulazione ore 6 di monday
arrivingProbMatrix<-matrix(rep(arrivingProb, 4), ncol = 2, byrow = T)
coefE = c(0.5, 2.4 , 1, 0.1)
coefU = c(1, 1, 1.1, 0.9)
arrivingProbMatrix[,1]<-arrivingProbMatrix[,1]*coefE
arrivingProbMatrix[,2]<-arrivingProbMatrix[,2]*coefU

arrivingProbArray <- array(arrivingProbMatrix, c(4, 2, 7))
coefE = c(rep(1.36, 5), rep(0.1, 2))
coefU = c(rep(1, 5), rep(1, 2))
arrivingProbArray[, 1,]<-arrivingProbArray[, 1,]*matrix(rep(coefE, 4), ncol = 7, byrow = T)
arrivingProbArray[, 2, ]<-arrivingProbArray[, 2, ]*matrix(rep(coefU, 4), ncol = 7, byrow = T)

hour<-rep(c(rep(1,6), rep(2, 6), rep(3, 6), rep(4, 6)), 100)
day<-rep(c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24), rep(6, 24), rep(7,24)), 15)
dET<-c(rep(NA, 2400))
dUT<-c(rep(NA, 2400))
for(i in 1:2400){
    dET[i]<-rpois(1, lambda = arrivingProbArray[hour[i], 1, day[i]])
    dUT[i]<-rpois(1, lambda = arrivingProbArray[hour[i], 2, day[i]])
}

#Function admission rate to simulation

dETS<-ifelse(dET>0, seq_along(dET), NA)
dETS<-data.frame(dETS, dET)
dETS<-dETS[complete.cases(dETS), ]
dim(dETS)
names(dETS)<-c("hour", "patient")
dUTS<-ifelse(dUT>0, seq_along(dUT), NA)
dUTS<-data.frame(dUTS, dUT)
dUTS<-dUTS[complete.cases(dUTS), ]
dim(dUTS)
names(dUTS)<-c("hour", "patient")
admissionRate<-merge(dUTS, dETS, by = "hour", all.x = TRUE, all.y = TRUE)

#Stress test --> progetta uno stess test sulla base del covid (dati CDC o vedi se trovi qualcos'altro)
#https://www.ecdc.europa.eu/en/publications-data/download-data-hospital-and-icu-admission-rates-and-current-occupancy-covid-19

