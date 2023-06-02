library(ggplot2)
library(ggpubr)
library(dplyr)

# Scelta dei coefficienti
arrivingProb = c(0.73, 1-0.73)*0.19*1.25 #-- 0.1735 / 0.0641
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

hoursSim = 24*30*2
nBedsStart = 36
nsimulation = 100

source(file.path(pathInputWin, "datathon2023/4_simulation.R"))
source(file.path(pathInputWin, "datathon2023/5_resultAnlysis.R"))

RR<-list()
for(k in 1:30){
    print(paste0("Simulazione K = ", k))
    nBedsStart <- 2*k
    result<-simulation(hoursSim, nBedsStart = nBedsStart, nsimulation, coef = 1)
    RR[[k]]<-analysis(result = result, hoursSim, nBedsStart = nBedsStart, nsimulation)
}

options(scipen = 999) 

RRREJ<-c()
RRREJsd<-c()
RRFREE<-c()
RRFREEsd<-c()
for(i in 1:30){
RRREJ<-c(RRREJ, ifelse(length(RR[[i]]$meanrejected) > 0, RR[[i]]$meanrejected, 0))
RRREJsd<-c(RRREJsd, ifelse(length(RR[[i]]$sdrejected) > 0, RR[[i]]$sdrejected, 0))
RRFREE<-c(RRFREE, ifelse(length(RR[[i]]$hourbedfreeday) > 0, RR[[i]]$hourbedfreeday, 0))
RRFREEsd<-c(RRFREEsd, ifelse(length(RR[[i]]$hourbedfreedaysd) > 0, RR[[i]]$hourbedfreedaysd, 0))
}

RRUnion<-data.frame(seq(2, 60, 2), RRREJ, RRREJsd, RRFREE, RRFREEsd)
names(RRUnion)<-c("id", "Rejected/Bed/Day", "sd Rejected", "Hour/Day/Bed available", "sd Free Bed")

TabExp = RRUnion
TabExp[,2:5]=TabExp[,2:5]*TabExp[,1]

TabExp$r = paste(round(TabExp$"Rejected/Bed/Day", digits = 4),round(TabExp$"sd Rejected", digits = 4),sep=' + ')
TabExp$x = paste(round(TabExp$"Hour/Day/Bed available", digits = 4),round(TabExp$"sd Free Bed", digits = 4),sep=' + ')

TabExp1 = cbind(TabExp$id, TabExp$r, TabExp$x)
TabExp1=as.data.frame(TabExp1)
names(TabExp1) = c("Bed Number", "Rejected patients/Bed/Day", "Hour available Bed/Day")


library(xtable)
xtable(TabExp1, digits = 4)

names(RRUnion)<-c("id", "Rejected/Bed/Day", "sd Rejected", "Hour/Day/Bed available", "sd Free Bed")

RR[[1]]$plot1
ggsave("plot1.jpeg")

#Analisi costi
#dailyCostBed = 1425 euro
#fixCost = 0.43* 1425
#saveLife = 103771*1.07 = saveLife
#table(is.na(admission$dateofdeath))[1]/table(is.na(admission$dateofdeath))[2]
# Mortality 0.11%
#table(admission$urgency)[1]/table(admission$urgency)[2]
# 0.73 elezione and 0.23 urgenze
# Prezzo per mancato acesso urgenze
# Prezzo per mancato accesso elezione = 1000

fixCost = 0.43* 1425
rejcost = 0.89*(0.23*103771+0.77*1000)

xtable(data.frame(fixCost, rejcost))

RRUnion[, 2:3] = RRUnion[, 2:3]*rejcost*RRUnion[,1]
RRUnion[, 4:5] = RRUnion[, 4:5]*RRUnion[,1]*fixCost/24 #Da costo giornaliero a costo orario

xtable(RRUnion)

analisiNum = RRUnion[,2:5]*RRUnion$id
analisiNum = analisiNum[,1]+analisiNum[,3]
which(analisiNum == min (analisiNum))*2

options(scipen = 999) 

plot = ggplot(RRUnion) +
geom_point(aes(x=42, y=9101.739), colour = "black", size = 5)+
geom_label(x=42, y = 15000, label = "N Beds = 42")+
#geom_vline(xintercept = 42, linetype = 2, colour = "red") +
geom_line(aes(id, id*RRUnion$"Rejected/Bed/Day"+id*RRUnion$"Hour/Day/Bed available"), colour = "black", linetype = 1, linewidth = 1) +
geom_line(aes(id, id*RRUnion$"Rejected/Bed/Day"), colour = "gray") +
geom_line(aes(id, id*RRUnion$"Hour/Day/Bed available"), colour = "black") +
geom_ribbon(aes(x = id, ymin = id*RRUnion$"Rejected/Bed/Day"-(id*1.28*RRUnion$"sd Rejected"), ymax = id*RRUnion$"Rejected/Bed/Day"+(id*1.28*RRUnion$"sd Rejected")), fill = "gray" , alpha = 0.2) +
geom_ribbon(aes(x = id, ymin = id*RRUnion$"Hour/Day/Bed available"-(id*1.28*RRUnion$"sd Free Bed"), ymax = id*RRUnion$"Hour/Day/Bed available"+(id*1.28*RRUnion$"sd Free Bed")), fill = "black" , alpha = 0.2) +
scale_y_continuous("Expense (euros) for Rejected Patients/Day (gray line)", sec.axis = sec_axis(~ ., name = "Expense (euros) for Free Beds/Day (black line)"))+
xlab("ICU total beds") +
theme_pubr()
ggsave("plot2.jpeg")


### Analisi costi modello finale a psoti fissi con nBeds = 42

hoursSim = 24*30*2
nBedsStart = 42
nsimulation = 250

result<-simulation(hoursSim, nBedsStart, nsimulation, coef = 1)
aa<-analysis(result = result, hoursSim, nBedsStart = nBedsStart, nsimulation)

aa$meanrejected*rejcost*42+aa$hourbedfreeday*(fixCost/24)*42
sqrt((aa$sdrejected*rejcost*42)^2+(aa$hourbedfreedaysd*(fixCost/24)*42)^2)


aa$plot1
ggsave("plot2.jpeg")