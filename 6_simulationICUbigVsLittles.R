# Pepara risultati per analisi TI grande Vs piccola

library(ggplot2)
library(ggpubr)
library(dplyr)

RRBIG<-list()
RRLIT<-list()
kk = 0
for(BBB in seq(2, 36, 2)){
    kk = kk + 1
    hoursSim = 24*54
    nBedsStartBig <- BBB*2
    nBedsStartLit <- BBB
    nsimulation = 100
    resultBig<-simulation(hoursSim, nBedsStart = nBedsStartBig, nsimulation, coef = 1)
    RRBIG[[kk]]<-analysis(result = resultBig, hoursSim, nBedsStart = nBedsStartBig, nsimulation)
    resultLit<-simulation(hoursSim, nBedsStart = nBedsStartLit, nsimulation, coef = 0.5)
    RRLIT[[kk]]<-analysis(result = resultLit, hoursSim, nBedsStart = nBedsStartLit, nsimulation)
}

RRBIGREJ<-c()
RRLITREJ<-c()
RRBIGREJsd<-c()
RRLITREJsd<-c()
for(i in 1:18){
RRBIGREJ<-c(RRBIGREJ, ifelse(length(RRBIG[[i]]$meanrejected) > 0, RRBIG[[i]]$meanrejected, 0))
RRLITREJ<-c(RRLITREJ, ifelse(length(RRLIT[[i]]$meanrejected) > 0, RRLIT[[i]]$meanrejected, 0))
RRBIGREJsd<-c(RRBIGREJsd, ifelse(length(RRBIG[[i]]$sdrejected) > 0, RRBIG[[i]]$sdrejected, 0))
RRLITREJsd<-c(RRLITREJsd, ifelse(length(RRLIT[[i]]$sdrejected) > 0, RRLIT[[i]]$sdrejected, 0))
}


RRUnion<-data.frame(seq(4, 72, 4), c(RRBIGREJ, RRLITREJ), c(RRBIGREJsd, RRLITREJsd), c(rep("BIG", 18), rep("LITTLE", 18)))
names(RRUnion)<-c("id", "Rejected/Bed/Day", "sd", "ICU type")

RRUnion<-RRUnion %>% arrange (id)

ggplot(RRUnion, aes(gruop = factor(RRUnion$"ICU type"))) +
geom_line(aes(id, RRUnion$"Rejected/Bed/Day", color = factor(RRUnion$"ICU type"))) +
#geom_ribbon(aes(x = id, ymin = RRUnion$"Rejected/Bed/Day"-(1.28*sd), ymax = RRUnion$"Rejected/Bed/Day"+(1.28*sd), fill = factor(RRUnion$"ICU type")) , alpha = 0.2) +
xlim(c(20, 72)) +
xlab("ICU total bed - x for 'BIG ICU' and x/2 for 'LITTLE ICU'") +
ylab("Number of ICU rejected patiets / ICU bed / Day") +
theme_pubr()


RRBIGREJ<-c()
RRLITREJ<-c()
RRBIGREJsd<-c()
RRLITREJsd<-c()
for(i in 1:18){
RRBIGREJ<-c(RRBIGREJ, ifelse(length(RRBIG[[i]]$hourbedfreeday) > 0, RRBIG[[i]]$hourbedfreeday, 0))
RRLITREJ<-c(RRLITREJ, ifelse(length(RRLIT[[i]]$hourbedfreeday) > 0, RRLIT[[i]]$hourbedfreeday, 0))
RRBIGREJsd<-c(RRBIGREJsd, ifelse(length(RRBIG[[i]]$hourbedfreedaysd) > 0, RRBIG[[i]]$hourbedfreedaysd, 0))
RRLITREJsd<-c(RRLITREJsd, ifelse(length(RRLIT[[i]]$hourbedfreedaysd) > 0, RRLIT[[i]]$hourbedfreedaysd, 0))
}

RRUnion<-data.frame(seq(4, 72, 4), c(RRBIGREJ, RRLITREJ), c(RRBIGREJsd, RRLITREJsd), c(rep("BIG", 18), rep("LITTLE", 18)))
names(RRUnion)<-c("id", "Hour/Day/Bed available", "sd","ICU type")

RRUnion<-RRUnion %>% arrange (id)

ggplot(RRUnion) +
geom_line(aes(id, RRUnion$"Hour/Day/Bed available", linetype = factor(RRUnion$"ICU type"))) +
#geom_ribbon(aes(x = id, ymin = RRUnion$"Hour/Day/Bed available"-(1.96*sd), ymax = RRUnion$"Hour/Day/Bed available"+(1.96*sd)) , alpha = 0.3) +
xlab("ICU total bed - x for 'BIG ICU' and x/2 for 'LITTLE ICU'") +
ylab("Hour / Day / ICU Bed available") +
theme_pubr()

ggplot(RRUnion, aes(gruop = factor(RRUnion$"ICU type"))) +
geom_line(aes(id, RRUnion$"Hour/Day/Bed available", color = factor(RRUnion$"ICU type"))) +
geom_ribbon(aes(x = id, ymin = RRUnion$"Hour/Day/Bed available"-(1.28*sd), ymax = RRUnion$"Hour/Day/Bed available"+(1.28*sd), fill = factor(RRUnion$"ICU type")) , alpha = 0.2) +
xlim(c(20, 72)) +
xlab("ICU total bed - x for 'BIG ICU' and x/2 for 'LITTLE ICU'") +
ylab("Hour / Day / ICU Bed available") +
theme_pubr()


RRBIG[[5]]$plot1
RRLIT[[5]]$plot1

RRBIG[[5]]$plot2
RRLIT[[5]]$plot2
