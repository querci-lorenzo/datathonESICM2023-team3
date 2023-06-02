k=0
RR<-list()
for(CO in seq(0,1,0.10)){
k = k+1
print(paste0("Simulazione K = ", CO))
result = simulationAdv(hoursSim = 30*24*2, nBedsStart = 42, nBedsMin = round(nBedsStart* 0.875, digits = 0), nBedsMax = round(nBedsStart* 1.125, digits = 0), nsimulation = 50, coef = 1, CO = CO)
RR[[k]] = analysisAdv(result = result, hoursSim = 30*24*2, nBedsStart, nsimulation = 50)
print(RR[[k]])$plot1
}

RRREJ<-c()
RRREJsd<-c()
RRFREE<-c()
RRFREEsd<-c()
for(i in 1:11){
RRREJ<-c(RRREJ, ifelse(length(RR[[i]]$meanrejected) > 0, RR[[i]]$meanrejected, 0))
RRREJsd<-c(RRREJsd, ifelse(length(RR[[i]]$sdrejected) > 0, RR[[i]]$sdrejected, 0))
RRFREE<-c(RRFREE, ifelse(length(RR[[i]]$hourbedfreeday) > 0, RR[[i]]$hourbedfreeday, 0))
RRFREEsd<-c(RRFREEsd, ifelse(length(RR[[i]]$hourbedfreedaysd) > 0, RR[[i]]$hourbedfreedaysd, 0))
}

RRUnion<-data.frame(seq(0,1,0.10), RRREJ, RRREJsd, RRFREE, RRFREEsd)
names(RRUnion)<-c("coef", "Rejected/Day", "sd Rejected", "Hour Free Beds/Day", "sd Free Bed")

fixCost = 0.43* 1425
rejcost = 0.89*(0.23*103771+0.77*1000)

RRUnion[, 2:3] = RRUnion[, 2:3]*rejcost
RRUnion[, 4:5] = RRUnion[, 4:5]*fixCost/24 #Da costo giornaliero a costo orario

analisiNum = RRUnion[,2:5]
analisiNum = analisiNum[,1]+analisiNum[,3]
seq(0,1,0.10)[which(analisiNum == min (analisiNum))]

plot = ggplot(RRUnion) +
geom_point(aes(x=0.0, y=10582.29), colour = "black", size = 5)+
geom_label(x=0.05, y = 12500, label = "Coef = 0.00")+
#geom_vline(xintercept = 42, linetype = 2, colour = "red") +
geom_line(aes(coef, RRUnion$"Rejected/Day"+RRUnion$"Hour Free Beds/Day"), colour = "black", linetype = 1, linewidth = 1) +
geom_line(aes(coef, RRUnion$"Rejected/Day"), colour = "gray") +
geom_line(aes(coef, RRUnion$"Hour Free Beds/Day"), colour = "black") +
geom_ribbon(aes(x = coef, ymin = RRUnion$"Rejected/Day"-(1.28*RRUnion$"sd Rejected"), ymax = RRUnion$"Rejected/Day"+(1.28*RRUnion$"sd Rejected")), fill = "gray" , alpha = 0.2) +
geom_ribbon(aes(x = coef, ymin = RRUnion$"Hour Free Beds/Day"-(1.28*RRUnion$"sd Free Bed"), ymax = RRUnion$"Hour Free Beds/Day"+(1.28*RRUnion$"sd Free Bed")), fill = "black" , alpha = 0.2) +
scale_y_continuous("Expense (euros) for Rejected Patients/Day (gray line)", sec.axis = sec_axis(~ ., name = "Expense (euros) for Free Beds/Day (black line)"))+
xlab("Coefficient IntellICU") +
theme_pubr()
ggsave("plot2.jpeg")

RR[[1]]$plot1
ggsave("plot3.jpeg")

xtable::xtable(RRUnion, digits = 4)


result = simulationAdv(hoursSim = 30*24*2, nBedsStart = 42, nBedsMin = round(nBedsStart* 0.875, digits = 0), nBedsMax = round(nBedsStart* 1.125, digits = 0), nsimulation = 150, coef = 1, CO = 0.0)
aa<-analysisAdv(result = result, hoursSim = 30*24*2, nBedsStart, nsimulation = 150)

aa$meanrejected*rejcost+aa$hourbedfreeday*(fixCost/24)
sqrt((aa$sdrejected*rejcost)^2+(aa$hourbedfreedaysd*(fixCost/24))^2)

aa$plot1
ggsave("plot2.jpeg")