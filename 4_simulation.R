#Funzioni da richiamare
admssionRateFunction<-function(hoursSim, coef = 1){
  arrivingProbArray = arrivingProbArray*coef
  hour<-rep(c(rep(1,6), rep(2, 6), rep(3, 6), rep(4, 6)), 100)
  day<-rep(c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24), rep(6, 24), rep(7,24)), 15)
  dET<-c(rep(NA, hoursSim))
  dUT<-c(rep(NA, hoursSim))
  for(i in 1:hoursSim){
    dET[i]<-rpois(1, lambda = arrivingProbArray[hour[i], 1, day[i]])
    dUT[i]<-rpois(1, lambda = arrivingProbArray[hour[i], 2, day[i]])
  }
  dETS<-ifelse(dET>0, seq_along(dET), NA)
  dETS<-data.frame(dETS, dET)
  dETS<-dETS[complete.cases(dETS), ]
  names(dETS)<-c("hour", "patient")
  dUTS<-ifelse(dUT>0, seq_along(dUT), NA)
  dUTS<-data.frame(dUTS, dUT)
  dUTS<-dUTS[complete.cases(dUTS), ]
  names(dUTS)<-c("hour", "patient")
  admissionRate<-merge(dUTS, dETS, by = "hour", all.x = TRUE, all.y = TRUE)
  names(admissionRate)<-c("hour", "election", "urgency")
  admissionRate <- imputeTS::na_replace(admissionRate, 0)
  return(admissionRate)
}

#Simulation
#hoursSim = 24*30
#nBedsStart = 10
#nBedsMax = 30
#simulation = 10
#pazienti = hoursSim*sum(arrivingProb)*1.3
#sum(admissionRate$patient.y, na.rm = T)+sum(admissionRate$patient.x, na.rm = T)



#Stochastic approach, corrected only for urgency
simulation<-function(hoursSim, nBedsStart, nsimulation, coef = 1){
hoursSim = hoursSim + 720 #First 720-hours will be deleted because ICU start with ll beds available
rejectedAdmission<-list()
hoursRejectedAdmission<-list()
timeToAdmissionRejected<-list()
admittedAdmission<-list()
bedMatrixList<-list()
for (s in 1:nsimulation){
  print(paste("Simulation n°:", s))
  admissionRate<-admssionRateFunction(2400, coef)
  a = 1 #contatore pazienti che richiedono il posto in ICU rispetto alla matrice admissionRate
  j = c() #admissionid dei pazienti che vengono rigettati dalla ICU
  h = c() #orario (sequenziale) in cui i pazienti vengono rigettati
  o = c() #hours to first free bad for rejected patients
  x = c() #admissionid dei pazienti ammessi
  nBeds = rep(1, nBedsStart) #1 = available bed
  hourBeds = rep(0, nBedsStart) #hours to left a bed free
  bedMatrix<-matrix(ncol=nBedsStart, nrow = hoursSim)
  for (i in 1:hoursSim){ # ore
    hourBeds<-hourBeds-1
    hourBeds<-ifelse(hourBeds<0, 0, hourBeds)
    nBeds<-ifelse(hourBeds==0, 1, nBeds)
    if(i == admissionRate$hour[a]){
      #print(paste("Admission request at", i, "hours"))
      #print(paste(sum(admissionRate$election[a]+admissionRate$urgency[a], na.rm = T), "patient request. Election:", admissionRate$election[a], ". Urgency:", admissionRate$urgency[a]))
      pth<-as.factor(c(sample(data$admissionid[data$urgency %in% 1], admissionRate[a, 3]), sample(data$admissionid[data$urgency %in% 0], admissionRate[a, 2]))) #Urgent patient admitted preferencialy respect to election
      jj = ifelse(length(pth)-sum(nBeds)>0, length(pth)-sum(nBeds), 0) #number of rejected patients at each time (internal variable)
      a = ifelse(a==length(admissionRate$hour), a, a+1)
      if(sum(nBeds)>=length(pth)){
        for (pt in 1:length(pth)){
          tp<-min(which(nBeds == 1))
          #print(paste("Admitted patient at", i, "hours at", tp, "bed. Urgency: ", data$urgency[data$admissionid %in% pth[pt]]))
          nBeds[tp] = 0
          hourBeds[tp]<-admission$lengthofstay[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
          x = c(x, as.character((pth[pt])))
        }
      }else if(sum(nBeds)>0 & sum(nBeds)<length(pth)){
        for (pt in 1:sum(nBeds)){
          tp<-min(which(nBeds == 1))
          #print(paste("Admitted patient at", i, "hours at", tp, "bed. Urgency: ", data$urgency[data$admissionid %in% pth[pt]]))
          nBeds[tp] = 0
          hourBeds[tp]<-admission$lengthofstay[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
          x = c(x, as.character((pth[pt])))
        }
        j = c(j, as.character(pth[(length(pth)-jj+1):length(pth)])) #admissionid dei pazienti che vengono rigettati dalla ICU
        h = c(h, rep(i, jj)) #orario (sequenziale) in cui i pazienti vengono rigettati
        o = c(o, ifelse(rep(length(sort(hourBeds, decreasing = TRUE))>= jj, jj), tail(sort(hourBeds, decreasing = TRUE), jj), c(rep(max(hourBeds), jj-length(sort(hourBeds, decreasing = TRUE))), tail(sort(hourBeds, decreasing = TRUE), jj))))
        #print(paste("Not-available bed for", jj, " patient."))
      }else{
        j = c(j, as.character(pth[(length(pth)-jj+1):length(pth)])) #admissionid dei pazienti che vengono rigettati dalla ICU
        h = c(h, rep(i, jj)) #orario (sequenziale) in cui i pazienti vengono rigettati
        o = c(o, ifelse(rep(length(sort(hourBeds, decreasing = TRUE))>= jj, jj), tail(sort(hourBeds, decreasing = TRUE), jj), c(rep(max(hourBeds), jj-length(sort(hourBeds, decreasing = TRUE))), tail(sort(hourBeds, decreasing = TRUE), jj))))
        #print(paste("Not-available bed for", jj, " patient."))
        }
      }
      bedMatrix[i, ]<-nBeds
    } 
    hoursRejectedAdmission[[s]]<-h[h>720]-720
    rejectedAdmission[[s]]<-j[!is.na(j) & h>720] 
    timeToAdmissionRejected[[s]]<-o[h>720]
    admittedAdmission[[s]]<-x[!is.na(x)] 
    bedMatrixList[[s]]<-bedMatrix[721:hoursSim,]
}

return(list(rejectedAdmission = rejectedAdmission, hoursRejectedAdmission = hoursRejectedAdmission, timeToAdmissionRejected = timeToAdmissionRejected, admittedAdmission = admittedAdmission, bedMatrixList = bedMatrixList))

}