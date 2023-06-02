### Simulazione avanzata

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

hoursSim = 24*30
nBedsStart = 42
simulation = 4

#Stochastic approach, corrected only for urgency
simulationAdv<-function(hoursSim, nBedsStart, nBedsMin = round(nBedsStart* 0.875, digits = 0), nBedsMax = round(nBedsStart* 1.125, digits = 0), nsimulation, coef = 1, CO = 0.4){
hoursSim = hoursSim + 720 + 24 #First 720-hours will be deleted because ICU start with ll beds available
nBedsStart=nBedsMin
rejectedAdmission<-list()
hoursRejectedAdmission<-list()
timeToAdmissionRejected<-list()
admittedAdmission<-list()
bedMatrixList<-list()
for (s in 1:nsimulation){
  print(paste("Simulation n°:", s))
  admissionRate<-admssionRateFunction(hoursSim, coef)
  a = 1 #contatore pazienti che richiedono il posto in ICU rispetto alla matrice admissionRate
  j = c() #admissionid dei pazienti che vengono rigettati dalla ICU
  o = c() #hours to first free bad for rejected patients
  h = c() #orario (sequenziale) in cui i pazienti vengono rigettati
  x = c() #admissionid dei pazienti ammessi
  KC = NULL
  CAdmRate = c()
  CLosU = c()
  CLosE = c()
  CRatioU = c()
  nBeds = matrix(rep(c(rep(1, nBedsStart), rep(NA, nBedsMax-nBedsStart)), hoursSim), ncol = nBedsMax, byrow = T) #1 = available bed
  hourBeds = c(rep(0, nBedsStart), rep(NA, nBedsMax-nBedsStart)) #hours to left a bed free
  idBeds<-rep(NA, nBedsMax)
  hourClockBeds<-rep(NA, nBedsMax)
  bedMatrix<-matrix(ncol=nBedsMax, nrow = hoursSim)
  PP = c()
  for (i in 1:hoursSim){ # ore
    KC = 0
    hourBeds<-hourBeds-1
    hourBeds<-ifelse(hourBeds<0, 0, hourBeds)
    if(sum(is.na(nBeds[i, ]))==sum(is.na(nBeds[i-1, ]))){nBeds[i, ]<-ifelse(hourBeds==0, 1, nBeds[i-1, ])
    }else if(sum(is.na(nBeds[i-1, ]))>sum(is.na(nBeds[i, ]))){     ###Algoritmo di apertura posti letto
      TT = ifelse(hourBeds==0, 1, nBeds[i-1, !is.na(nBeds[i-1, ])])
      nBeds[i, !is.na(nBeds[i-1, ])]<- TT[!is.na(TT)]
      hourBeds = ifelse(!is.na(nBeds[i,])&hourBeds%in%NA, 0, hourBeds)
    }else if(sum(is.na(nBeds[i-1, ]))<sum(is.na(nBeds[i, ]))){     ###Algoritmo di chiusura posti letto
      if(sum(nBeds[i-1, which(nBeds[i, ]%in%NA)]%in%c(1, NA))==length(which(nBeds[i, ]%in%NA))){ # Queso funziona solo laTI era tutta aperta!! MODIFICA!!!
      nBeds[i, -which(nBeds[i, ]%in%NA)] = nBeds[i-1, -which(nBeds[i, ]%in%NA)]
      hourBeds = ifelse(is.na(nBeds[i,]), NA, hourBeds)
      }else{nBeds[i, ]<-ifelse(hourBeds==0, 1, nBeds[i-1, ])}
    } 
    idBeds<-ifelse(hourBeds==0, NA, idBeds)
    hourClockBeds<-ifelse(hourBeds==0, NA, hourClockBeds+1)
    if(i == admissionRate$hour[a]){
      KC = 1
      #print(paste("Admission request at", i, "hours"))
      #print(paste(sum(admissionRate$election[a]+admissionRate$urgency[a], na.rm = T), "patient request. Election:", admissionRate$election[a], ". Urgency:", admissionRate$urgency[a]))
      pth<-as.factor(c(sample(data$admissionid[data$urgency %in% 1], admissionRate[a, 3]), sample(data$admissionid[data$urgency %in% 0], admissionRate[a, 2]))) #Urgent patient admitted preferencialy respect to election
      jj = ifelse(length(pth)-sum(nBeds[i, ], na.rm = T)>0, length(pth)-sum(nBeds[i, ], na.rm = T), 0) #number of rejected patients at each time (internal variable)
      a = ifelse(a==length(admissionRate$hour), a, a+1)
      if(sum(nBeds[i, ], na.rm = T)>=length(pth)){
        for (pt in 1:length(pth)){
          tp<-min(which(nBeds[i, ] == 1))
          #print(paste("Admitted patient at", i, "hours at", tp, "bed. Urgency: ", data$urgency[data$admissionid %in% pth[pt]]))
          nBeds[i, tp] = 0
          hourBeds[tp]<-admission$lengthofstay[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
          idBeds[tp] = admission$admissionid[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
          hourClockBeds[tp] = 1
          x = c(x, as.character((pth[pt])))
        }
      }else if(sum(nBeds[i, ], na.rm = T)>0 & sum(nBeds[i, ], na.rm = T)<length(pth)){
        for (pt in 1:sum(nBeds[i, ], na.rm = T)){
          tp<-min(which(nBeds[i, ] == 1))
          #print(paste("Admitted patient at", i, "hours at", tp, "bed. Urgency: ", data$urgency[data$admissionid %in% pth[pt]]))
          nBeds[i, tp] = 0
          hourBeds[tp]<-admission$lengthofstay[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
          idBeds[tp] = admission$admissionid[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
          hourClockBeds[tp] = 1
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
      bedMatrix[i, ]<-nBeds[i, ]
      #Start IntellICU
      #Read
      CAdmRate<-c(CAdmRate, ifelse(KC == 1, length(pth), 0))
      if(KC==1){   
          CLosU<-c(CLosU, mean(admission$lengthofstay[admission$admissionid%in%pth & admission$urgency%in%1], na.rm = T))
          CLosE<-c(CLosE, mean(admission$lengthofstay[admission$admissionid%in%pth & admission$urgency%in%0], na.rm = T))
          CRatioU<-c(CRatioU, table(admission$urgency[admission$admissionid%in%pth])[2])
      }
      #Write
      if(i>=720 & i%%24==0){
        PROB = rep(NA, 100)
        for(Y in 1:100){
        SNPat = rpois(24*7, lambda = mean(CAdmRate))
        SNLos = rpois(sum(SNPat), lambda = (mean(CLosU, na.rm = T)*sum(CRatioU, na.rm = T)+mean(CLosE, na.rm = T)*(length(CAdmRate)-sum(CRatioU, na.rm = T)))/length(CAdmRate))
        #Future Matrix Prediction
        # Rimuovi il conteggio della matrice e aggiungi la somma dei giorni rimanenti
        # Verifica di dove vengono i LOS (devono essere letti)
        degAtt = sum(rpois(sum(!is.na(hourClockBeds)), lambda = (mean(CLosU, na.rm = T)*sum(CRatioU, na.rm = T)+mean(CLosE, na.rm = T)*(length(CAdmRate)-sum(CRatioU, na.rm = T)))/length(CAdmRate)))
        PROB[Y] = ifelse((degAtt+sum(SNLos)-(24*7*(sum(!is.na(nBeds[i, ]))))-sum(SNLos[(length(SNLos)-9):length(SNLos)]))>0, 1, 0) #1 TI piena a 7 giorni
        }
        #Result
        #print(paste("Overbooking probability at 7 days: ", mean(PROB), ". Hour ", i))
        PP=c(PP, mean(PROB))
        if(mean(PROB)>CO & i<(hoursSim-7*24)){
          if(sum(nBeds[i+(7*24-1),], na.rm = T)>nBedsStart){
            nBeds[(i+(7*24)):(i+(8*24-1)),]<-rep(1, length(nBeds[i+(24*7),]))
          }else if(sum(nBeds[i+(7*24-1),], na.rm = T)==nBedsStart){
            nBeds[(i+(7*24)):(i+(8*24-1)), (1:(nBedsStart+((dim(nBeds)[2]-nBedsStart)/2)))]<-rep(1)
          }else{print("ERROR: error in beds matrix. Line 170.")}
        }
      }
      #Finish IntellICU
    } 
    hoursRejectedAdmission[[s]]<-h[h>720+24]-744
    rejectedAdmission[[s]]<-j[!is.na(j) & h>744] 
    timeToAdmissionRejected[[s]]<-o[h>744]
    admittedAdmission[[s]]<-x[!is.na(x)] 
    bedMatrixList[[s]]<-bedMatrix[745:hoursSim,]
}

return(list(rejectedAdmission = rejectedAdmission, hoursRejectedAdmission = hoursRejectedAdmission, timeToAdmissionRejected = timeToAdmissionRejected, admittedAdmission = admittedAdmission, bedMatrixList = bedMatrixList))

}