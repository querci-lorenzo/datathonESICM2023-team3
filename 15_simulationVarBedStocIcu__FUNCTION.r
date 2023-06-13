#############################################################
##############  FUNCTION VAR BEDS SIMULATION  ###############
#################        VERSION 0.1        #################
#############################################################
#################      QUERCI LORENZO       #################
################# QUERCI.LORENZO@GMAIL.COM  #################
#############################################################

admission = data

#Funzione simulazione

simulationAdv<-function(data,
                    month, 
                    nBedsStart, 
                    nBedsMin = round(nBedsStart* 0.875, digits = 0), 
                    nBedsMax = round(nBedsStart* 1.125, digits = 0), 
                    nsimulation, 
                    election, 
                    cAR, 
                    coefHE, 
                    coefHU, 
                    coefDE, 
                    coefDU, 
                    coefME, 
                    coefMU, 
                    coef, 
                    error, 
                    error.perc,
                    losprediction = "stochastic", # stochastic Vs predictive
                    CO){
month = month + 2
hoursSim = month*28*24 #First 60 days will be deleted because ICU start empty and because varbed alghoritm start after one week after the first month. We cut first to month to represent real function of alghortim
nBedsStart=nBedsMin
rejectedAdmission<-list()
hoursRejectedAdmission<-list()
timeToAdmissionRejected<-list()
admittedAdmission<-list()
bedMatrixList<-list()
prob<-list()
for (s in 1:nsimulation){
  print(paste("Simulation n°:", s))
  admissionRate<-admssionRateFunction(month = month, election = election, cAR = cAR, coefHE = coefHE, coefHU = coefHU, coefDE = coefDE, coefDU = coefDU, coefME = coefME, coefMU = coefMU, coef = coef, error = error, error.perc = error.perc)
  a = 1 #contatore pazienti che richiedono il posto in ICU rispetto alla matrice admissionRate
  j = c() #admissionid dei pazienti che vengono rigettati dalla ICU
  o = c() #hours to first free bad for rejected patients
  h = c() #orario (sequenziale) in cui i pazienti vengono rigettati
  x = c() #admissionid dei pazienti ammessi
  KC = NULL #control var. algh.
  CAdmRate = c() #write var. algh.
  CLosU = c() #write var. algh.
  CLosE = c() #write var. algh.
  CRatioU = c() #write var. algh.
  nBeds = matrix(rep(c(rep(1, nBedsStart), rep(NA, nBedsMax-nBedsStart)), hoursSim), ncol = nBedsMax, byrow = T) #1 = available bed
  hourBeds = c(rep(0, nBedsStart), rep(NA, nBedsMax-nBedsStart)) #hours to left a bed free
  idBeds<-rep(NA, nBedsMax) #raccoglie l'admission key dei pazienti ricoverati in quel momento (per il modello di predizione)
  hourClockBeds<-rep(NA, nBedsMax) #Conteggia i giorni di degenza attuali dei pazienti ricoverati
  bedMatrix<-matrix(ncol=nBedsMax, nrow = hoursSim) # Risultati
  PP = c() # Vettore che contiene le probabilitá di apertura
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
      if(sum(nBeds[i-1, which(nBeds[i, ]%in%NA)]%in%c(1, NA))==length(which(nBeds[i, ]%in%NA))){ # Queso funziona solo la TI era tutta aperta!! MODIFICA!!!
      nBeds[i, -which(nBeds[i, ]%in%NA)] = nBeds[i-1, -which(nBeds[i, ]%in%NA)]
      hourBeds = ifelse(is.na(nBeds[i,]), NA, hourBeds)
      }else{nBeds[i, ]<-ifelse(hourBeds==0, 1, nBeds[i-1, ])}
    } 
    idBeds<-ifelse(hourBeds==0, NA, idBeds)
    hourClockBeds<-ifelse(hourBeds==0, NA, hourClockBeds+1)
    if(i == admissionRate$hour[a]){
      KC = 1
      #print(paste("Admission request at", i, "hours")) ###DA DISATTIVARE
      #print(paste(sum(admissionRate$election[a]+admissionRate$urgency[a], na.rm = T), "patient request. Election:", admissionRate$election[a], ". Urgency:", admissionRate$urgency[a])) ###DA DISATTIVARE
      pth<-as.factor(c(sample(data$admissionid[data$urgency %in% 1], admissionRate[a, 3]), sample(data$admissionid[data$urgency %in% 0], admissionRate[a, 2]))) #Urgent patient admitted preferencialy respect to election
      jj = ifelse(length(pth)-sum(nBeds[i, ], na.rm = T)>0, length(pth)-sum(nBeds[i, ], na.rm = T), 0) #number of rejected patients at each time (internal variable)
      a = ifelse(a==length(admissionRate$hour), a, a+1)
      if(sum(nBeds[i, ], na.rm = T)>=length(pth)){
        for (pt in 1:length(pth)){
          tp<-min(which(nBeds[i, ] == 1))
          #print(paste("Admitted patient at", i, "hours at", tp, "bed. Urgency: ", data$urgency[data$admissionid %in% pth[pt]])) ###DA DISATTIVARE
          nBeds[i, tp] = 0
          hourBeds[tp]<-admission$lengthofstay[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
          idBeds[tp] = admission$admissionid[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
          hourClockBeds[tp] = 1
          x = c(x, as.character((pth[pt])))
        }
      }else if(sum(nBeds[i, ], na.rm = T)>0 & sum(nBeds[i, ], na.rm = T)<length(pth)){
        for (pt in 1:sum(nBeds[i, ], na.rm = T)){
          tp<-min(which(nBeds[i, ] == 1))
          #print(paste("Admitted patient at", i, "hours at", tp, "bed. Urgency: ", data$urgency[data$admissionid %in% pth[pt]])) ###DA DISATTIVARE
          nBeds[i, tp] = 0
          hourBeds[tp]<-admission$lengthofstay[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
          idBeds[tp] = admission$admissionid[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
          hourClockBeds[tp] = 1
          x = c(x, as.character((pth[pt])))
        }
        j = c(j, as.character(pth[(length(pth)-jj+1):length(pth)])) #admissionid dei pazienti che vengono rigettati dalla ICU
        h = c(h, rep(i, jj)) #orario (sequenziale) in cui i pazienti vengono rigettati
        o = c(o, ifelse(rep(length(sort(hourBeds, decreasing = TRUE))>= jj, jj), tail(sort(hourBeds, decreasing = TRUE), jj), c(rep(max(hourBeds), jj-length(sort(hourBeds, decreasing = TRUE))), tail(sort(hourBeds, decreasing = TRUE), jj))))
        #print(paste("Not-available bed for", jj, " patient."))  ###DA DISATTIVARE
      }else{
        j = c(j, as.character(pth[(length(pth)-jj+1):length(pth)])) #admissionid dei pazienti che vengono rigettati dalla ICU
        h = c(h, rep(i, jj)) #orario (sequenziale) in cui i pazienti vengono rigettati
        o = c(o, ifelse(rep(length(sort(hourBeds, decreasing = TRUE))>= jj, jj), tail(sort(hourBeds, decreasing = TRUE), jj), c(rep(max(hourBeds), jj-length(sort(hourBeds, decreasing = TRUE))), tail(sort(hourBeds, decreasing = TRUE), jj))))
        #print(paste("Not-available bed for", jj, " patient."))  ###DA DISATTIVARE
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
      if(i>=28*24 && i%%24==0){
        PROB = rep(NA, 100)
        for(Y in 1:100){
        #SNPat = rpois(24*7, lambda = mean(CAdmRate)) # Simulazione stocastoca ropis del numero di pazienti ammessi
        # Aggiornamento (ARIMA prediction)
        # CAdmRate_ts = ts(CAdmRate, start = 1, frequency = 1)
        # model_CAdmRate_ts = forecast::auto.arima(CAdmRate_ts,)
        # SNPat = forecast::forecast(model_CAdmRate_ts, 24*7)
        # Difficile lavorare con 
        # MASS::fitdistr(CAdmRate, "Poisson")
        # MASS::fitdistr(CAdmRate[(length(CAdmRate)-24*7):length(CAdmRate)], "Poisson")
        SNPat = rpois(24*7, lambda = (MASS::fitdistr(CAdmRate, "Poisson")$estimate + MASS::fitdistr(CAdmRate[(length(CAdmRate)-24*7):length(CAdmRate)], "Poisson")$estimate)/2)
        #Aggiungi tipo di predizione del los
        if(losprediction %in% "stochastic"){
        SNLos = rpois(sum(SNPat), lambda = (mean(CLosU, na.rm = T)*sum(CRatioU, na.rm = T)+mean(CLosE, na.rm = T)*(length(CAdmRate)-sum(CRatioU, na.rm = T)))/length(CAdmRate))
        }else if (losprediction %in% "predictive") {
           "In building"
        }else{print("Error in type of LOS prediction. Line 140")}
        #Future Matrix Prediction
        # Rimuovi il conteggio della matrice e aggiungi la somma dei giorni rimanenti
        # Verifica di dove vengono i LOS (devono essere letti)
        degAtt = sum(rpois(sum(!is.na(hourClockBeds)), lambda = (mean(CLosU, na.rm = T)*sum(CRatioU, na.rm = T)+mean(CLosE, na.rm = T)*(length(CAdmRate)-sum(CRatioU, na.rm = T)))/length(CAdmRate)))
        PROB[Y] = ifelse(
          degAtt+
          sum(SNLos)-
          (24*7*(sum(!is.na(nBeds[i, ]))))-
          sum(SNLos[(length(SNLos)-sum(SNPat[(length(SNPat)-24):length(SNPat)])):length(SNLos)])+
          ifelse(max(h, na.rm = T)>(i-24*7), 72, 0)+
          ifelse(sum(ifelse(PP[((i/24)-3):((i/24)-1)]>CO, 100, 0))%in%NA, 0, sum(ifelse(PP[((i/24)-3):((i/24)-1)]>CO, 100, 0)))
          >0, 1, 0) #1 TI piena a 7 giorni
        }
        #Result
        #print(paste("Overbooking probability at 7 days: ", mean(PROB), ". Hour ", i))  ###DA DISATTIVARE
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
    hoursRejectedAdmission[[s]]<-h[h>(28*2*24)]-(28*2*24)
    rejectedAdmission[[s]]<-j[!is.na(j) & h>(28*2*24)] 
    timeToAdmissionRejected[[s]]<-o[h>(28*2*24)]
    admittedAdmission[[s]]<-x[!is.na(x)] 
    bedMatrixList[[s]]<-bedMatrix[((28*2*24)+1):hoursSim,]
    prob[[s]] <- PP
}

return(list(rejectedAdmission = rejectedAdmission, hoursRejectedAdmission = hoursRejectedAdmission, timeToAdmissionRejected = timeToAdmissionRejected, admittedAdmission = admittedAdmission, bedMatrixList = bedMatrixList, prob = prob))

}

# Funzione analisi

analysisAdv<-function(result, 
                      hoursSim = 12*28*24, 
                      nBedsStart, 
                      nsimulation){
    #Letti liberi (mean and IC95%) per giorni
    ff = function(x){sum(x, na.rm = T)} #Percentuale letti liberi per ciascuna ora
    mat<-as.data.frame(matrix(rep(NA, hoursSim*nsimulation), ncol = nsimulation))
    for (f in 1:length(result$bedMatrixList)) {mat[, f]<-apply(result$bedMatrixList[[f]], 1, FUN = ff)}
    ff25 = function(x){quantile(x, 0.25, na.rm = T)}
    ff75 = function(x){quantile(x, 0.75, na.rm = T)}
    mat$Hours<-c(1:hoursSim)
    plot1<-ggplot(mat, aes(x = Hours)) + 
    geom_line(aes(y = apply(mat[1:nsimulation], 1, mean)))+
    geom_ribbon(aes(
    ymax = apply(mat[1:nsimulation], 1, mean)+1.96*apply(mat[1:nsimulation], 1, sd), 
    ymin = ifelse(apply(mat[1:nsimulation], 1, mean)-1.96*apply(mat[1:nsimulation], 1, sd)<0, 0, apply(mat[1:nsimulation], 1, mean)-1.96*apply(mat[1:nsimulation], 1, sd))), alpha = 0.3)+
    ylab("Total Hours Beds availables") +
    xlab("Hours of simulation") +
    ggtitle("% Beds available/hours simulation")+
    labs(caption = paste0("Simulation based on ", nBedsMin, " - ", nBedsMax, " beds-capacity and ", hoursSim, " hours of simulation. Simulation N = ", nsimulation)) +
    theme(plot.caption = element_text(hjust = 0))+
    theme_pubr()

    #Cost analysis
    freeBed<-data.frame(apply(mat[, 1:nsimulation], 2, sum)) #Totale ore libere nella simulazione per ciascuna simulazione
    hourbedfreeday<-apply(freeBed/(hoursSim/24), 2, mean)
    hourbedfreedaysd<-apply(freeBed/(hoursSim/24), 2, sd)

    #Analisi pazienti rejected
    meanrejected<-apply(as.data.frame(lapply(result$rejectedAdmission, length)), 1, mean)
    sdrejected<-apply(as.data.frame(lapply(result$rejectedAdmission, length)), 1, sd)

    #Analisi pazienti rejected hour
    meanHourRejected = mean(sapply(result$timeToAdmissionRejected, mean), na.rm = T)
    sdHourRejected = mean(sapply(result$timeToAdmissionRejected, sd), na.rm = T)
    
    #return
    return(
        list(
            plot1 = plot1,
            hourbedfreeday = hourbedfreeday,
            hourbedfreedaysd = hourbedfreedaysd,
            meanrejected = meanrejected/(hoursSim/24),
            sdrejected = sdrejected/(hoursSim/24),
            meanHourRejected = meanHourRejected,
            sdHourRejected = sdHourRejected
        )
    )
}