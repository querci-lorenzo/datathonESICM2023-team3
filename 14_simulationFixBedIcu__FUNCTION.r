#############################################################
##############  FUNCTION FIX BEDS SIMULATION  ###############
#################        VERSION 0.1        #################
#############################################################
#################      QUERCI LORENZO       #################
################# QUERCI.LORENZO@GMAIL.COM  #################
#############################################################

admission = data

#Funzione simulazione

simulation<-function(data = data,
                    month = 12, 
                    nBedsStart, 
                    nsimulation, 
                    election, 
                    cAR, 
                    coefHE, 
                    coefHU, 
                    coefDE, 
                    coefDU, 
                    coefME, 
                    coefMU, 
                    coef = 1, 
                    error = FALSE, 
                    error.perc = NA){
month = month + 1
hoursSim = month*28*24 #First 720-hours (30 days) will be deleted because ICU start empty
rejectedAdmission<-list()
hoursRejectedAdmission<-list()
timeToAdmissionRejected<-list()
admittedAdmission<-list()
bedMatrixList<-list()
for (s in 1:nsimulation){
  print(paste("Simulation n°:", s))
  admissionRate<-admssionRateFunction(month = month, election = election, cAR = cAR, coefHE = coefHE, coefHU = coefHU, coefDE = coefDE, coefDU = coefDU, coefME = coefME, coefMU = coefMU, coef = coef, error = error, error.perc = error.perc)
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
          #hourBeds[tp]<-admission$lengthofstay[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
          hourBeds[tp]<-data$lengthofstay[data$admissionid%in%pth[pt]]
          x = c(x, as.character((pth[pt])))
        }
      }else if(sum(nBeds)>0 & sum(nBeds)<length(pth)){
        for (pt in 1:sum(nBeds)){
          tp<-min(which(nBeds == 1))
          #print(paste("Admitted patient at", i, "hours at", tp, "bed. Urgency: ", data$urgency[data$admissionid %in% pth[pt]]))
          nBeds[tp] = 0
          #hourBeds[tp]<-admission$lengthofstay[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
          hourBeds[tp]<-data$lengthofstay[data$admissionid%in%pth[pt]]
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
    hoursRejectedAdmission[[s]]<-h[h>(28*24)]-(28*24)
    rejectedAdmission[[s]]<-j[!is.na(j) & h>(28*24)] 
    timeToAdmissionRejected[[s]]<-o[h>(28*24)]
    admittedAdmission[[s]]<-x[!is.na(x)] 
    bedMatrixList[[s]]<-bedMatrix[((28*24)+1):hoursSim,]
}

return(list(rejectedAdmission = rejectedAdmission, hoursRejectedAdmission = hoursRejectedAdmission, timeToAdmissionRejected = timeToAdmissionRejected, admittedAdmission = admittedAdmission, bedMatrixList = bedMatrixList))

}

#Funzione risultati analisi

analysis<-function(result, 
                    hoursSim = 12*28*24, 
                    nBedsStart, 
                    nsimulation){
    #Letti liberi (mean and IC95%) per giorni
    ff = function(x){sum(x)/nBedsStart} #Percentuale letti liberi per ciascuna ora
    mat<-as.data.frame(matrix(rep(NA, hoursSim*nsimulation), ncol = nsimulation))
    for (f in 1:length(result$bedMatrixList)) {mat[, f]<-apply(result$bedMatrixList[[f]], 1, FUN = ff)}
    ff25 = function(x){quantile(x, 0.25)}
    ff75 = function(x){quantile(x, 0.75)}
    mat$Hours<-c(1:hoursSim)
    plot1<-ggplot(mat, aes(x = Hours)) + 
    geom_line(aes(y = apply(mat[1:nsimulation], 1, mean)))+
    geom_ribbon(aes(
    ymax = ifelse(apply(mat[1:nsimulation], 1, mean)+1.96*apply(mat[1:nsimulation], 1, sd)>0.99, 1, apply(mat[1:nsimulation], 1, mean)+1.96*apply(mat[1:nsimulation], 1, sd)), 
    ymin = ifelse(apply(mat[1:nsimulation], 1, mean)-1.96*apply(mat[1:nsimulation], 1, sd)<0, 0, apply(mat[1:nsimulation], 1, mean)-1.96*apply(mat[1:nsimulation], 1, sd))), alpha = 0.3)+
    ylab("% Beds available") +
    ylim(c(0,1)) +
    xlab("Hours of simulation") +
    ggtitle("% Beds available/hours simulation")+
    labs(caption = paste0("Simulation based on ", nBedsStart, " beds-capacity and ", hoursSim, " hours of simulation. Simulation N = ", nsimulation)) +
    theme(plot.caption = element_text(hjust = 0))+
    theme_pubr()

    #Cost analysis --> free bed
    freeBed<-data.frame(apply(mat[, 1:nsimulation]*nBedsStart, 2, sum)) #Totale ore libere nella simulazione per ciascuna simulazione
    hourbedfree<-apply(freeBed/nBedsStart, 2, mean)
    hourbedfreesd<-apply(freeBed/nBedsStart, 2, sd)
    hourbedfreeday<-apply(freeBed/nBedsStart/(hoursSim/24), 2, mean)
    hourbedfreedaysd<-apply(freeBed/nBedsStart/(hoursSim/24), 2, sd)

    #Letti liberi (schema TI) nella simulazione
    ff2 = function(x){sum(x)/hoursSim} #Percentuale letti liberi per ciascuna ora
    mat2<-matrix(rep(NA, nBedsStart*nsimulation), ncol = nBedsStart)
    for (f in 1:length(result$bedMatrixList)){mat2[f, ]<-apply(result$bedMatrixList[[f]], 2, FUN = ff2)}
    freeBed<-data.frame(apply(mat2[, 1:nBedsStart], 2, median))
    freeBed<-freeBed %>% arrange(freeBed)
    freeBed$centro = c(1:nBedsStart)
    df <- data.frame(centro = c(1:nBedsStart), x = rep((nBedsStart/2):1, 2), y = c(rep(1,(nBedsStart/2)), rep(2,(nBedsStart/2))))
    df<-merge(df, freeBed, by = "centro")
    plot2<-ggplot(df, aes(x, y, fill = df$apply.mat2...1.nBedsStart...2..median.)) + 
    geom_tile(color = 'white', show.legend = FALSE, size = 2) + 
    geom_text(aes(label=round(df$apply.mat2...1.nBedsStart...2..median., digits = 2))) +
    coord_fixed(ratio = 1/3) +    # maintain a fixed aspect ratio so rectangles are wider than tall
    theme_void() +
    scale_fill_gradientn(limits = c(0,0.8), n.breaks =20, colours = gray.colors(20)) +
    theme(plot.background = element_rect(fill = "white", colour = "black", size = 1),
    plot.margin = margin(t = 0.5, r = 0.5,  b = 0.5, l = 0.5, unit = "cm"))

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
            hourbedfree = hourbedfree,
            hourbedfreesd = hourbedfreesd,
            hourbedfreeday = hourbedfreeday,
            hourbedfreedaysd = hourbedfreedaysd,
            plot2 = plot2,
            meanrejected = meanrejected/(hoursSim/24)/nBedsStart,
            sdrejected = sdrejected/(hoursSim/24)/nBedsStart,
            meanHourRejected = meanHourRejected,
            sdHourRejected = sdHourRejected
        )
    )
}