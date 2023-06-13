#############################################################
#################  FUNCTION ADMISSION RATE  #################
#################        VERSION 0.1        #################
#############################################################
#################      QUERCI LORENZO       #################
################# QUERCI.LORENZO@GMAIL.COM  #################
#############################################################


admssionRateFunction<-function(month = 12, election = 0.73, cAR = 0.19, coefHE = coefHE, coefHU = coefHU, coefDE = coefDE, coefDU = coefDU, coefME = coefME, coefMU = coefMU, coef = 1, error = FALSE, error.perc = NA){
    arrivingProb = c(election, 1-election)*cAR*1.25*coef
    # Inizio simulazione ore 6 di monday 1 Jan
    arrivingProbMatrix<-matrix(rep(arrivingProb, 4), ncol = 2, byrow = T)
    arrivingProbMatrix[,1]<-arrivingProbMatrix[,1]*coefHE
    arrivingProbMatrix[,2]<-arrivingProbMatrix[,2]*coefHU

    arrivingProbList = list()
    for(i in 1:12){
        arrivingProbMatrixH = arrivingProbMatrix
        arrivingProbMatrixH[,1]<-arrivingProbMatrixH[,1]*coefME[i]
        arrivingProbMatrixH[,2]<-arrivingProbMatrixH[,2]*coefMU[i]
        arrivingProbList[[i]] =  arrivingProbMatrixH
    }
    
    arrivingProbArrayList = list()
    for(i in 1:12){
        arrivingProbArrayList[[i]] <- array(arrivingProbList[[i]], c(4, 2, 7))
        arrivingProbArrayList[[i]][, 1,]<-arrivingProbArrayList[[i]][, 1,]*matrix(rep(coefDE, 4), ncol = 7, byrow = T)
        arrivingProbArrayList[[i]][, 2, ]<-arrivingProbArrayList[[i]][, 2, ]*matrix(rep(coefDU, 4), ncol = 7, byrow = T)
    }
    
    #Month of 28 days
    hour<-rep(c(rep(1,6), rep(2, 6), rep(3, 6), rep(4, 6)), 28)
    day<-rep(c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24), rep(6, 24), rep(7,24)), 4)

    dET<-c(rep(NA, length(hour)))
    dUT<-c(rep(NA, length(hour)))
    
    if(error%in% TRUE && error.perc > 0.00 && error.perc < 0.25){
        admissionRateMonth = list()
        for(m in 1:month){
            dET = c()
            dUT = c()
            if(m <= 12){
                for(i in 1:(28*24)){
                    dET[i]<-rpois(1, lambda = rnorm(1, arrivingProbArrayList[[m]][hour[i], 1, day[i]], error.perc*arrivingProbArrayList[[m]][hour[i], 1, day[i]])) #rnorm add an error relative to lambda
                    dUT[i]<-rpois(1, lambda = rnorm(1, arrivingProbArrayList[[m]][hour[i], 2, day[i]], error.perc*arrivingProbArrayList[[m]][hour[i], 2, day[i]])) #rnorm add an error relative to lambda (same error % to urgency or election)
                }
                admissionRateMonth[[m]] = list(dET, dUT)
            }else if(m> 12 && m <= 24){
                for(i in 1:(28*24)){
                    dET[i]<-rpois(1, lambda = rnorm(1, arrivingProbArrayList[[m-12]][hour[i], 1, day[i]], error.perc*arrivingProbArrayList[[m-12]][hour[i], 1, day[i]])) #rnorm add an error relative to lambda
                    dUT[i]<-rpois(1, lambda = rnorm(1, arrivingProbArrayList[[m-12]][hour[i], 2, day[i]], error.perc*arrivingProbArrayList[[m-12]][hour[i], 2, day[i]])) #rnorm add an error relative to lambda (same error % to urgency or election)
                }
                admissionRateMonth[[m]] = list(dET, dUT)
            }else{print(paste0("Error. Month ", month, " are more than max. months allowed (24 months)."))}
        }
    }else if (error%in% FALSE && error.perc %in% NA) {
       admissionRateMonth = list()
        for(m in 1:month){
            dET = c()
            dUT = c()
            if(m <= 12){
                for(i in 1:(28*24)){
                    dET[i]<-rpois(1, lambda = arrivingProbArrayList[[m]][hour[i], 1, day[i]])
                    dUT[i]<-rpois(1, lambda = arrivingProbArrayList[[m]][hour[i], 2, day[i]])
                }
                admissionRateMonth[[m]] = list(dET, dUT)
            }else if(m> 12 && m <= 24){
                for(i in 1:(28*24)){
                    dET[i]<-rpois(1, lambda = arrivingProbArrayList[[m-12]][hour[i], 1, day[i]])
                    dUT[i]<-rpois(1, lambda = arrivingProbArrayList[[m-12]][hour[i], 2, day[i]])
                }
                admissionRateMonth[[m]] = list(dET, dUT)
            }else{print(paste0("Error. Month ", month, " are more than max. months allowed (24 months)."))}
        }
    }else{print("Error in generate admission rate. No valid error was selected")}
    
    admissionRate = data.table::rbindlist(admissionRateMonth)
    dET = admissionRate$V1
    dUT = admissionRate$V2

    dETS<-ifelse(dET>0, seq_along(dET), NA)
    dETS<-data.frame(dETS, dET)
    dETS<-dETS[complete.cases(dETS), ]
    names(dETS)<-c("hour", "patient")
    dUTS<-ifelse(dUT>0, seq_along(dUT), NA)
    dUTS<-data.frame(dUTS, dUT)
    dUTS<-dUTS[complete.cases(dUTS), ]
    names(dUTS)<-c("hour", "patient")
    admissionRate<-merge(dETS, dUTS, by = "hour", all.x = TRUE, all.y = TRUE)
    names(admissionRate)<-c("hour", "election", "urgency")
    admissionRate <- imputeTS::na_replace(admissionRate, 0)
    return(admissionRate)
}