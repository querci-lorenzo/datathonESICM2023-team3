analysisAdv<-function(result = result, hoursSim = 30*24, nBedsStart, nsimulation = 4){
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

    #return
    return(
        list(
            plot1 = plot1,
            hourbedfreeday = hourbedfreeday,
            hourbedfreedaysd = hourbedfreedaysd,
            meanrejected = meanrejected/(hoursSim/24),
            sdrejected = sdrejected/(hoursSim/24)
        )
    )
}
