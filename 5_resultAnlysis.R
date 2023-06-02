#Funzione risultati analisi

analysis<-function(result = result, hoursSim, nBedsStart, nsimulation){
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

    #Cost analysis
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
            sdrejected = sdrejected/(hoursSim/24)/nBedsStart
        )
    )
}
