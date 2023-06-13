#DB connection
con <- DBI::dbConnect(
  bigrquery::bigquery(),
  project = "amsterdamumcdb",
  dataset = "version1_0_2",
  LOCATION = "EU",
  billing = #your ID Google Cloud
)

#DB exploration
DBI::dbListTables(con)

#DB query
sql_admission <- "SELECT * FROM admissions"
sql_listitems <- "SELECT admissionid, itemid, item, value, measuredat, registeredby FROM listitems"
sql_numericitems <- "SELECT admissionid, itemid, item, value, measuredat, registeredby FROM numericitems"

#Table extraction
admission <- DBI::dbGetQuery(con, sql_admission)
listitems<- DBI::dbGetQuery(con, sql_listitems)
numericitems<- DBI::dbGetQuery(con, sql_numericitems)

#Type of variable - table admission

#ID
admission$admissionid<-as.factor(admission$admissionid)
#Variabili simulazione
admission$location<-as.factor(admission$location) # paziente ammesso a IC, MC, ICMC o MC&IC (non entra nel modello predittivo, ma puó essere utilizzato per una simulazione che prende in considerazione letti di IC e MC
#Variabili admssion rate
admission$admissionyeargroup<-as.factor(admission$admissionyeargroup)
#Variabili modello
admission$readmission<-ifelse(admission$admissioncount>1, 1, 0)
admission$readmission<-as.factor(admission$readmission)
admission$gender<-as.factor(admission$gender)
admission$urgency<-as.factor(admission$urgency)
admission$origin<-as.factor(admission$origin)
admission$agegroup<-as.factor(admission$agegroup)
admission$weightgroup<-as.factor(admission$weightgroup)
admission$heightgroup<-as.factor(admission$heightgroup)
admission$specialty<-as.factor(admission$specialty)

admission<-admission[, c("admissionid", "location", "readmission", "gender", "urgency", "origin", "agegroup", "weightgroup", "heightgroup", "specialty", "lengthofstay")]

#Type of variable - table admission diagnosis
apache<-listitems[grep("Apache", listitems$item, ignore.case=TRUE), ]
d<-listitems[grep("D_", listitems$item, ignore.case=TRUE), ]
dmc<-listitems[grep("DMC_", listitems$item, ignore.case=TRUE), ]

#spread stat column across multiple columns
d_item<-spread(d[!duplicated(d[, c("admissionid", "item")]), c("admissionid", "item")], item, key=item, fill = 0)
d_value<-spread(d[!duplicated(d[, c("admissionid", "item")]), c("admissionid", "item")], item, key=item, fill = 0)
dmc_item<-spread(dmc[!duplicated(dmc[, c("admissionid", "item")]), c("admissionid", "item")], item, key=item, fill = 0)
dmc_value<-spread(dmc[!duplicated(dmc[, c("admissionid", "value")]), c("admissionid", "value")], value, key=value, fill = 0)
apache_S<-apache[apache$item %in% c("APACHE IV Groepen", "Apache II Hoofdgroep"), ]
apache_S_value<-spread(apache_S[!duplicated(apache_S[, c("admissionid", "value")]), c("admissionid", "value")], value, key=value, fill = 0)

#Type of variable - table numericitems

day = c(0, seq((1000*60*60*24), (5*1000*60*60*24), 1000*60*60*24))

itemsMIN<-list(
    plt = c("9964", "6797", "10409", "14252"),
    ibp = c("6642", "6679", "8843"),
    alb = c("6801", "9937", "9975"),
    htc = c("6777", "11423", "11545")
)

itemsMAX<-list(
    cre = c("6836", "9941", "14216"),
    bil = c("6813", "9945"),
    lac = c("10053", "6837", "9580"),
    pct = c("15775", "15565"),
    clo = c("14413"),
    fio = c("6699", "12279", "12369", "16246")
) 

itemsMED<-list(
    pao = c("22214", "9996", "7433"),
    pco = c("21213", "9990", "6846"),
    pep = c("8862", "8879", "9666", "12284", "12301", "12336", "12364", "15142", "16250"),
    wbc = c("6779", "9965"),
    phh = c("6848", "12310"),
    nad = c("6840", "9555"),
    rrv = c("8873", "12266", "7726", "9654", "12283", "12322", "12348", "12577"),
    rrm = "8874",
    tem = c("8658", "8659", "8662", "13058", "13059", "13060", "13061", "13062", "13063", "13952", "16110")
)

#Data cleaning 1
numericitems<-numericitems[!numericitems$registeredby %in% "systeem", ]

#Data

#Funzione per trovare il MINORE valore giornaliero
HubHubHubMIN<-list()
for(h in 1:length(itemsMIN)){
    HubHub<-data.frame()
    for(i in 1:5){
        valAddIs<-numericitems[numericitems$itemid %in% itemsMIN[[h]]  & numericitems$measuredat >= day[i] & numericitems$measuredat < day[i+1], c("value", "admissionid")]
        wrovalAddId <- valAddIs[!is.na(valAddIs$admissionid), ] %>% group_by(admissionid) %>% summarise(wro = min(value, na.rm = T)) %>% select(admissionid, wro)
        hub<-data.frame(wrovalAddId, rep(paste0(names(itemsMIN[h]), i), dim(wrovalAddId)[1]))
        HubHub<-rbind(HubHub, hub)
    }
    names(HubHub)<- c("admissionid", "value", "type")
    HubHubHubMIN[[names(itemsMIN[h])]]<-HubHub
}

#Funzione per trovare il MAGGIORE valore giornaliero
HubHubHubMAX<-list()
for(h in 1:length(itemsMAX)){
    HubHub<-data.frame()
    for(i in 1:5){
        valAddIs<-numericitems[numericitems$itemid %in% itemsMAX[[h]]  & numericitems$measuredat >= day[i] & numericitems$measuredat < day[i+1], c("value", "admissionid")]
        wrovalAddId <- valAddIs[!is.na(valAddIs$admissionid), ] %>% group_by(admissionid) %>% summarise(wro = min(value, na.rm = T)) %>% select(admissionid, wro)
        hub<-data.frame(wrovalAddId, rep(paste0(names(itemsMAX[h]), i), dim(wrovalAddId)[1]))
        HubHub<-rbind(HubHub, hub)
    }
    names(HubHub)<- c("admissionid", "value", "type")
    HubHubHubMAX[[names(itemsMAX[h])]]<-HubHub
}

#Funzione per trovare la mediana
HubHubHubMED<-list()
for(h in 1:length(itemsMED)){
    HubHub<-data.frame()
    for(i in 1:5){
        valAddIs<-numericitems[numericitems$itemid %in% itemsMED[[h]]  & numericitems$measuredat >= day[i] & numericitems$measuredat < day[i+1], c("value", "admissionid")]
        wrovalAddId <- valAddIs[!is.na(valAddIs$admissionid), ] %>% group_by(admissionid) %>% summarise(wro = median(value, na.rm = T)) %>% select(admissionid, wro)
        hub<-data.frame(wrovalAddId, rep(paste0(names(itemsMED[h]), i), dim(wrovalAddId)[1]))
        HubHub<-rbind(HubHub, hub)
    }
    names(HubHub)<- c("admissionid", "value", "type")
    HubHubHubMED[[names(itemsMAX[h])]]<-HubHub
}

#Unisci i database

df <- do.call("rbind", HubHubHubMAX)
data_frame_mod1 <- reshape(df, idvar = "admissionid", timevar = "type", direction = "wide")

df <- do.call("rbind", HubHubHubMIN)
data_frame_mod2 <- reshape(df, idvar = "admissionid", timevar = "type", direction = "wide")
DATA<-merge(data_frame_mod1, data_frame_mod2, by = "admissionid", all.x = TRUE)

df <- do.call("rbind", HubHubHubMED)
data_frame_mod <- reshape(df, idvar = "admissionid", timevar = "type", direction = "wide")
DATA<-merge(DATA, data_frame_mod, by = "admissionid", all.x = TRUE)

itemsSUM<-list(
    uro = c("8794", "8796", "8798", "8800", "8803")
)

HubHubHubSUM<-list()
for(h in 1:length(itemsSUM)){
    HubHub<-data.frame()
    for(i in 1:5){
        valAddIs<-numericitems[numericitems$itemid %in% itemsSUM[[h]]  & numericitems$measuredat >= day[i] & numericitems$measuredat < day[i+1], c("value", "admissionid")]
        wrovalAddId <- valAddIs[!is.na(valAddIs$admissionid), ] %>% group_by(admissionid) %>% summarise(wro = sum(value, na.rm = T)) %>% select(admissionid, wro)
        hub<-data.frame(wrovalAddId, rep(paste0(names(itemsSUM[h]), i), dim(wrovalAddId)[1]))
        HubHub<-rbind(HubHub, hub)
    }
    names(HubHub)<- c("admissionid", "value", "type")
    HubHubHubSUM[[names(itemsSUM[h])]]<-HubHub
}

df <- do.call("rbind", HubHubHubSUM)
data_frame_mod <- reshape(df, idvar = "admissionid", timevar = "type", direction = "wide")
DATA<-merge(DATA, data_frame_mod, by = "admissionid", all.x = TRUE)

rrv = c("8873", "12266", "7726", "9654", "12283", "12322", "12348", "12577")
rrm = "8874"

    HubHub<-data.frame()
    for(i in 1:5){
        valAddIs<-numericitems[numericitems$itemid %in% rrv  & numericitems$measuredat >= day[i] & numericitems$measuredat < day[i+1], c("value", "admissionid")]
        wrovalAddId <- valAddIs[!is.na(valAddIs$admissionid), ] %>% group_by(admissionid) %>% summarise(wro = mean(value, na.rm = T)) %>% select(admissionid, wro)
        hub<-data.frame(wrovalAddId, rep(paste0("rrm", i)))
        HubHub<-rbind(HubHub, hub)
    }
names(HubHub)<- c("admissionid", "value", "type")

data_frame_mod2 <- reshape(HubHub, idvar = "admissionid", timevar = "type", direction = "wide")
DATA<-merge(DATA, data_frame_mod2, by = "admissionid", all.x = TRUE)
dim(data)

    HubHub<-data.frame()
    for(i in 1:5){
        valAddIs<-numericitems[numericitems$itemid %in% rrm  & numericitems$measuredat >= day[i] & numericitems$measuredat < day[i+1], c("value", "admissionid")]
        wrovalAddId <- valAddIs[!is.na(valAddIs$admissionid), ] %>% group_by(admissionid) %>% summarise(wro = mean(value, na.rm = T)) %>% select(admissionid, wro)
        hub<-data.frame(wrovalAddId, rep(paste0("rvm", i)))
        HubHub<-rbind(HubHub, hub)
    }
names(HubHub)<- c("admissionid", "value", "type")

data_frame_mod2 <- reshape(HubHub, idvar = "admissionid", timevar = "type", direction = "wide")
DATA<-merge(DATA, data_frame_mod2, by = "admissionid", all.x = TRUE)
names(DATA)

# Clean 2
completeData <- as.data.frame(lapply(DATA[, 2:101], function(x) {
  q1 <- quantile(x, .010, na.rm = T)
  q2 <- quantile(x, .990, na.rm = T)
  replace(x, (x < q1 | x > q2), NA)
}))
completeData <- as.data.frame(lapply(completeData, function(x) {
  replace(x, (x%in%0.00), NA)
}))
completeData<-cbind(DATA$admissionid, completeData)
names(completeData)[1]<-"admissionid"

#Database complet for analysis
dim(admission)
data<-merge(admission, d_item, by = "admissionid", all=T)
dim(data)
data<-merge(data, d_value, by = "admissionid", all=T)
dim(data)
data<-merge(data, dmc_item, by = "admissionid", all=T)
dim(data)
data<-merge(data, dmc_value, by = "admissionid", all=T)
dim(data)
data<-merge(data, apache_S_value, by = "admissionid", all=T)
dim(data)
data<-merge(data, completeData, by = "admissionid", all=T)
dim(data)

HubHubHubGCS<-list()
    HubHub<-data.frame()
    for(i in 1:5){
        valAddIs<-listitems[listitems$itemid %in% 6734  & listitems$measuredat >= day[i] & listitems$measuredat < day[i+1], c("value", "admissionid")]
        wrovalAddId <- valAddIs[!is.na(valAddIs$admissionid), ] %>% group_by(admissionid) %>% summarise(wro = value[1]) %>% select(admissionid, wro)
        hub<-data.frame(wrovalAddId, rep(paste0("gcs", i)))
        HubHub<-rbind(HubHub, hub)
    }
    names(HubHub)<- c("admissionid", "value", "type")
    HubHubHubGCS[["gcs"]]<-HubHub

df <- do.call("rbind", HubHubHubGCS)
data_frame_mod2 <- reshape(df, idvar = "admissionid", timevar = "type", direction = "wide")
data<-merge(data, data_frame_mod2, by = "admissionid", all.x = TRUE)