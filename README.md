library(foreign)
library(plyr)
library(dplyr)
library(zoo)
library(forecast)
library(stats)
library(lubridate)

###Import datasets##

APMC<-read.csv("file://D:/Agriculture Comoditties/Monthly_data_cmo.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)

##Data exploration##

library(car)
coplot(modal_price~APMC_Commodity|date,type="l",data = APMC)

boxplot(modal_price~APMC_Commodity, smooth=TRUE, data=APMC)

##Data Prepration##

APMC<-APMC[APMC$max_price>=APMC$min_price,]
APMC<-APMC[APMC$modal_price>=APMC$min_price,]
APMC<-APMC[APMC$modal_price<=APMC$max_price,]
APMC<-APMC[APMC$modal_price>0,]
APMC<-APMC[APMC$min_price>0,]
APMC<-APMC[-which.max(APMC$max_price),]

APMC$APMC<-sapply(APMC$APMC,toupper)
APMC$Commodity<-sapply(APMC$Commodity,toupper)
APMC$APMC_Commodity<-as.factor(paste(APMC$APMC,APMC$Commodity,sep = "_"))

##Removing major outliers(3*IQR) from cluster of APMC_Commodity##
apmc<-APMC%>%group_by(APMC_Commodity)%>%filter_all(all_vars(modal_price<(quantile(modal_price,probs = .75)+3*IQR(modal_price))|modal_price>(quantile(modal_price,probs = .25)-3*IQR(modal_price))))

apmc$APMC_Commodity<-as.character(apmc$APMC_Commodity)
apmc$APMC_Commodity<-as.factor(apmc$APMC_Commodity)

#Filtering clusters less than 12 data points and 2 clusters with duplicate values(SATARA_GARLIC & NAGPUR_GARLIC)
tab<-table(apmc$APMC_Commodity)
idx<-names(tab)[tab>11&tab<28]
apmc<-apmc[apmc$APMC_Commodity%in%idx,c(12,5:9)]
apmc$APMC_Commodity<-as.character(apmc$APMC_Commodity)
apmc$APMC_Commodity<-as.factor(apmc$APMC_Commodity)


########Subset equally spaced time interval clusters###

tab2<-table(apmc$APMC_Commodity)
idx2<-names(tab2)[tab2==27]

A1<-apmc[apmc$APMC_Commodity%in%idx2,]
A1$APMC_Commodity<-as.character(A1$APMC_Commodity)
A1$APMC_Commodity<-as.factor(A1$APMC_Commodity)

A1$date<-as.yearmon(A1$date)
A1<-A1[order(A1$APMC_Commodity,A1$date),]  ##Sorting chronologically##


#Split panel data by Group/Section time series objects
#Derving seasonal pattern & type and trend:modal_price

md<-split(A1$modal_price,A1$APMC_Commodity)
Decomp_md<-list()
Seasonal_md<-list()
Trend_random_md<-list()
for (i in seq_along(md)) {
  Decomp_md[[i]]<-ts(md[[i]],start = c(2014,9), frequency = 12)
  Decomp_md[[i]]<-decompose(Decomp_md[[i]])
  Seasonal_md[[i]]<- list(Decomp_md[[i]]$seasonal,Decomp_md[[i]]$type)
  Trend_random_md[[i]]<-list(Decomp_md[[i]]$trend,Decomp_md[[i]]$random)
}

md_deseason<-list()
for (i in seq_along(md)) {
  md_deseason[[i]]<-md[[i]]-Seasonal_md[[i]][[1]]
}

Modal_prices<-data.frame(APMC_commodity=rep(names(md),each=27),modal_price=unlist(md,use.names = F,recursive = T),modal_price_ds=unlist((md_deseason),use.names = F,recursive = T),date=seq.Date(as.Date("2014/9/1"),as.Date("2016/11/1"),by="months"))
Modal_prices$year<-year(Modal_prices$date)
Modal_prices$Commodity<-sub(".*_", "", Modal_prices$APMC_commodity)

write.csv(Modal_prices,"modal.csv")

names(md_deseason)<-names(md)
names(Decomp_md)<-names(md)
names(Seasonal_md)<-names(md)
names(Trend_random_md)<-names(md)

plot(Decomp_md[[456]])

#Derving seasonal pattern & type and trend:min_price

mn<-split(A1$min_price,A1$APMC_Commodity)
Decomp_mn<-list()
Seasonal_mn<-list()
Trend_random_mn<-list()
for (i in seq_along(mn)) {
  Decomp_mn[[i]]<-ts(mn[[i]],start = c(2014,9), frequency = 12)
  Decomp_mn[[i]]<-decompose(Decomp_mn[[i]])
  Seasonal_mn[[i]]<- list(Decomp_mn[[i]]$seasonal,Decomp_mn[[i]]$type)
  Trend_random_mn[[i]]<-list(Decomp_mn[[i]]$trend,Decomp_mn[[i]]$random)
}

mn_deseason<-list()
for (i in seq_along(mn)) {
  mn_deseason[[i]]<-mn[[i]]-Seasonal_mn[[i]][[1]]
}

Min_prices<-data.frame(APMC_commodity=rep(names(mn),each=27),min_price=unlist(mn,use.names = F,recursive = T),min_prices_ds=unlist((mn_deseason),use.names = F,recursive = T),date=seq.Date(as.Date("2014/9/1"),as.Date("2016/11/1"),by="months"))
Min_prices$year<-year(Min_prices$date)
Min_prices$Commodity<-sub(".*_", "", Min_prices$APMC_commodity)

write.csv(Min_prices,"min.csv")

names(mn_deseason)<-names(mn)
names(Decomp_mn)<-names(mn)
names(Seasonal_mn)<-names(mn)
names(Trend_random_mn)<-names(mn)


#Derving seasonal pattern & type and trend:max_price

mx<-split(A1$max_price,A1$APMC_Commodity)
Decomp_mx<-list()
Seasonal_mx<-list()
Trend_random_mx<-list()
for (i in seq_along(mx)) {
  Decomp_mx[[i]]<-ts(mx[[i]],start = c(2014,9), frequency = 12)
  Decomp_mx[[i]]<-decompose(Decomp_mx[[i]])
  Seasonal_mx[[i]]<- list(Decomp_mx[[i]]$seasonal,Decomp_mx[[i]]$type)
  Trend_random_mx[[i]]<-list(Decomp_mx[[i]]$trend,Decomp_mx[[i]]$random)
}

mx_deseason<-list()
for (i in seq_along(mx)) {
  mx_deseason[[i]]<-mx[[i]]-Seasonal_mx[[i]][[1]]
}

Max_prices<-data.frame(APMC_commodity=rep(names(mx),each=27),max_price=unlist(mx,use.names = F,recursive = T),max_prices_ds=unlist((mx_deseason),use.names = F,recursive = T),date=seq.Date(as.Date("2014/9/1"),as.Date("2016/11/1"),by="months"))
Max_prices$year<-year(Max_prices$date)
Max_prices$Commodity<-sub(".*_", "", Max_prices$APMC_commodity)

write.csv(Max_prices,"max.csv")

names(mx_deseason)<-names(mx)
names(Decomp_mx)<-names(mx)
names(Seasonal_mx)<-names(mx)
names(Trend_random_mx)<-names(mx)



