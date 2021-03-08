setwd("/users/cindy/Desktop/DATA SCIENCE/los-angeles-parking-citations")
install.packages("data.table")
install.packages("ggplot2")
library(data.table)
library(tidyverse)
library(readr)
library(corrplot)
library(stringr)
library(plyr)
library(ggplot2)
library(lubridate)



parking=fread("parking-citations.csv")

str(parking)
summary(parking)
colnames(parking)

#############################################################
##CLeaning Up Variables

#missing values management
parking[parking==""]=NA
missingvalues= parking %>% summarize_all(funs(sum(is.na(.))/n()))
missingvalues= gather(missingvalues, key="feature", value="missing_pct")
missingvalues %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()
#will need to drop VIN, Marked Time, Meter Id due to too many missing values

parking1=parking[,-c(4,5,8)]

#Seperate Plate Expiration Year and Month
parking1$ExpYear=as.numeric(substr(parking1$`Plate Expiry Date`,1,4))
parking1$ExpMonth=as.numeric(substr(parking1$`Plate Expiry Date`,5,6))

#Seperate Issue Date to Year Month and Day
parking1$`Issue Date`=substr(parking1$`Issue Date`,1,nchar(parking1$`Issue Date`)-9)
parking1$Year=as.numeric(substr(parking1$`Issue Date`,1,4))
parking1$Month=as.numeric(substr(parking1$`Issue Date`,6,7))
parking1$Day=as.numeric(substr(parking1$`Issue Date`,9,10))

#table shows that there is not enough informaiton for years 2010-2014,will need to get rid of previous years before 2015
table(parking1$Year)
parking1=subset(parking1, Year==2015|Year==2016|Year==2017| Year==2018|Year==2019)

#if only working with complete cases, narrows it down to 7860402, which is still good enough to analyze
parking1=parking1[complete.cases(parking1),]

#don't need to analyze 2019, since it is an incomplete year
Parking2=subset(parking1, Year==2015|Year==2016|Year==2017| Year==2018)



######################################
#Starting to look at fiscal year reports by year
Parking3=subset(parking1, Year==2015)
Parking3=subset(parking1, Year==2016)
hist(Parking3$Month,ylim=range(100000,200000))

Parking3$`Issue Date`=as.Date(Parking3$`Issue Date`)


#Exploring data before truncating
hist(parking1$Year, col="red",xlab="Year",ylab="Number of Parking Citations",main="Citations by Year")
hist(parking1$Year,col="black",xlim=range(2015,2019), ylim=range(1600000,1900000),xlab="Year",ylab="Number of Parking Citations",main="Citations 2015-2018")
#Less citations in 2018

hist(Parking2$Month, col="blue", ylim=range(500000,680000),main="Citations by Month",xlab="Month", ylab="Number of Parking Citations")
#THe most citations happened in March

x=table(Parking2$`Day`)
barplot(x, xlab="Day of the Month",ylab="Number rof Parking Citations",main="Citations by Day of the Month",col="purple")
#Data is evenly distributed so I did not want to explore much further

ggplot(Parking2,aes(`Issue time`))+geom_freqpoly(bins=48)+labs(title="Parking Citations throughout Time of Day",x="Issued Time")
#8 am, 10 am,12pm most amount of tickets

Parking2$`Issue Date`=as.Date(Parking2$`Issue Date`)
Parking2$Weedkay=weekdays(Parking2$`Issue Date`)
Parking2$Weedkay=ordered(Parking2$Weedkay,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
day=data.frame(table(Parking2$Weedkay))
ggplot(day,aes(Var1,Freq))+geom_bar(stat="identity",fill="yellow")+labs(x="Day of the Week",title="Citations by Day of the Week")
#Most tickets are gven during the week, specifically from tuesday through thursday. Observed holidays are usually on Monday, so that 
#explains why Monday is not as popular as the rest of the weekdays. Fridays are also not very popular.

###2015
parking2015=subset(Parking2,Year==2015)
parking2015$`Issue Date`=as.Date(parking2015$`Issue Date`)
x=data.frame(table(parking2015$Month))
ts=x
colnames(x)=c("Month","Count")
x$Month=as.numeric(x$Month)
ggplot(x,aes(x=Month,y=Count))+geom_point()+geom_line(stat="identity")+geom_vline(xintercept=3,color="blue")+geom_vline(xintercept=5.9,color="red")+geom_vline(xintercept=10.74, color="blue")+geom_vline(xintercept=12.12,color="blue")+labs(title="2015")
ts$Var1=paste("2015.",ts$Var1,sep="")
  
##2016
parking2016=subset(Parking2,Year==2016)
parking2016$`Issue Date`=as.Date(parking2016$`Issue Date`)
y=data.frame(table(parking2016$Month))
temp=y
colnames(y)=c("Month","Count")
y$Month=as.numeric(y$Month)
ggplot(y,aes(x=Month,y=Count))+geom_point()+geom_line(stat="identity")+geom_vline(xintercept=2.92,color="blue")+geom_vline(xintercept=5.38,color="blue")+geom_vline(xintercept=6.1, color="red")+geom_vline(xintercept=10.9,color="blue")+geom_vline(xintercept=12.06,color="blue")+labs(title="2016")
temp$Var1=paste("2016.",temp$Var1,sep="")
ts=rbind(ts,temp)

##2017
parking2017=subset(Parking2,Year==2017)
parking2017$`Issue Date`=as.Date(parking2017$`Issue Date`)
z=data.frame(table(parking2017$Month))
temp=z
temp$Var1=paste("2017.",temp$Var1,sep="")
ts=rbind(ts,temp)
colnames(z)=c("Month","Count")
z$Month=as.numeric(z$Month)
ggplot(z,aes(x=Month,y=Count))+geom_point()+geom_line(stat="identity")+geom_vline(xintercept=3.06,color="blue")+geom_vline(xintercept=6.01,color="red")+geom_vline(xintercept=10.74, color="blue")+geom_vline(xintercept=12.19,color="blue")+labs(title="2017")


##2018
parking2018=subset(Parking2,Year==2018)
parking2018$`Issue Date`=as.Date(parking2018$`Issue Date`)
a=data.frame(table(parking2018$Month))
temp=a
temp$Var1=paste("2018.",temp$Var1,sep="")
ts=rbind(ts,temp)
colnames(a)=c("Month","Count")
a$Month=as.numeric(a$Month)
ggplot(a,aes(x=Month,y=Count))+geom_point()+geom_line(stat="identity")+geom_vline(xintercept=3.06,color="blue")+geom_vline(xintercept=6.01,color="red")+geom_vline(xintercept=10.74, color="blue")+geom_vline(xintercept=12.19,color="blue")+labs(title="2018")

#Every year there is a consistent pattern the second mid year report always happens around March and the number of parking
#citatins tends to increase as the date approaches. The year end report and the first mid year report don't tend to influence
#the number of parking citations.

ts1=ts
ts1=ts(ts1$Freq,frequency=12,start=(2015))
lts1=log(ts1)
plot(ts1,ylab="Number of Citations",main="Citations 2015-2018")
#Fiscal Year Reports
abline(v=decimal_date(ymd("2015-03-03")),col="blue")
abline(v=decimal_date(ymd("2015-05-28")),col="red")
abline(v=decimal_date(ymd("2015-10-23")),col="blue")
abline(v=decimal_date(ymd("2015-12-04")),col="blue")
abline(v=decimal_date(ymd("2016-02-26")),col="blue")
abline(v=decimal_date(ymd("2016-05-12")),col="blue")
abline(v=decimal_date(ymd("2016-06-03")),col="red")
abline(v=decimal_date(ymd("2016-10-28")),col="blue")
abline(v=decimal_date(ymd("2016-12-02")),col="blue")
abline(v=decimal_date(ymd("2017-03-16")),col="blue")
abline(v=decimal_date(ymd("2017-05-31")),col="red")
abline(v=decimal_date(ymd("2017-10-27")),col="blue")
abline(v=decimal_date(ymd("2017-11-30")),col="blue")
abline(v=decimal_date(ymd("2018-03-02")),col="blue")
abline(v=decimal_date(ymd("2018-06-01")),col="red")
abline(v=decimal_date(ymd("2018-10-26")),col="blue")
abline(v=decimal_date(ymd("2018-12-06")),col="blue")
legend(2018,170000,legend=c("Mid Year Reports","End of Fiscal Year"),col=c("blue","red"),lty=1:2,cex=0.7)

#looking at all 4 years altogether, you can see the pattern repeating every year

#ts$Var1=as.factor(ts$Var1)

#ggplot(ts,aes(x=ts$Var1,y=ts$Freq))+geom_line()

#Rename make models
make=data.frame(table(parking1$`Make`))
#table shows that there is 2303 different types of car makes(variety of abbreviations for same car make

parking1=subset(parking1, Year==2015|Year==2016|Year==2017| Year==2018)
#Renaming Labels
parking1$Make=gsub("TOYO","TOYT",parking1$Make)
parking1$Make=gsub("DDOG","DODG",parking1$Make)
parking1$Make=gsub("BMER","BMW",parking1$Make)
parking1$Make=gsub("VW","VOLK",parking1$Make)
parking1$Make=gsub("AAUD","AUDI",parking1$Make)
parking1$Make=gsub("MIT","MITS",parking1$Make)
parking1$Make=gsub("LIN","LINC",parking1$Make)
parking1$Make=gsub("FIA","FIAT",parking1$Make)
parking1$Make=gsub("RANG","RROV",parking1$Make)
parking1$Make=gsub("MASR","MASE",parking1$Make)
parking1$Make=gsub("SMAR","SMRT",parking1$Make)
parking1$Make=gsub("HUMR","HUMM",parking1$Make)
parking1$Make=gsub("BNTL","BENT",parking1$Make)
parking1$Make=gsub("DAT","DATS",parking1$Make)
parking1$Make=gsub("OLD","OLDS",parking1$Make)
parking1$Make=gsub("FREI","FRHT",parking1$Make)

find.list=list("HONDA","HND","HDA")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"HOND",parking1$Make)

find.list=list("NISN","NIS","NSSN")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"NISS",parking1$Make)

find.list=list("MBNZ","BENZ","MBZ","MER","MECD","MBEZ","BNZ","MBEN")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"MERZ",parking1$Make)

find.list=list("HYND","HYU","HYN","HUYD","HUND")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"HYUN",parking1$Make)

find.list=list("LEXU","LEX","LXUS","LXXS","LXXU")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"LEXS",parking1$Make)

find.list=list("HYND","HYU","HYN","HUYD","HUND")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"HYUN",parking1$Make)

find.list=list("MZD","MZDA","MAZ","MAZA","MAZDA")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"MAZD",parking1$Make)

find.list=list("INF","INFN","INFT","INFY")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"INFI",parking1$Make)

find.list=list("CHRL","CHRS","CHSL","CHSR")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"CHRY",parking1$Make)

find.list=list("ACR","ACRA","ACU","ACUA")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"ACUR",parking1$Make)

find.list=list("SUB","SUBU","SUBR")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"SUBA",parking1$Make)

find.list=list("CAD","CADY")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"CADI",parking1$Make)

find.list=list("LRVR","LRV","LAND","LNRV")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"LNDR",parking1$Make)

find.list=list("POR","PRSH")##
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"PORS",parking1$Make)

find.list=list("BUICK","BUIK")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"BUIC",parking1$Make)

find.list=list("PONI","PONC")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"PONT",parking1$Make)

find.list=list("SATU","SAT","SATR","SATN")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"STRN",parking1$Make)

find.list=list("JAGR","JAG","JAGA","JGR")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"JAGU",parking1$Make)

find.list=list("KAWA","KWAS")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"KAWK",parking1$Make)

find.list=list("HARL","HRLY")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"HD",parking1$Make)

find.list=list("ROLS","ROLL")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"ROL",parking1$Make)

find.list=list("FER","FERA","FERI")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"FERR",parking1$Make)

find.list=list("ISUZ","ISUS","ISZU","IZUZ")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"ISU",parking1$Make)

find.list=list("PLY","PLYT")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"PLYM",parking1$Make)

find.list=list("MINI","COOP")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"MNNI",parking1$Make)

find.list=list("FRGT","FRGH","FHRT","FHT")
find.string=paste(unlist(find.list), collapse = "|")
parking1$Make=gsub(find.string,"FRHT",parking1$Make)

sort(table(parking1$'Make'),decreasing=T)

parking1$Make=gsub("NISSS","NISS",parking1$Make)
parking1$Make=gsub("MERZZ","MERZ",parking1$Make)
parking1$Make=gsub("HYUNNN","HYUN",parking1$Make)
parking1$Make=gsub("LEXSSS","LEXS",parking1$Make)
parking1$Make=gsub("MAZDD","MAZD",parking1$Make)
parking1$Make=gsub("ACURR","ACUR",parking1$Make)
parking1$Make=gsub("SUBAA","SUBA",parking1$Make)
parking1$Make=gsub("INFII","INFI",parking1$Make)

x=head(sort(table(parking1$`Make`), decreasing=T),n=20)
barplot(x,xlab="Make", ylab="Frequency", main="Top 20 Car Makes with Citations")
#most common car makes are Toyota honda Nissan, etc.

#Factors
#converting to categorical variables
parking1$`RP State Plate`=as.factor(parking1$`RP State Plate`)
parking1$`Body Style`=as.factor(parking1$`Body Style`)
parking1$Color=as.factor(parking1$Color)
parking1$Agency=as.factor(parking1$Agency)
parking1$`Violation code`=as.factor(parking1$`Violation code`)
parking1$`Route`=as.factor(parking1$`Route`)
parking1$`Make`=as.factor(parking1$`Make`)


#finding and sorting top of categorical variables
sort(table(parking1$`RP State Plate`), decreasing=T)
sort(table(parking1$Make), decreasing=T)
sort(table(parking1$`Body Style`), decreasing=T)
sort(table(parking1$`Color`), decreasing=T)
sort(table(parking1$`Violation code`), decreasing=T)
sort(table(parking1$`Route`), decreasing=T)
sort(table(parking1$`Agency`), decreasing=T)




###############################################################
######EDA

###Factor Variables
#Single Variable

x=head(sort(table(parking1$`RP State Plate`), decreasing=T),n=20)
x2=x[2:20]
barplot(x,xlab="State Plate", ylab="Frequency",main="Top 20 State License Plates")
barplot(x2,xlab="State Plate", ylab="Frequency",main="Non-CA Top License Plates")
#CA, AZ, TX, NV, FL are the top 5 states that received a parking citation over the years of

x3=data.frame(head(sort(table(parking1$`Agency`), decreasing=T),n=12))
x3$Var1=as.factor(x3$Var1)
levels(x3$Var1)=c("54"="DOT HLYW","51"="DOT WEST","56"="DOT CENTL","53"="DOT VALY","55"="DOT STHN","1"="WESTERN","2"="LAX CUR","57"="HAB VIOLS","58"="SPEC EVENT","40"="BLDG & SAF","11"="VN AIRPORT","3"="VALLEY")
ggplot(x3,aes(Var1,Freq))+geom_bar(stat="identity",fill="turquoise")+theme(axis.text.x = element_text(angle=65))+labs(x="Agency",title="Top 12 Agencies")
#Top agenncies are the Department of Transportation offices

x4=head(sort(table(parking1$`Make`), decreasing=T),n=20)
barplot(x4,xlab="Make", ylab="Frequency",main="Top 20 Car Makes")
#TOYT,HOND,FORD,NISS CHEV, BMW, MERZ,VOLK, HYUN,DODG most common makes to get tickets

x5=head(sort(table(parking1$`Body Style`), decreasing=T),n=10)
barplot(x5,xlab="Body Style", ylab="Frequency", main="Top 10 Car Styles")
#passenger, pick up, van, tanker,concrete, tractor tk, motorcycle, su, motorhome,office trl

dx5=data.frame(x5)
dx5$Total[dx5$Var1=="PA"]=6479206
dx5$Total[dx5$Var1=="PU"]=1107381
dx5$Total[dx5$Var1=="TR"]=283930
dx5$Total[dx5$Var1=="MC"]=158309
dx5=dx5[complete.cases(dx5),]
dx5$percent=dx5$Freq/dx5$Total
bp=barplot(dx5$percent,names.arg=c("PA","PU","TR","MC"),xlab="Body Style", ylab="Tickets Relative to Number of Registered Cars", main="Top 10 Car Styles tickets")
text(x=bp[,1], y=-1, adj=c(1, 1), Var1, cex=0.8, srt=45, xpd=TRUE)
#Passenger vehicles are the car styes that most commonly get tickets, this is proven even after taking the total number of
#registered cars

parking1$Color=gsub("WH","WT",parking1$Color)
x6=head(sort(table(parking1$`Color`), decreasing=T),n=10)
barplot(x6,xlab="Color", ylab="Frequency",main="Top 10 Car Colors")
#Black, White, Grey, Silver, Blue, Red, Green, Burgundy, Go, MR

x7=head(sort(table(parking1$`Route`), decreasing=T),n=10)
barplot(x7,xlab="Route", ylab="Frequency")

x8=head(sort(table(parking1$`Violation code`), decreasing=T),n=10)
barplot(x8,xlab="Violation Code", ylab="Frequency", main="Top Violation Codes")
#most common tickets are for no parking/street sweeping, meter expired, preferential parking, red zone and display of tabs


x9=head(sort(table(parking1$`Violation Description`), decreasing=T),n=13)
barplot(x9,xlab="Violation Description", ylab="Frequency",cex.names=.6,main="Top Violations")
#top vioaltions dont correlate with top violation codes, don't use

ggplot(parking1, aes(`Fine amount`))+geom_histogram(bins=500)+labs(x="Fine Amount",y="Frequency",title="Distibution of fine Amounts")
#most tickets were between $50-$100

######################################################3
#######################################
#Models on 2018
#fixing categorical variables to become other if not within top 10
topcars=c("TOYT","HOND","FORD","NISS","CHEV","BMW","MERZ","VOLK","HYUN","DODG","LEXS","KIA","JEEP","MAZD","AUDI")
parking2018$Make[!(parking2018$Make %in% topcars)]="OTHR"
table(parking2018$Make)

topcolors=c("BK","WT","GY","SL","BL","RD","GN","GO","MR","BN")
parking2018$Color[!(parking2018$Color %in% topcolors)]="OTHR"
table(parking2018$Color)

topstyles=c("PA","PU","VN","TK","CM","TR","MH","MC","SU","OT","RV","MS","BU","UT")
parking2018$`Body Style`[!(parking2018$`Body Style` %in% topstyles)]="XX"
table(parking2018$`Body Style`)

topcodes=c("80.69BS","88.13B+","80.58L","80.56E4+","5204A-","80.69B","80.69C","5200","80.56E1","80.69AP+")
parking2018$`Violation code`[!(parking2018$`Violation code` %in% topcodes)]="OTHR"
table(parking2018$`Violation code`)


################################################
#preparing for regression models
str(parking2018)
parking2018[,c(4,6,7,8,11,12,22)]=lapply(parking2018[,c(4,6,7,8,11,12,22)], factor)


park18=parking2018[,-c(1,5,9,10,13,15,16,17,18,19)]
park18[,c(3,4,5,6,7,8,12)]=lapply(park18[,c(3,4,5,6,7,8,12)], factor)
str(park18)

day=data.frame(table(park18$Weedkay))
ggplot(day,aes(Var1,Freq))+geom_bar(stat="identity",fill="yellow")+labs(x="Day of the Week",title="Citations by Day of the Week")

###Edit to change time of day distribution
park18$tod[park18$`Issue time`>=800 & park18$`Issue time`<=1200]="M"#Morning
park18$tod[park18$`Issue time`>1200 & park18$`Issue time`<=1700]="A"#afternoon
park18$tod[park18$`Issue time`>1700 & park18$`Issue time`<=2000]="E"#evening
park18$tod[park18$`Issue time`>2000 | park18$`Issue time`<800]="O"#overnight
park18$tod=as.factor(park18$tod)
barplot(table(park18$tod),xlab="Time of Day", ylab="Frequency",main="Distribution by Time of Day")
#Mornings are the most common tickets

#################################################################################################
##Multiple Variables Exploratoin in 2018 of only complete observations

park18$Month=as.factor(park18$Month)

#park18$Month=ordered(park18$Month,levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
ggplot(park18,aes(Month))+geom_bar(aes(fill=`Violation code`))+labs(title="Distribution of Violations by Month")

ggplot(park18,aes(Weedkay))+geom_bar(aes(fill=`Violation code`))+labs(title="Distribution of Violations by Weekday")

ggplot(park18,aes(`Violation code`,`Fine amount`))+geom_violin(col="orange2")+labs(title="Fine amounts by Violation Code")

ggplot(park18,aes(Agency))+geom_bar(aes(fill=`Violation code`))+labs(title="Distribution of Violations by Agency")

ggplot(park18,aes(Agency,`Fine amount`))+geom_violin(col="orange2")+labs(title="Fine amounts by Agency")+ylim(range(20,150))

ggplot(park18,aes(`RP State Plate`))+geom_bar(aes(fill=`Violation code`),position="fill")+labs(title="Distribution of Violations by State Plate")

ggplot(park18,aes(Weedkay,`Issue time`))+geom_violin(col="red")+labs(title="Citations by Time and Day of Week")+xlab("Weekday")


#########################
#Correlation
parking2018$Agency=as.numeric(parking2018$Agency)
numparking=parking2018[,c(3,5,11,14,15,16,20,21)]
cor(numparking,use="complete.obs") #no multicollnearity
corrplot(cor(numparking,use="complete.obs"), use="complete.obs")
#longitude and latitude show collinearity

########################################################
#Models
model1=lm(`Fine amount`~.,park18)
summary(model1)
#Most siginificant variables are weekday, violation code, Agency,body style, and issue time

model2=lm(`Fine amount`~Day+Weedkay+Month,park18)
#model with time,not significant enough
summary(model2)

model3=lm(`Fine amount`~`Issue time`+Agency+`Violation code`+Weedkay,park18)
summary(model3)  


model4=lm(`Fine amount`~`Issue time`+`Body Style`+`Violation code`+Weedkay+Make+Color,park18)
summary(model4)

model5=lm(`Fine amount`~`Issue time`+Agency+Weedkay,park18)
summary(model5)#best model, which proves that without violation code, time of dat, agency, and day of week are statistically
#significant preedictors

#############################################3
##text
#install.packages("tidytext")
#library(tidytext)
#text_df=tibble(parking2018$`Violation Description`,text=text)
#parking2018$`Violation Description`%>%unnest_tokens(word,text)
###################################
###MAP
install.packages("proj4")
library(proj4)

#convering us feet coordniates to DMS
parkmap=parking2018 %>% filter(Latitude != 99999 | Longitude != 99999) 
pj ="+proj=lcc +lat_1=34.03333333333333 +lat_2=35.46666666666667 +lat_0=33.5 +lon_0=-118 +x_0=2000000 +y_0=500000.0000000002 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 no_defs"
parkmap =bind_cols(parkmap,proj4::project(data.frame(parkmap$Latitude, parkmap$Longitude), proj = pj, inverse = TRUE))


write.csv(parkmap,"/users/cindy/Desktop/DATA SCIENCE/los-angeles-parking-citations/parkmap.csv",row.names=FALSE)
#explore map coordinates on tableau






#After a lot of EDA it is easy to see that
#there wasa significant drop in citations in 2018. Although my linear models can’t be used to predict 
#fine amount, is it evident that Agency, Time, and Weekday are important variables in predicting probability of citation
#There is higher number of citations morning time from 8am to 12 pm, also on Weekdays Monday-Friday
#and the top agencies to cite parked cars are the Department of Transportation Offices followed by LAX Current
#Also after further exploration End of Fiscal year does not predict higher number of parking citations but mid year reports do, 
#which usually happen in March and November. Lower number of parking citations in beginning of fiscal year, June through October
