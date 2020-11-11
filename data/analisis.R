library(dplyr)
library(stringr)
library(forecast)
library(ggplot2)
library(reshape2)
library(lubridate)
library(ineq)
library(lmtest)
library(urca)

turista <- read.delim2("P:/Facultad/macro 3/trabajo final/data/dolar turista.txt")
oficial <- read.delim2("P:/Facultad/macro 3/trabajo final/data/dolar oficial.txt")
mep <- read.delim2("P:/Facultad/macro 3/trabajo final/data/dolar mep.txt")
ccl <- read.delim2("P:/Facultad/macro 3/trabajo final/data/dolar contado con liqui.txt")
blue <- read.delim2("P:/Facultad/macro 3/trabajo final/data/dolar blue.txt")

data <- oficial[,c(1,3)]
data <- merge(data, turista[,c(1,2)], by="Fecha", all.x = T)
data <- merge(data, mep[,c(1,2)], by="Fecha", all.x = T)
data <- merge(data, ccl[,c(1,2)], by="Fecha", all.x = T)
data <- merge(data, blue[,c(1,3)], by="Fecha", all.x = T)

data$Fecha <- as.character(data$Fecha)
data$Fecha <- as.Date(data$Fecha, format = '%d-%m-%Y')

data <- arrange(data, data$Fecha)
database <- data

data <- data[is.na(data$MEP)==F,]
data$Turista <- ifelse(is.na(data$Turista)==T, data$Oficial, data$Turista)

brechas <- data[,c(1,2)]
brechas$blue_ofi <- (data$Blue/data$Oficial)-1 
brechas$mep_ofi <- (data$MEP/data$Oficial)-1 
brechas$ccl_ofi <- (data$Ccl/data$Oficial)-1 
brechas$blue_tur <- (data$Blue/data$Turista)-1
brechas$mep_tur <- (data$MEP/data$Turista)-1
brechas$ccl_tur <- (data$Ccl/data$Turista)-1

brechas_ofi <- brechas[,c(1,3:5)]
brechas_ofi <- melt(brechas_ofi, id="Fecha")
brechas_tur <- brechas[,c(1,6:8)]
brechas_tur<- melt(brechas_tur, id="Fecha")

borrador <- melt(data, id="Fecha")
borrador$Fecha <- as.Date(borrador$Fecha, format ='%Y-%m-%d')

windows()
ggplot(borrador, aes(x=Fecha, y=value, colour=variable, group=variable))+
  geom_line(size=1)+
  scale_y_continuous(name = "Tipo de cambio", labels = scales::dollar)+
  scale_x_date(date_breaks = "2 month", name = "Fecha")+
  theme(legend.position="bottom", legend.key.size=unit(1.2,"cm"),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13))

ggplot(borrador[borrador$Fecha>="2019-12-20",], aes(x=Fecha, y=value, colour=variable, group=variable))+
  geom_line(size=1)+
  scale_y_continuous(name = "Tipo de cambio", labels = scales::dollar)+
  scale_x_date(date_breaks = "month", name = "Fecha")+
  theme(legend.position="bottom", legend.key.size=unit(1.2,"cm"),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13))

ggplot(brechas_ofi[brechas_ofi$Fecha>="2019-12-20",], aes(x=Fecha, y=value, colour=variable, group=variable))+
  geom_line(size=1)+
  scale_y_continuous(name = "Tipo de cambio", labels = scales::percent)+
  scale_x_date(date_breaks = "2 month", name = "Fecha")+
  geom_smooth(method = "loess", size=2)+ 
  theme(legend.position="bottom", legend.key.size=unit(1.2,"cm"),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13))

ggplot(brechas_tur[brechas_tur$Fecha>="2019-12-20",], aes(x=Fecha, y=value, colour=variable, group=variable))+
  geom_line(size=1)+
  scale_y_continuous(name = "Tipo de cambio", labels = scales::percent)+
  scale_x_date(date_breaks = "month", name = "Fecha")+
  theme(legend.position="bottom", legend.key.size=unit(1.2,"cm"),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13))

grangertest(Oficial ~ Turista, order = 1, data = data)
grangertest(Turista ~ Oficial, order = 1, data = data) #Ni oficial ni turista se causan

grangertest(Oficial ~ Blue, order = 1, data = data)
grangertest(Blue ~ Oficial, order = 1, data = data) #Ni oficial ni Blue se causan

grangertest(Oficial ~ MEP, order = 1, data = data)
grangertest(MEP ~ Oficial, order = 1, data = data) #Ni oficial ni MEP se causan

grangertest(Oficial ~ Ccl, order = 1, data = data)
grangertest(Ccl ~ Oficial, order = 1, data = data) #Ni oficial ni Ccl se causan

grangertest(Turista ~ Blue, order = 1, data = data)
grangertest(Blue ~ Turista, order = 1, data = data) #Turista anticipa blue

grangertest(Turista ~ MEP, order = 1, data = data)
grangertest(MEP ~ Turista, order = 1, data = data) #Turista anticipa MEP

grangertest(Turista ~ Ccl, order = 1, data = data)
grangertest(Ccl ~ Turista, order = 1, data = data) #Turista anticipa Ccl

grangertest(MEP ~ Blue, order = 1, data = data)
grangertest(Blue ~ MEP, order = 1, data = data) #MEP anticipa blue

grangertest(Ccl ~ Blue, order = 4, data = data)
grangertest(Blue ~ Ccl, order = 1, data = data) #ccl anticipa blue

grangertest(MEP ~ Ccl, order = 1, data = data)
grangertest(Ccl ~ MEP, order = 1, data = data) #MEP anticipa ccl

grangertest(Oficial ~ Blue, order = 3, data = database)
grangertest(Blue ~ Oficial, order = 4, data = database) #Oficial anticipa blue (en el LP)




#######raiz unitaria


df.test <- ur.df(data$MEP, type=c("drift"),lags=0)
summary(df.test)
df.test <- ur.df(diff(data$MEP), type=c("drift"),lags=0)
summary(df.test) #El test ADF se indica raiz unitaria para MEP

df.test <- ur.df(data$Blue, type=c("drift"),lags=0)
summary(df.test)
df.test <- ur.df(diff(data$Blue), type=c("drift"),lags=0)
summary(df.test) #El test ADF se indica raiz unitaria para Blue

df.test <- ur.df(data$Ccl, type=c("drift"),lags=0)
summary(df.test)
df.test <- ur.df(diff(data$Ccl), type=c("drift"),lags=0)
summary(df.test) #El test ADF se indica raiz unitaria para CCl

df.test <- ur.df(data$Turista, type=c("drift"),lags=0)
summary(df.test)
df.test <- ur.df(diff(data$Turista), type=c("drift"),lags=0)
summary(df.test) #El test ADF se indica raiz unitaria para Turista

df.test <- ur.df(data$Oficial, type=c("drift"),lags=0)
summary(df.test)
df.test <- ur.df(diff(data$Oficial), type=c("drift"),lags=0)
summary(df.test) #El test ADF se indica raiz unitaria para Oficial


######### VEC
data[,c(2:6)] <- apply(data[,c(2:6)], 2, log)#logaritmos



###Turista ~ BLUE
library("MTS")
database$Turista <- ifelse(is.na(database$Turista)==T, database$Oficial, database$Turista)
#database <- arrange(database, desc(Fecha))
borrador <- database[c(database$Fecha<"2020-10-30" & database$Fecha> "2015-12-31"),c(3,6)]
borrador <- apply(borrador, 2, log)
borrador <- as.data.frame(na.omit(borrador))
#borrador <- data[c(data$Fecha<"2020-11-01" & data$Fecha>"2019-06-31"),c(3,6)]

#Ecuación de cointegración
coinbluetur <- lm(data = borrador, Turista~Blue)
summary(coinbluetur)

#Orden de cointegracion
borrador <- ts(borrador, frequency = 7, start = c(2018, 300))
VARorder(borrador,maxp=10)
library(vars)
vecbluemep <- ca.jo(borrador, type="trace",spec="longrun",ecdet="none",K=2)
summary(vecbluemep) #No se rechaza la inexistencia de cointegración, pero es significativa

#estimacion vecm
vecbluemep<-cajorls(vecbluemep,r=1)
summary(vecbluemep$rlm)
#En c(database$Fecha<"2020-10-30" & database$Fecha> "2015-12-31"), Turista WExogeno, Blue acomoda hacia arriba

halflife <- as.data.frame(vecbluemep[["rlm"]][["coefficients"]])
halflife <- log(2)/abs(halflife[1,])
halflife



###Turista ~ MEP
library("MTS")
borrador <- data[c(data$Fecha<"2020-11-01" & data$Fecha>"2019-09-01"),c(3,4)]

#Ecuación de cointegración
coinbluetur <- lm(data = borrador, Turista~MEP)
summary(coinbluetur)

#Orden de cointegracion
borrador <- ts(borrador, frequency = 7, start = c(2018, 300))
VARorder(borrador,maxp=10)
library(vars)
vecbluemep <- ca.jo(borrador, type="eigen",spec="longrun",ecdet="trend",K=5)
summary(vecbluemep) #No se rechaza la inexistencia de cointegración, pero es significativa

#estimacion vecm
vecbluemep<-cajorls(vecbluemep,r=1)
summary(vecbluemep$rlm)

halflife <- as.data.frame(vecbluemep[["rlm"]][["coefficients"]])
halflife <- log(2)/abs(halflife[1,])
halflife



###Turista ~ BLUE y MEP
library("MTS")
borrador <- data[c(data$Fecha<"2020-11-01" & data$Fecha>"2019-09-01"),c(3,6,4)]

#Ecuación de cointegración
coinbluetur <- lm(data = borrador, Turista~Blue+MEP)
summary(coinbluetur)

#Orden de cointegracion
borrador <- ts(borrador, frequency = 7, start = c(2018, 300))
VARorder(borrador,maxp=10)
library(vars)
vecbluemep <- ca.jo(borrador, type="eigen",spec="longrun",ecdet="const",K=2)
summary(vecbluemep) #No se rechaza la inexistencia de cointegración, pero es significativa

#estimacion vecm
vecbluemep<-cajorls(vecbluemep,r=1)
summary(vecbluemep$rlm)


###BLue ~ MEP
library("MTS")
borrador <- data[c(data$Fecha<"2020-11-01" & data$Fecha>"2019-09-01"),c(4,6)]

#Ecuación de cointegración
coinbluetur <- lm(data = borrador, MEP~Blue)
summary(coinbluetur)

#Orden de cointegracion
borrador <- ts(borrador, frequency = 7, start = c(2018, 300))
VARorder(borrador,maxp=10)
library(vars)
vecbluemep <- ca.jo(borrador, type="eigen",spec="longrun",ecdet="none",K=2)
summary(vecbluemep) #No se rechaza la inexistencia de cointegración, pero es significativa

#estimacion vecm
vecbluemep<-cajorls(vecbluemep,r=1)
summary(vecbluemep$rlm)
#EL MEP es WE, el BLue ajusta al alza sobre el desvio. 



###Brecha BLue MEP
library("MTS")
library("tidyr")
borrador <- brechas_tur 


#Ecuación de cointegración
coinbluetur <- lm(data = borrador, blue_tur~mep_tur)
summary(coinbluetur)

#Orden de cointegracion
borrador <- ts(borrador[,c(3,4)], frequency = 7, start = c(2018, 300))
VARorder(borrador,maxp=10)
library(vars)
vecbluemep <- ca.jo(borrador, type="eigen",spec="longrun",ecdet="trend",K=2)
summary(vecbluemep) #No se rechaza la inexistencia de cointegración, pero es significativa

#estimacion vecm
vecbluemep<-cajorls(vecbluemep,r=1)
summary(vecbluemep$rlm)
#EL MEP es WE, el BLue ajusta al alza sobre el desvio. 

halflife <- as.data.frame(vecbluemep[["rlm"]][["coefficients"]])
halflife <- log(2)/abs(halflife[1,])
halflife
