library(dplyr)
library(stringr)
library(forecast)
library(ggplot2)
library(reshape2)
library(lubridate)
library(ineq)
library(lmtest)
library(urca)

mep <- read.delim2("P:/Facultad/macro 3/trabajo final/data/dolar mep.txt")
blue <- read.delim2("P:/Facultad/macro 3/trabajo final/data/dolar blue.txt")
tasa <- read.delim2("P:/Facultad/macro 3/trabajo final/data/tasa.txt")
reservas <- read.delim2("P:/Facultad/macro 3/trabajo final/data/reservas.txt")
merval <- read.delim2("P:/Facultad/macro 3/trabajo final/data/merval.txt")
BM <- read.delim2("P:/Facultad/macro 3/trabajo final/data/BM.txt")

blue <- blue[,c(1,3)]
mep <- mep[,c(1,2)]
merval <- merval[,c(1,3)]
colnames(merval) <- c("Fecha", "merval")
tasa <- tasa[,c(1,2)]
colnames(tasa) <- c("Fecha", "tasa")

BM$Fecha <- as.character(BM$Fecha)
BM$Fecha <- ifelse(substr(BM$Fecha,1,2)=="1-", paste("01-", substr(BM$Fecha, 3,20), sep = ""),BM$Fecha)
BM$Fecha <- ifelse(substr(BM$Fecha,1,2)=="2-", paste("02-", substr(BM$Fecha, 3,20), sep = ""),BM$Fecha)
BM$Fecha <- ifelse(substr(BM$Fecha,1,2)=="3-", paste("03-", substr(BM$Fecha, 3,20), sep = ""),BM$Fecha)
BM$Fecha <- ifelse(substr(BM$Fecha,1,2)=="4-", paste("04-", substr(BM$Fecha, 3,20), sep = ""),BM$Fecha)
BM$Fecha <- ifelse(substr(BM$Fecha,1,2)=="5-", paste("05-", substr(BM$Fecha, 3,20), sep = ""),BM$Fecha)
BM$Fecha <- ifelse(substr(BM$Fecha,1,2)=="6-", paste("06-", substr(BM$Fecha, 3,20), sep = ""),BM$Fecha)
BM$Fecha <- ifelse(substr(BM$Fecha,1,2)=="7-", paste("07-", substr(BM$Fecha, 3,20), sep = ""),BM$Fecha)
BM$Fecha <- ifelse(substr(BM$Fecha,1,2)=="8-", paste("08-", substr(BM$Fecha, 3,20), sep = ""),BM$Fecha)
BM$Fecha <- ifelse(substr(BM$Fecha,1,2)=="9-", paste("09-", substr(BM$Fecha, 3,20), sep = ""),BM$Fecha)

BM$Fecha <- paste(substr(BM$Fecha,1,6), "20", substr(BM$Fecha,7,8), sep = "")


reservas$Fecha <- substr(reservas$Fecha,7,16)
reservas$Reservas <-  substr(reservas$Reservas, 9, 21)
reservas$Reservas <- gsub(".", "", reservas$Reservas, fixed = T )
reservas$Reservas <- gsub(",", ".", reservas$Reservas, fixed = T )
reservas$Reservas <- as.numeric(reservas$Reservas)

merval$merval <- gsub(".", "", merval$merval, fixed = T)
merval$merval <- gsub(",", ".", merval$merval, fixed = T)
merval$merval <- as.numeric(merval$merval)

data <- blue
data <- merge(data, mep, by="Fecha", all.x = T)
data <- merge(data, BM, by="Fecha", all.x = T)
data <- merge(data, merval, by="Fecha", all.x = T)
data <- merge(data, tasa, by="Fecha", all.x = T)
data <- merge(data, reservas, by="Fecha", all.x = T)

data$Fecha <- as.character(data$Fecha)
data$Fecha <- as.Date(data$Fecha, format = '%d-%m-%Y')
data <- arrange(data, data$Fecha)
database <- data
data <- data[is.na(data$MEP)==F,]
data <- na.omit(data)

borrador <- data
borrador[,c(2:7)] <- apply(borrador[,c(2:7)], 2, log)
borrador <- borrador[borrador$Fecha>"2019-09-01",]

#vecm
library("MTS")
coinbluetur <- lm(data = borrador, Blue~MEP+BM+merval+tasa+Reservas)
summary(coinbluetur)

borrador <- ts(borrador[,c(2:6)], frequency = 7, start = c(2018, 300))
VARorder(borrador,maxp=10)
library(vars)
vecbluemep <- ca.jo(borrador, type="eigen",spec="longrun",ecdet="none",K=2)
summary(vecbluemep) #No se rechaza la inexistencia de cointegración, pero es significativa

#estimacion vecm
vecbluemep<-cajorls(vecbluemep,r=1)
summary(vecbluemep$rlm)

