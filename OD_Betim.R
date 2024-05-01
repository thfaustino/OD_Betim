
#LIBRARIES
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)

#CARREGANDO ARQUIVOS
setwd('GPS')
myfiles = list.files(path=getwd(),pattern="*.xls*",full.names=TRUE)
GPS = ldply(myfiles, read_excel)
setwd('..')
setwd('SBE')
myfiles = list.files(path=getwd(),pattern="*.csv",full.names=TRUE)
SBE = ldply(myfiles, read.csv)
setwd('..')
setwd('PED')
myfiles = list.files(path=getwd(),pattern="*.csv",full.names=TRUE)
PED = ldply(myfiles, read.csv, sep=";",)
setwd('..')
rm(myfiles)

#LIMPANDO ARQUIVOS
##SBE
SBE[,1]<-NULL

for(j in 1:ncol(SBE)){
  if(is.na(SBE[1,j]) & j!=ncol(SBE)){
    SBE[,j]<-SBE[,j+1]
 }
}
rm(j)

SBE$X.1<-NULL
SBE$X.2<-NULL
SBE$X.3<-NULL
SBE$X.4<-NULL
SBE$X.5<-NULL
SBE$X.6<-NULL
SBE$X.7<-NULL
SBE$X.8<-NULL
SBE$X.9<-NULL
SBE$X.10<-NULL
colnames(SBE)<-c("CARTAO","TP_USUARIO","DATA_UTILIZACAO","HORA_UTILIZACAO","LATITUDE","LONGITUDE","TP_PGTO","COD_LINHA","VEIC_PROG","VEIC_REAL","HORA_ABERTURA","VALOR_COBRADO")

#GPS
GPS<- GPS %>% dplyr::select(Veículo,`Data da Posição`,Latitude,Longitude)
colnames(GPS)<-c("VEIC","DATA_HORA","LATITUDE","LONGITUDE")
GPS<-filter(GPS,!is.na(GPS$VEIC))

#PED
colnames(PED)<-c("PED_COD","DESC","LATITUDE","LONGITUDE","ROUTES")
PED<-separate(PED,ROUTES, into=paste0("ROUTES",1:(2*length(summary(as.factor(SBE$COD_LINHA))))),sep=',')
PED<-pivot_longer(PED,cols=colnames(PED[,5:ncol(PED)]),values_to = "LINHA")
PED$name<-NULL             
PED<-filter(PED,!is.na(PED$LINHA))             
PED$LINHA<-gsub(' ','',PED$LINHA)

#CRIANDO CHAVES PARA ASSOCIACAO NO EMBARQUE
GPS$CHAVE<-paste(GPS$VEIC,GPS$DATA_HORA)
GPS$CHAVE<-gsub(':','',GPS$CHAVE)
GPS$CHAVE<-gsub(' ','',GPS$CHAVE)
GPS$CHAVE<-gsub('-','',GPS$CHAVE)
GPS$CHAVE<-as.numeric(GPS$CHAVE)
SBE$CHAVE<-


#rascunho
head(SBE)
GPS$LATITUDE