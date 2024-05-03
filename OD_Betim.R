
#LIBRARIES
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(sp)
library(terra)
library(geosphere)

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
GPS<- GPS %>% dplyr::select(Veículo,`Data da Posição`,`Grade de Operação`,...6,Latitude,Longitude)
colnames(GPS)<-c("VEIC","DATA_HORA","LINHA","SENTIDO","LATITUDE","LONGITUDE")
GPS<-filter(GPS,!is.na(GPS$VEIC))

#PED
colnames(PED)<-c("PED_COD","DESC","LATITUDE","LONGITUDE","ROUTES")
PED<-separate(PED,ROUTES, into=paste0("ROUTES",1:100),sep=',')
PED<-pivot_longer(PED,cols=colnames(PED[,5:ncol(PED)]),values_to = "LINHA")
PED$name<-NULL             
PED<-filter(PED,!is.na(PED$LINHA))             
PED$LINHA<-gsub(' ','',PED$LINHA)
PED$LINHA<-as.character(PED$LINHA)
PED<-as.data.frame(PED)

#CRIANDO CHAVES PARA ASSOCIACAO NO EMBARQUE
GPS$CHAVE<-paste(GPS$VEIC,GPS$DATA_HORA)
GPS$CHAVE<-gsub(':','',GPS$CHAVE)
GPS$CHAVE<-gsub(' ','',GPS$CHAVE)
GPS$CHAVE<-gsub('-','',GPS$CHAVE)
GPS$CHAVE<-gsub('/','',GPS$CHAVE)
GPS$CHAVE<-as.numeric(GPS$CHAVE)
GPS<-arrange(GPS,VEIC,DATA_HORA)
SBE$CHAVE<-paste(SBE$VEIC_PROG,str_sub(SBE$DATA_UTILIZACAO,start=-4),str_sub(SBE$DATA_UTILIZACAO,start=4,end=5),str_sub(SBE$DATA_UTILIZACAO,end=2),SBE$HORA_UTILIZACAO)
SBE$CHAVE<-gsub(':','',SBE$CHAVE)
SBE$CHAVE<-gsub(' ','',SBE$CHAVE)
SBE$CHAVE<-gsub('-','',SBE$CHAVE)
SBE$CHAVE<-gsub('/','',SBE$CHAVE)
SBE$CHAVE<-gsub('A','',SBE$CHAVE)
SBE$CHAVE<-as.numeric(SBE$CHAVE)

##COMPATIBILIZANDO DIAS DE GPS COM O SBE E RETIRANDO VEÍCULOS COM FALHAS NA COMUNICAÇÃO
SBE<-filter(SBE,gsub('A','',SBE$VEIC_PROG) %in% GPS$VEIC)
SBE$DATAHORA_UTILIZACAO<-as.POSIXct(str_c(SBE$DATA_UTILIZACAO,' ',SBE$HORA_UTILIZACAO), format="%d/%m/%Y %H:%M:%S")
SBE<-filter(SBE,DATAHORA_UTILIZACAO>=min(GPS$DATA_HORA),DATAHORA_UTILIZACAO<=max(GPS$DATA_HORA))

### *** VERIFICAR A NECESSIDADE DE CORRIGIR O NÚMERO DE ORDEM DOS VEÍCULOS DO STPBC**
# FUNÇÃO PARA ASSOCIAR O CARTAO AO PED
associar_par_cod_siu <- function(SBE, GPS) {
  setDT(SBE)
  setDT(GPS)
  
  # Verificar valores faltantes
  if (any(is.na(SBE$CHAVE))) {
    stop("Valores faltantes na coluna CHAVE do dataset SBE.")
  }
  if (any(is.na(GPS$CHAVE))) {
    stop("Valores faltantes na coluna CHAVE do dataset GPS.")
  }
  
  setkey(SBE, CHAVE)
  setkey(GPS, CHAVE)
  indices_proximos <- GPS[SBE, roll = "nearest", on = "CHAVE"]$LINHA
  SBE$LINHA_GPS <- str_sub(indices_proximos,end=4)
  indices_proximos <- GPS[SBE, roll = "nearest", on = "CHAVE"]$SENTIDO
  SBE$SENTIDO <- str_sub(indices_proximos)
  indices_proximos <- GPS[SBE, roll = "nearest", on = "CHAVE"]$LATITUDE
  SBE$LATITUDE <- indices_proximos
  indices_proximos <- GPS[SBE, roll = "nearest", on = "CHAVE"]$LONGITUDE
  SBE$LONGITUDE<- indices_proximos
  return(SBE)
}

#ASSOCIANDO EMBARQUES
EMBARQUES <- associar_par_cod_siu(SBE, GPS)
EMBARQUES$COD_LINHA<-as.character(EMBARQUES$COD_LINHA)
EMBARQUES$CHAVE<-str_c(EMBARQUES$DATA_UTILIZACAO,EMBARQUES$VEIC_PROG,EMBARQUES$HORA_ABERTURA,sep="-")
EMBARQUES$LINHA_GPS<-gsub('-','',EMBARQUES$LINHA_GPS)
EMBARQUES$LINHA_GPS<-gsub(' ','',EMBARQUES$LINHA_GPS)

#ASSOCIANDO EMBARQUES COM PEDS
PED<-filter(PED,LINHA %in% EMBARQUES$LINHA_GPS,LINHA!="")

procura_ped_proximo<-function(x,y,z,w){
  return(which.min(sqrt(((z-x)^2)+((w-y)^2))))
}

Cod_PED<-0
PED2<-PED

for(j in 1:nrow(EMBARQUES)){
  PED2<-filter(PED,str_sub(LINHA,end=nchar(EMBARQUES$COD_LINHA[j]))==EMBARQUES$COD_LINHA[j])
  Cod_PED[j]<-procura_ped_proximo(EMBARQUES$LONGITUDE[j],EMBARQUES$LATITUDE[j],PED$LONGITUDE,PED$LATITUDE)
}

EMBARQUES$COD_PED<-PED$PED_COD[Cod_PED]
EMBARQUES$LONGITUDE<-PED$LONGITUDE[Cod_PED]
EMBARQUES$LATITUDE<-PED$LATITUDE[Cod_PED]

#CORRIGINDO LINHA GPS

for(j in 1:nrow(EMBARQUES)){
  if(EMBARQUES$LINHA_GPS[j]=="" && EMBARQUES$CHAVE[j]==EMBARQUES$CHAVE[j-1]){
    EMBARQUES$LINHA_GPS[j]==EMBARQUES$LINHA_GPS[j-1]
  }
}

#EMBARQUE_POR_PED_LINHA - SIU HORARIO DA VIAGEM
EMBARQUES$FX<-as.numeric(str_sub(EMBARQUES$HORA_ABERTURA,end=2))
EMBARQUES$EXPANSAO<-1
pivot<-EMBARQUES %>% dplyr::select(COD_PED,COD_LINHA,FX,EXPANSAO) %>% group_by(COD_PED,COD_LINHA,FX) %>% summarise(Passageiros=sum(EXPANSAO))
pivot$FX<-as.numeric(pivot$FX)
pivot<-data.frame(pivot)
h_24<-data.frame(cbind(rep(0,24),rep(0,24),c(0:23),rep(0,24)))
colnames(h_24)<-c("COD_PED","COD_LINHA","FX","Passageiros")
pivot<-rbind(h_24,pivot)
colnames(pivot)<-c("COD_PED","COD_LINHA","FX","Passageiros")
EMBARQUE_POR_PED<-spread(pivot,FX,Passageiros)
EMBARQUE_POR_PED$"Total Geral"<-EMBARQUE_POR_PED$"0"+EMBARQUE_POR_PED$"1"+EMBARQUE_POR_PED$"2"+EMBARQUE_POR_PED$"3"+EMBARQUE_POR_PED$"4"+EMBARQUE_POR_PED$"5"+EMBARQUE_POR_PED$"6"+EMBARQUE_POR_PED$"7"+EMBARQUE_POR_PED$"8"+EMBARQUE_POR_PED$"9"+EMBARQUE_POR_PED$"10"+EMBARQUE_POR_PED$"11"+EMBARQUE_POR_PED$"12"+EMBARQUE_POR_PED$"13"+EMBARQUE_POR_PED$"14"+EMBARQUE_POR_PED$"15"+EMBARQUE_POR_PED$"16"+EMBARQUE_POR_PED$"17"+EMBARQUE_POR_PED$"18"+EMBARQUE_POR_PED$"19"+EMBARQUE_POR_PED$"20"+EMBARQUE_POR_PED$"21"+EMBARQUE_POR_PED$"22"+EMBARQUE_POR_PED$"23"
PED2<-PED %>% dplyr::select(PED_COD,DESC,LATITUDE,LONGITUDE)
PED2<-unique(PED2)
EMBARQUE_POR_PED<-merge(EMBARQUE_POR_PED,PED2,by.x="COD_PED",by.y="PED_COD")
EMBARQUE_POR_PED<-EMBARQUE_POR_PED %>% dplyr::select(COD_PED,COD_LINHA,DESC,"0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23",`Total Geral`,LATITUDE,LONGITUDE)
EMBARQUE_POR_PED<-unique(EMBARQUE_POR_PED)
write.csv(EMBARQUE_POR_PED,"EMBARQUE_POR_PED_HrAbertura.csv",row.names = F)
