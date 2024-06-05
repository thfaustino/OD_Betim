options(scipen = 999)
start<-Sys.time()

#LIBRARIES
library(plyr)
library(stringr)
library(readxl)
library(dplyr)
library(sf)

#CARREGANDO ARQUIVOS
setwd('D:/BETIM/OD/01_DADOS_BRUTOS')
setwd('SBE')
myfiles = list.files(path=getwd(),pattern="*.csv",full.names=TRUE)
SBE = ldply(myfiles, read.csv)
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

#GRADE HEXAGONAL
setwd('LIMITE')
LIM<-sf::st_read('BETIM.shp')
extensao<-sf::st_bbox(LIM)
grid<-sf::st_make_grid(extensao,cellsize = .005, square=F,crs=4326)
grid<-sf::st_as_sf(grid,crs=4326)
grid$ID_GRID<-1:nrow(grid)
setwd('..')

#BAIRROS
setwd('BAIRROS')
myfiles = list.files(path=getwd(),pattern="*.kml",full.names=TRUE)
BAIRRO<-st_read(myfiles[1],crs=4326)
BAIRRO<-st_zm(BAIRRO, drop = TRUE, what = "ZM")
geometries <- st_geometry(BAIRRO)

# Filtrar apenas as geometrias que são POLYGON ou MULTIPOLYGON
polygons <- lapply(geometries, function(geom) {
  if (inherits(geom, "GEOMETRYCOLLECTION")) {
    st_collection_extract(geom, "POLYGON")
  } else if (inherits(geom, "POLYGON") || inherits(geom, "MULTIPOLYGON")) {
    geom
  } else {
    NULL
  }
})

polygons <- polygons[!sapply(polygons, is.null)]
polygons_sf <- st_sfc(polygons, crs = st_crs(BAIRRO))
polygons_sf <- st_sf(geometry = polygons_sf)
BAIRRO$Description<-NULL
BAIRRO$geometry<-polygons_sf$geometry
rm(polygons_sf,polygons)
invalid_geom <- st_is_valid(BAIRRO)
BAIRRO[invalid_geom == T, ]
BAIRRO <- st_make_valid(BAIRRO)
rm(geometries,invalid_geom)

#GPS
setwd('GPS')
myfiles = list.files(path=getwd(),pattern="*.xls*",full.names=TRUE)
SBE2<-SBE

associar_par_cod_siu <- function(SBE, GPS) {
  data.table::setDT(SBE)
  data.table::setDT(GPS)
  
  # Verificar valores faltantes
  if (any(is.na(SBE$CHAVE))) {
    stop("Valores faltantes na coluna CHAVE do dataset SBE.")
  }
  if (any(is.na(GPS$CHAVE))) {
    stop("Valores faltantes na coluna CHAVE do dataset GPS.")
  }
  
  data.table::setkey(SBE, CHAVE)
  data.table::setkey(GPS, CHAVE)
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

for(j in 1:length(myfiles)){
  
  SBE<-SBE2
  GPS = as.data.frame(read_excel(myfiles[j]))
  GPS<-GPS %>% dplyr::select(Veículo,`Data da Posição`,`Grade de Operação`,...6,Latitude,Longitude)
  colnames(GPS)<-c("VEIC","DATA_HORA","LINHA","SENTIDO","LATITUDE","LONGITUDE")
  GPS<-dplyr::filter(GPS,!is.na(GPS$VEIC))
  GPS$DATA_HORA<-as.character(GPS$DATA_HORA)
  GPS<-as.data.frame(GPS)
  
  #CRIANDO CHAVES PARA ASSOCIACAO NO EMBARQUE
  GPS$CHAVE<-str_c(GPS$VEIC,GPS$DATA_HORA)
  GPS$CHAVE<-gsub(':','',GPS$CHAVE)
  GPS$CHAVE<-gsub(' ','',GPS$CHAVE)
  GPS$CHAVE<-gsub('-','',GPS$CHAVE)
  GPS$CHAVE<-gsub('/','',GPS$CHAVE)
  GPS$CHAVE<-as.numeric(GPS$CHAVE)
  GPS<-GPS[!duplicated(GPS$CHAVE),]
  GPS<-arrange(GPS,CHAVE)
  SBE$DATAHORA_UTILIZACAO<-as.POSIXct(str_c(SBE$DATA_UTILIZACAO,' ',SBE$HORA_UTILIZACAO), format="%d/%m/%Y %H:%M:%S")
  SBE<-dplyr::filter(SBE,gsub('A','',SBE$VEIC_PROG) %in% GPS$VEIC)
  SBE$CHAVE<-str_c(SBE$VEIC_PROG,str_sub(SBE$DATA_UTILIZACAO,start=-4),str_sub(SBE$DATA_UTILIZACAO,start=4,end=5),str_sub(SBE$DATA_UTILIZACAO,end=2),SBE$HORA_UTILIZACAO)
  SBE$CHAVE<-gsub(':','',SBE$CHAVE)
  SBE$CHAVE<-gsub(' ','',SBE$CHAVE)
  SBE$CHAVE<-gsub('-','',SBE$CHAVE)
  SBE$CHAVE<-gsub('/','',SBE$CHAVE)
  SBE$CHAVE<-gsub('A','',SBE$CHAVE)
  SBE$CHAVE<-as.numeric(SBE$CHAVE)
  SBE<-dplyr::filter(SBE,!is.na(CHAVE))
  
  ##COMPATIBILIZANDO DIAS DE GPS COM O SBE E RETIRANDO VEÍCULOS COM FALHAS NA COMUNICAÇÃO
  SBE<-dplyr::filter(SBE,DATAHORA_UTILIZACAO>=min(GPS$DATA_HORA),DATAHORA_UTILIZACAO<=max(GPS$DATA_HORA))

  #ASSOCIANDO EMBARQUES
  EMBARQUES2 <- associar_par_cod_siu(SBE, GPS)
  if(j==1){
    EMBARQUES<-EMBARQUES2
    GPS2<-GPS
  }else{
    EMBARQUES<-rbind(EMBARQUES,EMBARQUES2)
    GPS2<-rbind(GPS2,GPS)
  }
  print(str_c(j," de ",length(myfiles)," finalizado."))
}

#SALVANDO EMBARQUES
setwd('..')
setwd('..')
setwd('02_RESULTADOS')
write.csv2(EMBARQUES,'EMBARQUES.csv',row.names = F)
setwd('..')
rm(j,EMBARQUES2,GPS,SBE,SBE2,myfiles,associar_par_cod_siu)

### TRABALHANDO COM DADOS DE PESQUISA SOBE E DESCE COM SENHA
#CARTAO
EMBARQUES$ID_val<-c(1:nrow(EMBARQUES))
VAL<-EMBARQUES %>% dplyr::select(ID_val,LATITUDE,LONGITUDE)
VAL<-sf::st_as_sf(VAL,coords=c("LONGITUDE","LATITUDE"),crs=4326)
centroides<-st_centroid(grid)
centroides<-st_join(centroides,BAIRRO)
grid$NM_BAIRRO_EMB<-centroides$Name
VAL<-sf::st_join(VAL,grid)
VAL<-as.data.frame(VAL)
EMBARQUES$ID_grid<-VAL$ID_GRID
rm(VAL,extensao)

#RETIRANDO VALE SOCIAL E DINHEIRO (CARTÕES COM DESEMBARQUE NÃO RASTREÁVEIS)
OD<-EMBARQUES %>% dplyr::select(CARTAO,TP_USUARIO,DATA_UTILIZACAO,HORA_UTILIZACAO,COD_LINHA,VEIC_PROG,HORA_ABERTURA,SENTIDO,VEIC_PROG,ID_grid)
OD<-arrange(OD,CARTAO,DATA_UTILIZACAO,HORA_UTILIZACAO)
DIN<-rbind(dplyr::filter(OD,CARTAO=='0'),dplyr::filter(OD,TP_USUARIO=='VALE SOC'))
OD<-dplyr::filter(OD,CARTAO!=0)
OD<-dplyr::filter(OD,TP_USUARIO!='VALE SOC')

#PRIMEIRA VALIDAÇÃO DO DIA
OD$CARTAO_DIA<-str_c(OD$CARTAO,OD$DATA_UTILIZACAO)
val1<-OD %>% dplyr::select(CARTAO_DIA,ID_grid)
val1<-val1[!duplicated(val1$CARTAO_DIA),]
colnames(val1)<-c("CARTAO_DIA","VAL1")
OD<-merge(OD,val1,all.x=T)
rm(val1)
OD<-arrange(OD,CARTAO,DATA_UTILIZACAO,HORA_UTILIZACAO)

#SEGUNDA VALIDAÇÃO: PROXIMAS VALIDAÇÕES COM INTERVALO MENOR DO QUE 5 MINUTOS E NO MESMO DIA
OD$ID_DESTINO <- OD$VAL1
indices <- (OD$CARTAO_DIA[-1] == OD$CARTAO_DIA[-nrow(OD)]) & (as.POSIXct(OD$HORA_UTILIZACAO[-1], format = "%H:%M")-as.POSIXct(OD$HORA_UTILIZACAO[-nrow(OD)], format = "%H:%M")<300)
indices <- which(indices)
DIN<-rbind(DIN,OD[indices+1,] %>% dplyr::select(colnames(DIN)))
indices<-indices+1
OD<-OD[-indices,]
indices <- (OD$CARTAO_DIA[-1] == OD$CARTAO_DIA[-nrow(OD)]) & (as.POSIXct(OD$HORA_UTILIZACAO[-1], format = "%H:%M")-as.POSIXct(OD$HORA_UTILIZACAO[-nrow(OD)], format = "%H:%M")>=300)
indices <- which(indices)
OD$ID_DESTINO[indices]<-OD$ID_grid[indices+1]
rm(indices)

#VALIDACOES NA MESMA GRADE
DIN<-rbind(DIN,dplyr::filter(OD,ID_DESTINO==ID_grid) %>% dplyr::select(colnames(DIN)))
OD<-filter(OD,ID_DESTINO!=ID_grid)
OD$VAL1<-NULL
OD$CARTAO_DIA<-NULL

#VALIDACOES DESTINO - IDENTIFICAR PADRÃO
setwd('01_DADOS_BRUTOS/GPS')
myfiles = list.files(path=getwd(),pattern="*.xls*",full.names=TRUE)
centroides<-st_centroid(grid)
centroides<-st_join(centroides,BAIRRO)
OD$DATA_HORA<-str_c(OD$DATA_UTILIZACAO,OD$HORA_UTILIZACAO,sep=' ')
OD$DATA_HORA<-as.POSIXct(OD$DATA_HORA, format="%d/%m/%Y %H:%M:%S",tz='UTC')
OD<-merge(OD,centroides,by.x="ID_DESTINO",by.y = "ID_GRID")
OD<-st_as_sf(OD,crs=4326)
OD$VEIC_PROG<-gsub('A','',OD$VEIC_PROG)

for(j in 1:length(myfiles)){
  GPS<-read_excel(myfiles[j])  %>% dplyr::select(Veículo,`Data da Posição`,Latitude,Longitude)
  colnames(GPS)<-c("VEIC","DATA_HORA","LATITUDE","LONGITUDE")
  GPS<-GPS[-1,]
  GPS<-as.data.frame(GPS)
  GPS$DATA_HORA<-as.POSIXct(GPS$DATA_HORA, format="%Y-%m-%d %H:%M:%S")
  GPS<-arrange(GPS,VEIC,DATA_HORA)
  GPS<-sf::st_as_sf(GPS,coords=c("LONGITUDE","LATITUDE"),crs=4326)
  GPS<-sf::st_join(GPS,grid)
  OD2<-filter(OD,VEIC_PROG==GPS$VEIC[1])
  OD2<-filter(OD2,OD2$DATA_HORA<=max(GPS$DATA_HORA))
  OD2<-filter(OD2,OD2$DATA_HORA>=min(GPS$DATA_HORA))
  i<-1
  for(i in 1:nrow(OD2)){
    GPS2<-dplyr::filter(GPS,GPS$DATA_HORA>OD2$DATA_HORA[i],GPS$DATA_HORA<=OD2$DATA_HORA[i]+7200)
    dist<-st_distance(OD2[i,],GPS2)
    indice<-which(dist==min(dist))
    indice<-indice[1]
    OD2$ID_DESEMBARQUE[i]<-GPS2$ID_GRID[indice]
    OD2$HORA_DESEMBARQUE[i]<-GPS2$DATA_HORA[indice]
    OD2$DIST_DES[i]<-min(dist)
  }
  if(j==1){
    SDCS<-OD2
  }else{
    SDCS<-rbind(SDCS,OD2)
  }
  print(str_c(gsub('.xls','',basename(myfiles[j])),' concluido. Faltam ',length(myfiles)-j,' dados de veículos.'))
}

SDCS<-as.data.frame(SDCS)
SDCS$x<-NULL
val_nao_encont<-dplyr::filter(SDCS,is.na(ID_DESEMBARQUE))
SDCS<-dplyr::filter(SDCS,!is.na(ID_DESEMBARQUE))
SDCS<-SDCS[!duplicated(SDCS),]
Q1<-quantile(SDCS$DIST_DES,.25)
Q2<-quantile(SDCS$DIST_DES,.75)
IQR<-Q2-Q1
Lsup=Q2+(1.5*IQR)
val_nao_encont<-rbind(val_nao_encont,filter(SDCS,DIST_DES>Lsup))
SDCS<-filter(SDCS,DIST_DES<=Lsup)
OD<-SDCS
val_nao_encont<-val_nao_encont %>% dplyr::select(colnames(DIN))
DIN<-rbind(DIN,val_nao_encont)
rm(SDCS,IQR,Lsup,Q1,Q2)

#EXPANSÃO: DESTINO PARA CARTÕES SEM PAR O-D
DIN$CHAVE<-str_c(DIN$COD_LINHA,DIN$ID_grid,str_sub(DIN$HORA_UTILIZACAO,end=2),sep='-')
DIN$COUNTER<-1
OD$CHAVE<-str_c(OD$COD_LINHA,OD$ID_grid,str_sub(OD$HORA_UTILIZACAO,end=2),sep="-")
OD$COUNTER<-1
ODfe<-OD %>% dplyr::select(CHAVE,COUNTER) %>% dplyr::group_by(CHAVE) %>% dplyr::summarise(QTDod=sum(COUNTER))
DINfe<-DIN %>% dplyr::select(CHAVE,COUNTER) %>% dplyr::group_by(CHAVE) %>% dplyr::summarise(QTDo=sum(COUNTER))
FE1<-merge(ODfe,DINfe,all.x=T,all.y=T)
FE1[is.na(FE1)]<-0
FE1$FE1<-(FE1$QTDod+FE1$QTDo)/(FE1$QTDod)
OD<-merge(OD,FE1 %>% dplyr::select(CHAVE,FE1),all.x=T)
rm(ODfe,DINfe,FE1)

#EXPANSÃO: CARTÕES SEM PAR O/D E SEM CARTOES COM PAR OD NO HEXÁGONO NA VIAGEM
OD$CHAVE<-stringr::str_c(OD$COD_LINHA,OD$DATA_UTILIZACAO,OD$HORA_ABERTURA,sep="-")
DIN$CHAVE<-stringr::str_c(DIN$COD_LINHA,DIN$DATA_UTILIZACAO,DIN$HORA_ABERTURA,sep="-")
ODfe<-OD %>% dplyr::select(CHAVE,FE1) %>% dplyr::group_by(CHAVE) %>% dplyr::summarise(QTDod=sum(FE1))
DINfe<-DIN %>% dplyr::select(CHAVE,COUNTER) %>% dplyr::group_by(CHAVE) %>% dplyr::summarise(QTDo=sum(COUNTER))
FE2<-merge(ODfe,DINfe,all.x=T,all.y=T)
FE2$QTDo[is.na(FE2$QTDo)]<-0
FE2$FE2<-(FE2$QTDod+FE2$QTDo)/(FE2$QTDod)
OD<-merge(OD,FE2 %>% dplyr::select(CHAVE,FE2),all.x=T)

#EXPANSÃO: CARTÕES SEM PAR O/D E SEM CARTOES COM PAR OD NO HEXÁGONO NA LINHA
OD$CHAVE<-stringr::str_c(OD$COD_LINHA,OD$DATA_UTILIZACAO,sep="-")
EMBARQUES$COUNTER<-1
EMBARQUES$CHAVE<-stringr::str_c(EMBARQUES$COD_LINHA,EMBARQUES$DATA_UTILIZACAO,sep="-")
ODfe<-OD %>% dplyr::select(CHAVE,FE2) %>% dplyr::group_by(CHAVE) %>% dplyr::summarise(QTDod=sum(FE2))
DINfe<-EMBARQUES %>% dplyr::select(CHAVE,COUNTER) %>% dplyr::group_by(CHAVE) %>% dplyr::summarise(QTDo=sum(COUNTER))
FE4<-merge(ODfe,DINfe,all.x=T,all.y=T)
FE4$QTDo[is.na(FE4$QTDo)]<-0
FE4$FE4<-(FE4$QTDo)/(FE4$QTDod)
OD<-merge(OD,FE4 %>% dplyr::select(CHAVE,FE4),all.x=T)
OD$FE<-OD$FE4*OD$FE2
rm(EMBARQUES,DIN,DINfe,FE2,FE4,ODfe)
OD$CHAVE<-NULL
OD<-arrange(OD,COD_LINHA,DATA_UTILIZACAO,HORA_ABERTURA,HORA_UTILIZACAO)
OD$FE1<-NULL
OD$FE2<-NULL
OD$COUNTER<-NULL
OD$FE4<-NULL

#SALVANDO RESULTADOS
setwd('..')
setwd('..')
setwd('02_RESULTADOS')
write.csv2(OD,"SDCS.csv",row.names = F)
end=Sys.time()

## INDICES DE ROTATIVIDADE
OD<-arrange(OD,COD_LINHA,HORA_ABERTURA,DATA_HORA)
EMBARQUES<-OD %>% dplyr::select(DATA_UTILIZACAO,COD_LINHA,VEIC_PROG,HORA_ABERTURA,SENTIDO,ID_grid,DATA_HORA,FE)
DESEMBARQUES<-OD %>% dplyr::select(DATA_UTILIZACAO,COD_LINHA,VEIC_PROG,HORA_ABERTURA,SENTIDO,ID_DESEMBARQUE,HORA_DESEMBARQUE,FE)
DESEMBARQUES$FE=(-1)*DESEMBARQUES$FE
colnames(DESEMBARQUES)<-colnames(EMBARQUES)
OCUP<-rbind(EMBARQUES,DESEMBARQUES)
rm(DESEMBARQUES)
OCUP <- OCUP %>%  arrange(DATA_UTILIZACAO, COD_LINHA, HORA_ABERTURA, DATA_HORA) %>%  group_by(DATA_UTILIZACAO, HORA_ABERTURA, VEIC_PROG) %>%  mutate(OCUP = cumsum(FE)) %>%  ungroup()
EMBARQUES<-EMBARQUES %>% group_by(DATA_UTILIZACAO,COD_LINHA,VEIC_PROG,HORA_ABERTURA) %>% summarise(PASS_VG=sum(FE))
EMBARQUES$CHAVE<-str_c(EMBARQUES$VEIC_PROG,EMBARQUES$DATA_UTILIZACAO,EMBARQUES$HORA_ABERTURA)
OCUP$CHAVE<-str_c(OCUP$VEIC_PROG,OCUP$DATA_UTILIZACAO,OCUP$HORA_ABERTURA)
OCUP<-merge(OCUP,EMBARQUES %>% dplyr::select(CHAVE,PASS_VG),all.x=T)
PTC<-OCUP %>% group_by(CHAVE) %>% summarise(PTC=max(OCUP))
OCUP<-merge(OCUP,PTC,by.x="CHAVE",by.y="CHAVE")
OCUP$IR<-OCUP$PASS_VG/OCUP$PTC
write.csv2(OCUP,'IR.csv',row.names = F)

