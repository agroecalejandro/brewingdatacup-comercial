
#########################################################3 Script para resolver el problema Comercial
rm(list = ls())

###################################### cargar librerias  ######################################
library(readr)
library(openair)
library(ggplot2)
library(dplyr)
library(reshape2)
library(ts)
library(tidyverse)
library(reshape2)
library(xts)
library(zoo)
library(ggplot2)
library(readr)
library(readxl)
library(openair)
library(forecast)
library(TSA)
library(dplyr)
library(data.table)
library("factoextra")

######################################################### Caragar datos #########################################################

dicc=readxl::read_xlsx("DemandForecast_Challenge.xlsx", 1)
datos=readxl::read_xlsx("DemandForecast_Challenge.xlsx", 2)
catal=readxl::read_xlsx("DemandForecast_Challenge.xlsx", 3)

colnames(datos)[1]="date"
colnames(datos)[2]="sub"
colnames(datos)[4]="hecto"

########################################################## exploracion de datos  ###################################################
############################ Graficas Exploratorias

ggplot(datos,aes(x=SKU,y=hecto, color=SKU))+
  geom_boxplot ()+
  theme_bw()+
  theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position="none")+
  theme(plot.title = element_text(lineheight=1,hjust = 0.5,size=18))+
  theme( axis.text.x  = element_text(angle=0,vjust=0.0, size=13),
         axis.text.y = element_text(size=13))

ggplot(datos,aes(x=hecto))+
  geom_histogram()+
  theme_bw()+
  theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position="none")+
  theme(plot.title = element_text(lineheight=1,hjust = 0.5,size=18))+
  theme( axis.text.x  = element_text(angle=0,vjust=0.0, size=13),
         axis.text.y = element_text(size=13))

# Exploracion de datos
# Se realizó una exploracion de datos
# Observamos que hay 3486 combinaciones  SKU  con subajencia que tienen una venta de 0 hectolitros 
# en el periodo de los datos, por lo cal se exclyó del análisis esa combinación
 
datos_suma=datos %>%
  dplyr::group_by(sub,SKU) %>%
  dplyr::summarise(sum=sum(hecto,na.rm=TRUE))

sum(datos_suma$sum==0)
datos_venta=datos_suma[datos_suma$sum!=0,]
sub_SKU_sin_venta = datos_suma[which(datos_suma$sum==0),]

######################################## Funcion para quitar eliminar el stock_out

stock_out=function(X)
{
  vec=rep(NA, nrow(X))
  for(i in 1:nrow(X))
  {
    if(sum(X$hecto[1:i])==0)
    {
      vec[i]=TRUE
      index_zero=sum(vec, na.rm = TRUE)
      
    }
    
  }
  X=X[(index_zero+1):nrow(X),]
  return(X)
}

###############################################################  pronosticar febrero  para Ajuste del modelo  ##################################

########################## Filtrar series de tiempos y convertirlas en formato ts
datos_1=list()

for(i in 1:nrow(datos_suma))
{
  
  ################ se extrae las combinacino a evaluar
  
  dato=datos[which(datos$sub == datos_suma$sub[i] & datos$SKU == datos_suma$SKU[i]),]
  dato=dato[-nrow(dato),]
  
  ###################### Eliminar las combinanciones de subagencias-SKU que su suma total sea cero
  
  if(sum(dato$hecto, na.rm = TRUE)==0)
  {
    next
  }
  
  if(!sum(dato$hecto[1], na.rm=TRUE)!=0)
  {
    dato=stock_out(dato)
  }
  
  start_year__dato=as.numeric(format(dato$date[1], "%Y"))
  start_month__dato=as.numeric(format(dato$date[1], "%m"))
  dato=ts(dato$hecto,start = c(start_year__dato,start_month__dato), frequency=12)
  nombre_i=paste(datos_suma$sub[i], datos_suma$SKU[i])
  datos_1[[nombre_i]]=dato
  
}

##################################### definir periodicidad

per=list()

for( i in 1:length(datos_2))
{
  p<-periodogram(datos_2[[i]],plot=FALSE)
  per[[i]]=data.table(period=1/p$freq, spec=p$spec)[order(-spec)][1:2]
  
}

######################################################################## definir modelo
model=list()

for(i in 1:length(datos_1))
  
{
  model[[i]]=auto.arima(datos_1[[i]] )
  
}

######################################################3 prediccion 
pred=list()
p=list()

for(i in 1:length(datos_1))
  
{
  pred[[i]]=forecast(model[[i]], h= 1 )
  p[[i]]=data.frame(Feb=pred[[i]]$mean[1],name=names(datos_1)[i])
  
}


p=do.call(rbind, p)

######################################################################## Armar base para cacular el error del mes
p$Feb_ventas= NA
datos_feb=selectByDate(datos,year=2018,month = 2)

for(i in 1:nrow(p))
{
  p$Feb_ventas[i]=datos_feb$hecto[which(datos_feb$sub == substr(p$name[i],1,5) & datos_feb$SKU == substr(p$name[i],7,14) )]
  
}

p$dif_abs=abs(p$Feb_ventas-p$Feb)

error_feb=sum(p$dif_abs)/sum(p$Feb_ventas)

#### exportar error de febrero para el reporte final
write.csv(error_feb,file = "error_feb.csv", row.names = FALSE)


################################################### Prdecir marzo, abril y Mayo #####################################################

########################## Filtrar series de tiempos y convertirlas en formato ts
datos_1=list()

for(i in 1:nrow(datos_suma))
{
  
  ################ se extrae la combinacion (SKU por cada subagencia)  a evaluar
  
  dato=datos[which(datos$sub == datos_suma$sub[i] & datos$SKU == datos_suma$SKU[i]),]
  
  ###################### Eliminar las combinanciones de subagencias-SKU que su suma total sea cero
  
  # Saltar SKU para la subagenca si todos los no hay vente en todo el periodo#
  if(sum(dato$hecto, na.rm = TRUE)==0)
  {
    next
  }
  
  # Si el primer valor de hectolitros vendidos es igual a 0, ejecutar la funcion que elimina
  # el stock out
  if(!sum(dato$hecto[1], na.rm=TRUE)!=0)
  {
    dato=stock_out(dato)
  } # Si el primer dato para los hectolitros vendidos para el SKU para la subagencia,
  # no ejecutar la funcion de calculo de stock out 
  
  #Extraer el anio y el mes de los datos de los sku para las subagencias
  start_year__dato=as.numeric(format(dato$date[1], "%Y"))
  start_month__dato=as.numeric(format(dato$date[1], "%m"))
  
  dato=ts(dato$hecto,start = c(start_year__dato,start_month__dato), frequency=12)
  nombre_i=paste(datos_suma$sub[i], datos_suma$SKU[i])
  datos_1[[nombre_i]]=dato
  
}

##################################### definir periodicidad de la serie ####

per=list()

for( i in 1:length(datos_2))
{
  p<-periodogram(datos_2[[i]],plot=FALSE)
  per[[i]]=data.table(period=1/p$freq, spec=p$spec)[order(-spec)][1:2]
  
}

############################ Dado que es ################################
model=list()

for(i in 1:length(datos_1))
  
{
  model[[i]]=auto.arima(datos_1[[i]] )
  
}

######################## prediccion ########################################
## enero y febrero
pred=list()
p=list()

for(i in 1:length(datos_1))
  
{
  pred[[i]]=forecast(model[[i]],h= 3 )
  p[[i]]=data.frame(Mar=pred[[i]]$mean[1],Apr=pred[[i]]$mean[2],May=pred[[i]]$mean[3],
                    name=names(datos_1)[i])
  
}


p=do.call(rbind, p)

########################## base de pronostico



###########################  summary
summa=list()

for(i in 1:length(datos_1))
  
{
  s=summary(model[[i]])
  res=data.frame(s,sd=model[[i]]$sigma2,mean_res=mean(model[[i]]$residuals))
  summa[[i]]=res
  
}

plot(pred[[2658]])

summa=do.call(rbind, summa)

ds_mean=mean(summa$mean_res, na.rm = TRUE)

############################################################# Exportar datos para el informe ###################################################
############### Datos para el analisis de SKU

p$sub=substr(1, 5, x=as.character(p$name))
p$sku=substr(6, 14, x=as.character(p$name))


sub_sum_sku=p %>%
  group_by(sku) %>%
  summarise(Mar=sum(Mar, na.rm = TRUE),
            Apr=sum(Apr, na.rm = TRUE),
            May=sum(May, na.rm = TRUE))


sub_sum$total=(sub_sum$Mar+ sub_sum$Apr+ sub_sum$May)
sub_sum_sku$total=(sub_sum_sku$Mar+ sub_sum_sku$Apr+ sub_sum_sku$May)


write.csv(sub_sum_sku, "subagencias.csv", row.names = FALSE)

################ Datos para generar la tabla de metodologia

error_feb=error_feb*100
error_may=mean(summa$mean_res, na.rm = TRUE)
herram="R, RStudio"
Info_ext="Shapefile de division politica de México, INEGI"
transfo_var="Euclidean distance, Cluster"

variables=c(as.character(round(error_feb,1)), 
            as.character(round(error_may, 1)),
            Info_ext,
            herram,
            transfo_var)

nombres_var=c("Error de febrero",
              "Error de mayo",
              "Herramientas utilizadas",
              "Informacion externa utilizada",
              "Transformacion de variables")

tabla_metodos=data.frame(nombres_var, variables)



### Datos para generar mapa de subagencias para los meses pronosticados 
### Asignar coordenadas a los datos

coords=catal[,-2]
sub_coords=merge(sub_sum, coords, by="sub")
sub_coords=melt(sub_coords, id=c("sub", "Latitud", "Longitud") )
colnames(sub_coords)[4]="mes"
coordinates(sub_coords)=~Longitud+Latitud
sub_coords@proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

estados=raster::shapefile("destdv250k_2gw.shp")

map=tm_shape(estados, space.color="blue") +
  tm_polygons(col="white") +
  tm_shape(sub_coords) +
  tm_bubbles(col="red", size="value") +
  tmap::tm_facets(by="mes") +
  tm_layout(legend.show = FALSE)




#################################################### clasificar SKUS  ###############################################################

datos_pronos=read.csv("meses_modelo.csv")
res.dist <- dist(datos_pronos[,-1], method = "euclidean")
res.hc <- hclust(d = res.dist,method = "complete")

fviz_dend(res.hc, k = 8, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


######################################  validacion del dendrograma

res.coph <- cophenetic(res.hc)
cor(res.dist, res.coph)

########################################  Promedio por grupos

res.hc$order
res.hc$cluster=c(rep(1,2),rep(2,3),rep(3,6),rep(4,9),rep(5,118),rep(6,4),rep(7,1),rep(8,2))
clus=data.frame(trat=res.hc$order,clus=res.hc$cluster)
clus=clus[order(clus$trat),]

mean_grup=aggregate(datos_pronos[,-1], by=list(cluster=clus$clus), mean)
total_group=colSums(mean_grup[,-1])

for(i in 1:nrow(mean_grup))
{
  mean_grup[i,-1]=round( (mean_grup[i,-1] *100)/total_group ,2)
  
}


colSums(mean_grup[,-1])





