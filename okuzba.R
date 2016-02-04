# install.packages("ggmap")
# install.packages("ncdf4")
# install.packages("plotrix")
# install.packages("raster")
# install.packages("RCurl")
# install.packages("rworldmap")
library(ncdf4)
library(ggmap)
library(rworldmap)
library(RCurl)
library(RJSONIO)
library(plyr)
library(plotrix)
library(RgoogleMaps)
library(raster)

source("operatorji.R")
source("nastaviVeter.R")

geocode.cache <- function(mesta) {
  kode = read.csv('kode.csv', row.names = 1)
  for(mesto in mesta) {
    mesto = toupper(mesto)
    if(!mesto %in% rownames(kode)) {
      polozaj = geocode(paste(mesto, "SLOVENIJA"))
      kode[mesto, ] = polozaj
    }
  }
  write.csv(kode, 'kode.csv')
  return(kode[toupper(mesta), ])
}

####### naložimo podatke #####
premik_D <- read.table("drobnica_stalez_3l.txt", fill = TRUE)
premik_P <- read.table("prasici_stalez_3l.txt",fill = TRUE)
premik_G <- read.table("govedo_3leta.txt",fill = TRUE)
premik <- premik_D[3:length(premik_D[,1]),]
premik1 <- premik_P[3:length(premik_P[,1]),]
premik2 <- premik_G[5:length(premik_G[,1]),]

## zemljevid slovenije
newmap <- getMap(resolution = "high")
europe.limits <- geocode.cache(c("Salovci", "Črnomelj", "Lendava", "Kobarid", "Bovec", "Trst"))





obcine <- sapply(premik$V2, as.character)
obcine <- iconv(obcine, to="windows-1251")
stevilo <- premik$V7
stevilo <- data.matrix(stevilo)
bum <-iconv(obcine, to="windows-1251")
bum <- as.data.frame(table(bum))

drobnica <-  rep(0, length(bum$bum))

for(i in 1:length(bum$bum)){
  drobnica[i] = sum(na.omit(as.numeric(stevilo[which(obcine==bum$bum[i])])))
}






lon <- rep(0, length(bum$bum))
lat <- rep(0, length(bum$bum))

for(i in 1:length(bum$bum)){
  mesto <- bum$bum[i]
  lon[i] <- as.numeric(geocode.cache(mesto)$lon)
  lat[i] <- as.numeric(geocode.cache(mesto)$lat)
}



drobnicaTabela <- data.frame(lon = lon, lat = lat, stevilo = drobnica)
rownames(drobnicaTabela) <- as.character(bum$bum)
save(drobnicaTabela, file = 'drobnica.txt')





lim<- geocode.cache(c("Salovci", "Črnomelj", "Lendava", "Kobarid", "Bovec", "Trst"))

lonn <- c(min(lim$lon), max(lim$lon)) #define our map's ylim
latt <- c(min(lim$lat), max(lim$lat)) #define our map's xlim
markers = paste0("&markers=color:blue|label:S|latt[1],lonn[1]&markers=color:",
                 "green|label:G|latt[2], lonn[2]")
center = c(geocode.cache("Celje")$lat, geocode.cache("Ljubljana")$lon)  #tell what point to center on
zoom <- 8 
#### Drobnica grafični prikaz, razmerje 1:50
try(terrmap <- GetMap(center=center, zoom=zoom, markers=markers,maptype= "roadmap"), silent=TRUE)
png("drobnica.png", type='cairo-png')
tmp <- PlotOnStaticMap(terrmap, lat, lon, cex=as.numeric(bum$Freq)/50,pch=20,col="red")
dev.off()




plot(newmap,xlim= range(europe.limits$lon), ylim= range(europe.limits$lat), asp = 1)
points(lon, lat, pch=19, col="red", cex = as.numeric(bum$Freq)/200)


###########################################################################

obcine1 <- sapply(premik1$V2, as.character)


bum1 <-iconv(obcine1, to="windows-1251")
bum1 <- as.data.frame(table(bum1))
stevilo1 <- premik1$V7
stevilo1 <- data.matrix(stevilo1)

prasici <-  rep(0, length(bum1$bum1))

for(i in 1:length(bum1$bum1)){
  prasici[i] = sum(na.omit(as.numeric(stevilo1[which(obcine1==bum1$bum1[i])])))
}


lon1 <- rep(0, length(bum1$bum1))
lat1 <- rep(0, length(bum1$bum1))

for(i in 1:length(bum1$bum1)){
  mesto1 <- bum1$bum1[i]
  lon1[i] <- as.numeric(geocode.cache(mesto1)$lon)
  lat1[i] <- as.numeric(geocode.cache(mesto1)$lat)
  
}


prasiciTabela <- data.frame(lon = lon1, lat = lat1, stevilo = prasici)
rownames(prasiciTabela) <- as.character(bum1$bum1)
save(prasiciTabela, file = 'prasici.txt')

##### prasici.png je prikaz prasicev v Sloveniji, razmerje 1:250
try(terrmap1 <- GetMap(center=center, zoom=zoom, markers=markers,maptype= "roadmap"))
png("prasici.png", type='cairo-png')
tmp1 <- PlotOnStaticMap(terrmap, lat1, lon1, cex=as.numeric(bum1$Freq)/250,pch=20,col="blue")
dev.off()



plot(newmap,xlim= range(europe.limits$lon), ylim= range(europe.limits$lat), asp = 1)
points(lon1, lat1, pch=19, col="blue", cex = as.numeric(bum1$Freq)/700)


#########################################################################
obcine2 <- sapply(premik2$V2, as.character)

bum2 <-iconv(obcine2, to="windows-1251")
bum2 <- as.data.frame(table(bum2))
stevilo2 <- premik2$V7
stevilo2 <- data.matrix(stevilo2)

goveda <-  rep(0, length(bum2$bum2))

for(i in 1:length(bum2$bum2)){
  goveda[i] = sum(na.omit(as.numeric(stevilo2[which(obcine2==bum2$bum2[i])])))
}

lon2 <- rep(0, length(bum2$bum2))
lat2 <- rep(0, length(bum2$bum2))

for(i in 1:length(bum2$bum2)){
  mesto2 <- bum2$bum2[i]
  lon2[i] <- as.numeric(geocode.cache(mesto2)$lon)
  lat2[i] <- as.numeric(geocode.cache(mesto2)$lat)
  
}

govedaTabela <- data.frame(lon = lon2, lat = lat2, stevilo = goveda)
rownames(govedaTabela) <- as.character(bum2$bum2)
save(govedaTabela, file = 'goveda.txt')

try(terrmap2 <- GetMap(center=center, zoom=zoom, markers=markers,maptype= "roadmap"))
### Govedo-zemljevid, razmerje 1:150
png("govedo.png", type='cairo-png')
tmp2 <- PlotOnStaticMap(terrmap2, lat2, lon2, cex=as.numeric(bum2$Freq)/150,pch=20,col="purple")

dev.off()

plot(newmap,xlim= range(europe.limits$lon), ylim= range(europe.limits$lat), asp = 1)
points(lon2, lat2, pch=19, col="green", cex = as.numeric(bum2$Freq)/500)
lon[is.na(lon)] <- 0
lon1[is.na(lon1)] <- 0
lon2[is.na(lon2)] <- 0
lat[is.na(lat)] <- 0
lat1[is.na(lat1)] <- 0
lat2[is.na(lat2)] <- 0
