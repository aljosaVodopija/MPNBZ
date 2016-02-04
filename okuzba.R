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
poskus <-  read.delim("drobnica_stalez_3l.txt")  # dvakrat ista datoteka
premik_D <- read.table("drobnica_stalez_3l.txt", fill = TRUE)
premik_P <- read.table("prasici_stalez_3l.txt",fill = TRUE)
premik_G <- read.table("govedo_3leta.txt",fill = TRUE)
premik <- premik_D[3:length(premik_D[,1]),]
premik1 <- premik_P[3:length(premik_P[,1]),]
premik2 <- premik_G[5:length(premik_G[,1]),]
map <- get_map(location = 'Europe', zoom = 4)

## zemljevid slovenije
newmap <- getMap(resolution = "high")
europe.limits <- geocode.cache(c("Salovci", "Črnomelj", "Lendava", "Kobarid", "Bovec", "Trst"))





ou <- sapply(premik$V1, as.character)
obcine <- sapply(premik$V2, as.character)
#for(i in 1:length(obcine)){
#if(unlist(gregexpr("[0-9]+", obcine[i]))== 1){
#obcine[i] = ou[i]
#} 
#}


#unwanted_array = list('Š'='S', 'š'='s', 'Ž'='Z', "ž"= "z", "Č"="C", "č"="c")
#bu <- chartr(paste(names(unwanted_array), collapse=''),paste(unwanted_array, collapse=''),obcine)
obcine <- iconv(obcine, to="windows-1251")
stevilo <- premik$V7
stevilo <- data.matrix(stevilo)
bum <-iconv(obcine, to="windows-1251")
bum <- as.data.frame(table(bum))

drobnica <-  rep(0, length(bum$bum))

for(i in 1:length(bum$bum)){
  drobnica[i] = sum(na.omit(as.numeric(stevilo[which(obcine==bum$bum[i])])))
}



vidi <- bum$bum[which(is.na(drobnica))]



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


#lon <- x1[,1]
#lat <- x1[,2]
#lon[which(is.na(lon))] <- rep(ajd$lon, length(which(is.na(lon))))
#lat[which(is.na(lat))] <- rep(ajd$lat, length(which(is.na(lat))))

#a<-as.data.frame(table(lon))
#b<-as.data.frame(table(lat))
#d<-length(a[,1])
#for(i in 1:d){
#k = which(a[,2]==b[i,2])
#a[i,1] = a[k,1] 
#}
#a[,2] = b[,2]




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

ou1 <- sapply(premik1$V1, as.character)
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
ou2 <- sapply(premik2$V1, as.character)
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

cell.length <- 1000
premik2 <- data.matrix(premik2)
x.min <- min(as.numeric(premik2[,5]))#min(na.omit(lat2))
x.max <- max(as.numeric(premik2[,5]))#max(na.omit(lat2))
y.min <- min(as.numeric(premik2[,6]))#min(na.omit(lon2))
y.max <- max(as.numeric(premik2[,6]))#max(na.omit(lon2))
ncol <- round((x.max - x.min) / cell.length, 0)
nrow <- round((y.max - y.min) / cell.length, 0)
blank.grid <- raster(ncols=ncol, nrows=nrow, xmn=x.min, xmx=x.max, ymn=y.min, ymx=y.max)


land.grid = rasterize(cbind(lon2,lat2), blank.grid)
plot(land.grid)
plot(newmap,xlim= range(europe.limits$lon), ylim= range(europe.limits$lat), asp = 1)
points(lon2, lat2, pch=19, col="green", cex = as.numeric(bum2$Freq)/500)
lon[is.na(lon)] <- 0
lon1[is.na(lon1)] <- 0
lon2[is.na(lon2)] <- 0
lat[is.na(lat)] <- 0
lat1[is.na(lat1)] <- 0
lat2[is.na(lat2)] <- 0



load("zonalniVeter.txt")
load("meridionalniVeter.txt")
load("temperatura.txt")

# F =(X,Y) je vektorsko polje vetra
X = zonalniVeter[,,1] #zonalni veter v km/h (vzhod-zahod)
Y = meridionalniVeter[,,1] #meridionalni veter v km/h (jug-sever)
Tem = temperatura[,,1] # povprecna dnevna temperatura v kelvinih!!




load("matrikaNicel.txt")
#####################################





############################################
############ dinamicen del #################


A = c(13.5-1/90, (45.20+(1/30))-(1.5/120)) ## spodnje levo oglišče pravokotnika v katerega spravimo Slovenijo

D = c(16.5+1/90, 47+(1.5/120)) ## zgornje desno oglišče pravokotnika v katerega spravimo Slovenijo

x <- 1/90   ### premik v smeri x osi
y <- 1/120  ### premik v smeri y osi
dim <- c((D[1]-A[1])/x+1, (D[2]-A[2])/y+1)

vsi_x <- (0:(dim[1]-1))*x + A[1]
vsi_y <- (0:(dim[2]-1))*y + A[2]

##############################################
############ Parametra #######################
st_muh <- 900 ## stevilo muh na eno žival
gamma <- 0 ### stopnja natalitete muh, vstavi se načeloma kot vektor, lahko je tudi funkcija odvisna od temerature

################################################


muhe_z<- matrix(rep(0, dim[1]*dim[2]), dim[1], dim[2])
muhe_o<- matrix(rep(0, dim[1]*dim[2]), dim[1], dim[2])
okuzeni <- matrix(rep(0, dim[1]*dim[2]), dim[1], dim[2])

#######################################################
# če je več izbruhov, kopiramo tale del kode ###
######################################################
### Parametra naj bi bil mesto okuzbe - npr: Grosuplje
#####################################################
okuzba<- geocode.cache("Grosuplje") ### lokacija, na kateri nastane okužba

########### Paramerer-stevilo okuzenih ##############
stevilo_okuzenih <- 200 ### stevilo okuzenih na dani lokaciji
####################################################
x_koordinata <- which(abs(vsi_x-okuzba$lon)==min(abs(vsi_x-okuzba$lon)))
y_koordinata <- which(abs(vsi_y-okuzba$lat)==min(abs(vsi_y-okuzba$lat)))
okuzeni[x_koordinata, y_koordinata] <- stevilo_okuzenih
#################################################



### Govedo #### 
matrika <- matrix(rep(0, dim[1]*dim[2]), dim[1], dim[2])
muhe_o <- (okuzeni * st_muh) /0.9
n <- length(govedaTabela$lon2)
for(k in 1:n){
  prva <- which(abs(vsi_x-govedaTabela$lon2[k])==min(abs(vsi_x-govedaTabela$lon2[k])))
  druga <- which(abs(vsi_y-govedaTabela$lat2[k])==min(abs(vsi_y-govedaTabela$lat2[k])))
  matrika[prva, druga] <- (matrika[prva, druga] + (govedaTabela$stevilo[k])/ (0.9))
  muhe_z[prva, druga] <- (muhe_z[prva, druga] + (govedaTabela$stevilo[k]*st_muh)/ (0.9))  
}


divergenca <- DIV(X,Y)


#####################################################################################

##############################################################
############ Parametri ###########################
c_2 = 1 # verjetnost prenosa z okuženega goveda na zdravo muho 
c_3 = 0.001 ### gradient zdravih muh
c_4 = 0.01#.001#0.001 ### divergenca
c_5 = 10^(-6)#.05#0.1 ### laplace
c_6 = 0.001 ### gradient okuzenih muh
leto = 100### opazovalni cas okuzbe
b = 0.9 ## verjetnost prenosa okužbe
#######################################################


#####################################################################3
############################## SISTEM DIF: ENAČB-PREPISI!!!! ##################
for(i in 1:leto){
  
  gradient_z <-GRADF(muhe_z, X,Y)
  gradient_o <- GRADF(muhe_o, X,Y)
  laplace_z <- LAPLACE(muhe_z)
  laplace_o <- LAPLACE(muhe_o)
  muhe_z <- muhe_z + gamma * muhe_z -  c_2 *st_muh * okuzeni + c_3 * gradient_z + c_4*divergenca*muhe_z + c_5 *laplace_z
  muhe_o <- muhe_o + c_2 * st_muh* okuzeni + c_6* gradient_o + c_4*divergenca*muhe_o + c_5 * laplace_o
  muhe_z <- muhe_z * matrikaNicel
  muhe_o <- muhe_o * matrikaNicel
  
  matrika = matrika - (c_2/st_muh)*muhe_o
  okuzeni = okuzeni + (c_2/st_muh)*muhe_o

}

#################################################################
##################################################################
indeksi_x <- which(okuzeni > 0) %% dim(okuzeni>0)[1]
indeksi_y <- which(okuzeni > 0) %/% dim(okuzeni > 0)[1]
mapa_x <- vsi_x[indeksi_x]
mapa_y <- vsi_y[indeksi_y]



plot(newmap,xlim= range(europe.limits$lon), ylim= range(europe.limits$lat), asp = 1, main = "POSKUS")
points(mapa_x, mapa_y, pch=19, col="green", cex = 1)
