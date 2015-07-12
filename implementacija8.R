setwd("C:/projekt")
install.packages("ggmap")
install.packages("rworldmap")
library(ggmap)
library(rworldmap)
poskus <-  read.delim("C:/Projekt/drobnica_stalez_3l.txt")
premik_D <- read.table("C:/Projekt/drobnica_stalez_3l.txt", fill = TRUE)
premik_P <- read.table("C:/Projekt/prasici_stalez_3l.txt",fill = TRUE)
premik_G <- read.table("C:/Projekt/govedo_3leta.txt",fill = TRUE)
premik <- premik_D[3:length(premik_D[,1]),]
premik1 <- premik_P[3:length(premik_P[,1]),]
premik2 <- premik_G[5:length(premik_G[,1]),]
map <- get_map(location = 'Europe', zoom = 4)
newmap <- getMap(resolution = "high")
europe.limits <- geocode(c("Salovci", "Črnomelj", "Lendava", "Kobarid", "Bovec", "Trst"))





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
  lon[i] <- as.numeric(geocode(paste(mesto, "Slovenija"))$lon)
  lat[i] <- as.numeric(geocode(paste(mesto, "Slovenija"))$lat)
}
vec <-which(is.na(lon))
lon[vec[1]] <- geocode("DIVAČA, SLOVENIJA")$lon
lat[vec[1]] <- geocode("DIVAČA, SLOVENIJA")$lat
lon[vec[2]] <- geocode("ČRNA, SLOVENIJA")$lon
lat[vec[2]] <- geocode("ČRNA, SLOVENIJA")$lat
lon[vec[3]] <- geocode("LUČE, SLOVENIJA")$lon
lat[vec[3]] <- geocode("LUČE, SLOVENIJA")$lat
lon[vec[5]] <- geocode("REČICA, SLOVENIJA")$lon
lat[vec[5]] <- geocode("REČICA, SLOVENIJA")$lat
lon[vec[6]] <- geocode("SENČUR, SLOVENIJA")$lon
lat[vec[6]] <- geocode("SUNČUR, SLOVENIJA")$lat
lon[vec[7]] <- geocode("SOLČAVA, SLOVENIJA")$lon
lat[vec[7]] <- geocode("SOLČAVA, SLOVENIJA")$lat
lon[vec[8]] <- geocode("ZAVRČ, SLOVENIJA")$lon
lat[vec[8]] <- geocode("ZAVRČ, SLOVENIJA")$lat



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




library(RgoogleMaps)
lim<- geocode(c("Salovci", "Črnomelj", "Lendava", "Kobarid", "Bovec", "Trst"))

lonn <- c(min(lim$lon), max(lim$lon)) #define our map's ylim
latt <- c(min(lim$lat), max(lim$lat)) #define our map's xlim
markers = paste0("&markers=color:blue|label:S|latt[1],lonn[1]&markers=color:",
                 "green|label:G|latt[2], lonn[2]")
center = c(geocode("Celje, Slovenija")$lat, geocode("Ljubljana, Slovenija")$lon)  #tell what point to center on
zoom <- 8 #zoom: 1 = furthest out (entire globe), larger numbers = closer in
terrmap <- GetMap(center=center, zoom=zoom, markers=markers,maptype= "roadmap") 
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
  lon1[i] <- as.numeric(geocode(paste(mesto1, "Slovenija"))$lon)
  lat1[i] <- as.numeric(geocode(paste(mesto1, "Slovenija"))$lat)
  
}

vec1 <- which(is.na(lon1))
lon1[vec1]  <- lon[vec]
lat1[vec1]  <- lat[vec]
terrmap1 <- GetMap(center=center, zoom=zoom, markers=markers,maptype= "roadmap") 
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
  lon2[i] <- as.numeric(geocode(paste(mesto2, "Slovenija"))$lon)
  lat2[i] <- as.numeric(geocode(paste(mesto2, "Slovenija"))$lat)
  
}

vec2 <- which(is.na(lon2))
lon2[vec2]  <- lon[vec]
lat2[vec2]  <- lat[vec]
terrmap2 <- GetMap(center=center, zoom=zoom, markers=markers,maptype= "roadmap") 
png("govedo.png", type='cairo-png')
tmp2 <- PlotOnStaticMap(terrmap2, lat2, lon2, cex=as.numeric(bum2$Freq)/150,pch=20,col="purple")

dev.off()

library(raster)
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


##############################################################################
######## ZA ALJOŠO ###########

#A = c(13.5-1/90, (45.20+(1/30))-(1.5/120)) 
#D = c(16.5+1/90, 47+(1.5/120))

A <- c(13.5, 45.25) ## spodnje levo oglišče
D <- c(16.5, 47)    ## zgornje desno oglišče
x <- 1/90
y <- 1/120
dim <- c((D[1]-A[1])/x, (D[2]-A[2])/y)
vsi_x <- (1:dim[1])*x + A[1]
vsi_y <- (1:dim[2])*y + A[2]
muhe <- 2500 ## muh na eno žival

##### Drobnica #######
matrika <- matrix(rep(0, dim[1]*dim[2]), dim[1], dim[2])
n <- length(lon)

for(k in 1:n){
  prva <- which(abs(vsi_x-(lon[k]-0.5))==min(abs(vsi_x-(lon[k]-0.5))))
  druga <- which(abs(vsi_y-(lat[k]-0.5))==min(abs(vsi_y-(lat[k]-0.5))))
  matrika[prva, druga] <- (matrika[prva, druga] + (muhe*drobnica[k])/ (0.9))
}

##### Prašiči ##########

matrika1 <- matrix(rep(0, dim[1]*dim[2]), dim[1], dim[2])
n1 <- length(lon1)
for(k in 1:n1){
  prva <- which(abs(vsi_x-(lon1[k]-0.5))==min(abs(vsi_x-(lon1[k]-0.5))))
  druga <- which(abs(vsi_y-(lat1[k]-0.5))==min(abs(vsi_y-(lat1[k]-0.5))))
  matrika1[prva, druga] <- (matrika1[prva, druga] + (muhe*prasici[k])/ (0.9))
}

### Govedo #### 
matrika2 <- matrix(rep(0, dim[1]*dim[2]), dim[1], dim[2])
n2 <- length(lon2)
for(k in 1:n2){
  prva <- which(abs(vsi_x-(lon2[k]-0.5))==min(abs(vsi_x-(lon2[k]-0.5))))
  druga <- which(abs(vsi_y-(lat2[k]-0.5))==min(abs(vsi_y-(lat2[k]-0.5))))
  matrika2[prva, druga] <- (matrika2[prva, druga] + (muhe*goveda[k])/ (0.9))
}

### v matiki2 so shranjene muhe/km^2, če predpostavimo, da je gostitelj govedo. 
