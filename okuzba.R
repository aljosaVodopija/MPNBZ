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
  lon1[i] <- as.numeric(geocode.cache(mesto1)$lon)
  lat1[i] <- as.numeric(geocode.cache(mesto1)$lat)
  
}

##### prasici.png je prikaz prasicev v Sloveniji, razmerje 1:250
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
  lon2[i] <- as.numeric(geocode.cache(mesto2)$lon)
  lat2[i] <- as.numeric(geocode.cache(mesto2)$lat)
  
}

terrmap2 <- GetMap(center=center, zoom=zoom, markers=markers,maptype= "roadmap") 
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


#################################################################33
file = "20150530.nc" #to bo nek datum v odvisnosti od t, t.j. datum(t), 
# ko bo zanka nrejena mi posljes pa bom popravil

fid=ncdf4::nc_open(file, write=FALSE)



#preberemo vetrovni komponenti
ut = ncdf4::ncvar_get(fid, "u10")[61:153,53:108,]

vt = ncdf4::ncvar_get(fid, "v10")[61:153,53:108,]
# temperatura v odvisnosti od casa
tt = ncdf4::ncvar_get(fid, "t2")[61:153,53:108,]
ncdf4::nc_close(fid)

#povprecimo veter cez dan
u = ut[,,8]
v = vt[,,8]
t = tt[,,8]

for (i in c(10,12,14,16,18,20,22)){
  u = u + ut[,,i]
  v = v + vt[,,i]
  t = t + tt[,,i]
}

v = 1/8*v
u = 1/8*u
t = 1/8*t # povprecna dnevna temperatura

N = dim(u)[1] # to je v bistvu dim[1]+2 in dim[2]+2 v tvojem primeru
M = dim(u)[2] 




NASTAVI_VETER = function(u,N,M){ 
  dim = c((N-2)*3,(M-2)*4)
  X = matrix(rep(0, dim[1]*dim[2]), dim[1])
  for (i in 2:(N-1)){ 
    for (j in 2:(M-1)){
      B = matrix(rep(0,4*3),3)
      B[1,1] = 1/6*(u[i,j-1]+u[i-1,j-1]+u[i-1,j]) + 1/2*u[i,j]
      B[1,2] = 1/4*(u[i-1,j-1]+u[i-1,j])+ 1/2*u[i,j]
      B[1,3] = 1/4*(u[i-1,j]+u[i-1,j+1])+ 1/2*u[i,j] 
      B[1,4] = 1/6*(u[i-1,j]+u[i-1,j+1]+u[i,j+1]) + 1/2*u[i,j]
      B[2,1] = 1/3*u[i,j-1] + 2/3*u[i,j]
      B[2,2] = 1/4*u[i,j-1] + 3/4*u[i,j]
      B[2,3] = 1/4*u[i,j+1] + 3/4*u[i,j]
      B[2,4] = 1/3*u[i,j+1] + 2/3*u[i,j]
      B[3,1] = 1/6*(u[i,j-1]+u[i+1,j-1]+u[i+1,j]) + 1/2*u[i,j]
      B[3,2] = 1/4*(u[i+1,j-1]+u[i+1,j]) + 1/2*u[i,j]
      B[3,3] = 1/4*(u[i+1,j]+u[i+1,j+1]) + 1/2*u[i,j]
      B[3,4] = 1/6*(u[i+1,j]+u[i+1,j+1]+u[i,j+1]) + 1/2*u[i,j]
      X[(3*(i-2)+1):(3*(i-2)+3),(4*(j-2)+1):(4*(j-2)+4)]=B
    }}
  return(X)
}



# F =(X,Y) je vektorsko polje vetra
X = (3.6*24)*NASTAVI_VETER(u,N,M) #zonalni veter v km/h (vzhod-zahod)
Y = (3.6*24)*NASTAVI_VETER(v,N,M) #meridionalni veter v km/h (jug-sever)
Tem = NASTAVI_VETER(t,N,M) # povprecna dnevna temperatura v kelvinih!!
Tem = Tem-273 




#Zacetne nastavitve
hx = 0.9 #longitude razdalja
hy = 1 #latitude razdalja

#Odvod skalarnega polja F po y (Fy), pri tem uporablja sim. dif.

odvody = function(F,h){
  
  n = dim(F)[1]
  m = dim(F)[2]
  Fy = matrix(1:(n*m),n)
  
  Fy[,2:(m-1)] = 1/(2*h)*(F[,3:m]-F[,1:(m-2)])
  Fy[,1] = 1/(2*h)*(F[,2]-F[,1])
  Fy[,m] = 1/(2*h)*(F[,m]-F[,(m-1)])
  
  return(Fy)
}

#Odvod skalarnega polja F po x (Fx), pri tem uporablja sim. dif.

odvodx = function(F,h){
  
  n = dim(F)[1]
  m = dim(F)[2]
  Fx = matrix(1:(n*m),n)
  
  Fx[2:(n-1),] = 1/(2*h)*(F[3:n,]-F[1:(n-2),])
  Fx[1,] = 1/(2*h)*(F[2,]-F[1,])
  Fx[n,] = 1/(2*h)*(F[n,]-F[(n-1),])
  
  return(Fx)
}

#Drugi odvod skalrnega polja F po y (Fyy)

odvodyy = function(F,h){
  
  n = dim(F)[1]
  m = dim(F)[2]
  Fyy = matrix(1:(n*m),n)
  
  Fyy[,2:(m-1)] = 1/(h^2)*(F[,3:m]+F[,1:(m-2)]-2*F[,2:(m-1)])
  Fyy[,1] = 1/(h^2)*(F[,2]-F[,1])
  Fyy[,m] = 1/(h^2)*(F[,(m-1)]-F[,m])
  
  return(Fyy)
}

#Drugi odvod skalarnega polja F po x (Fxx)

odvodxx = function(F,h){
  
  n = dim(F)[1]
  m = dim(F)[2]
  Fxx = matrix(1:(n*m),n)
  
  Fxx[2:(n-1),] = 1/(h^2)*(F[3:n,]+F[1:(n-2),]-2*F[2:(n-1),])
  Fxx[1,] = 1/(h^2)*(F[2,]-F[1,])
  Fxx[n,] = 1/(h^2)*(F[(n-1),]-F[n,])
  
  return(Fxx)
}


#DIV izracuna diskretno divergenco F=(F1,F2) (vektorsko polje vetra)
#Prejme metriko X (jakost zonalnega vetra) in matriko Y (jakost meridionalnega vetra),
#pri tem velja dim(Y)=dim(X). 
#Izraèuna matriki F1x in F2y ter vrne matriko F1x + F2y. 

DIV = function(X,Y){
  
  return(odvodx(X,hx)+odvody(Y,hy))
}

#GRADF izracuna s. p. med grad(m)*F (m skalarno polje, F vektorsko polje)
#Prejme metriko M in matriki X, Y  
#Izraèuna matriki Mx, My in vrne iskani s. p. 

GRADF = function(M,X,Y){
  
  return(odvodx(M,hx)*X+odvody(M,hy)*Y)
}

#Diskretiziran Laplaceov operator skalarnega polja
#Prejme matriko M (skalarno polje)
#Izracuna Mxx in Myy ter vrne Mxx + Myy

LAPLACE = function(M){
  
  return(odvodxx(M,hx)+odvodyy(M,hy))
}

###############################################################

NASTAVI_NICLE = function(){
  
  #zacetne nastavitve
  #odpremo podatke
  file = "geo_em.d02.nc"
  fid = ncdf4::nc_open(file,write=FALSE)
  lu = ncdf4::ncvar_get(fid, "LU_INDEX")[61:153,53:108] #LU index
  visina = ncdf4::ncvar_get(fid, "HGT_M")[61:153,53:108]  # nadmorska visina
  ncdf4::nc_close(fid)
  N = 93
  M = 56
  max_visina = 999 # maksimalna nadmorska visina
  neugodne_povrsine = c(1,2,3,4,5,11,15,17)#neugodne povrsine
  dim = c((N-2)*3,(M-2)*4)
  X = matrix(rep(0, dim[1]*dim[2]), dim[1])
  
  for (i in 2:(N-1)){ 
    
    for (j in 2:(M-1)){
      
      #preveri ali je visina visja od max oz. ali je povrsina neugodna in
      #v pozitivnem primeru nastavi nicle
      if((visina[i,j] > max_visina) || (lu[i,j] %in% neugodne_povrsine)){
        X[(3*(i-2)+1):(3*(i-2)+3),(4*(j-2)+1):(4*(j-2)+4)]=0
      } else {
        X[(3*(i-2)+1):(3*(i-2)+3),(4*(j-2)+1):(4*(j-2)+4)]=1
      }
      
    }}
  
  return(X)
}

matrika_nicel = NASTAVI_NICLE()
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
n <- length(lon2)
for(k in 1:n){
  prva <- which(abs(vsi_x-lon2[k])==min(abs(vsi_x-lon2[k])))
  druga <- which(abs(vsi_y-lat2[k])==min(abs(vsi_y-lat2[k])))
  matrika[prva, druga] <- (matrika[prva, druga] + (goveda[k])/ (0.9))
  muhe_z[prva, druga] <- (muhe_z[prva, druga] + (goveda[k]*st_muh)/ (0.9))  
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
  muhe_z <- muhe_z * matrika_nicel
  muhe_o <- muhe_o * matrika_nicel
  
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
