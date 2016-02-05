# Priprava ----------------------------------------------------------------

# Po potrebi je treba namestiti pakete s spodnjim ukazom:
# install.packages(c("RgoogleMaps", "rworldmap", "rworldxtra"))

# Naložimo knjižnice za delo z zemljevidi
library(RgoogleMaps)
library(rworldmap)

# Naložimo pomožne funkcije
source("funkcije.r")

# Naložimo vnaprej izračunane podatke. Če jih ni, moramo najprej pognati
# program "predpriprava.r"
load("vmesni-podatki/zonalniVeter.RData")
load("vmesni-podatki/meridionalniVeter.RData")
load("vmesni-podatki/temperatura.RData")
load("vmesni-podatki/matrikaNicel.RData")
load("vmesni-podatki/goveda.RData")
load("vmesni-podatki/drobnica.RData")
load("vmesni-podatki/prasici.RData")


# Risanje zemljevidov -----------------------------------------------------

# Če želimo, lahko s spodnjimi ukazi narišemo zemljevide gospodarstev
# narisi(drobnica, "red", 200)
# shraniZemljevid(drobnica, "izhodni-podatki/drobnica.png", "red", 50)
# narisi(prasici, "blue", 700)
# shraniZemljevid(prasici, "izhodni-podatki/prasici.png", "blue", 250)
# narisi(govedo, "green", 500)
# shraniZemljevid(govedo, "izhodni-podatki/govedo.png", "purple", 150)



# Dinamični del -----------------------------------------------------------

############ Parametri #######################
st_muh <- 900 ## stevilo muh na eno žival
gamma <- 0 ### stopnja natalitete muh
c_2 = 1 # verjetnost prenosa z okuženega goveda na zdravo muho 
c_3 = 0.001 ### gradient zdravih muh
c_4 = 0.01#.001#0.001 ### divergenca
c_5 = 10^(-6)#.05#0.1 ### laplace
c_6 = 0.001 ### gradient okuzenih muh
stevilo.dni = 30### opazovalni cas okuzbe
b = 0.9 ## verjetnost prenosa okužbe

#######################################################
# če je več izbruhov, kopiramo tale del kode ###
######################################################
### Parametra naj bi bil mesto okuzbe - npr: Grosuplje
#####################################################
kraji_okuzbe <- c("Grosuplje") ### lokacija, na kateri nastane okužba

########### Paramerer-stevilo okuzenih ##############
stevilo_okuzenih <- 200 ### stevilo okuzenih na dani lokaciji




#####################################



############################################
############ dinamicen del #################


A = c(13.5-1/90, (45.20+(1/30))-(1.5/120)) ## spodnje levo oglišče pravokotnika v katerega spravimo Slovenijo

D = c(16.5+1/90, 47+(1.5/120)) ## zgornje desno oglišče pravokotnika v katerega spravimo Slovenijo

x <- 1/90   ### premik v smeri x osi
y <- 1/120  ### premik v smeri y osi
dimenzije <- c((D[1] - A[1]) / x + 1, (D[2] - A[2]) / y + 1)

lon_indeksov <- (0:(dim[1]-1)) * x + A[1]
lat_indeksov <- (0:(dim[2]-1)) * y + A[2]

##############################################

koord2indeks <- function(koordinate) {
  x_indeks <- which.min(abs(lon_indeksov - koordinate$lon))
  y_indeks <- which.min(abs(lat_indeksov - koordinate$lat))
  return(matrix(c(x_indeks, y_indeks), ncol = 2))
}

indeks2koord <- function(indeks) {
  return(list(lon = lon_indeksov[indeks[1]], lat = lat_indeksov[indeks[2]]))
}

################################################


zdrave_muhe <- array(0, dim=dimenzije)
okuzene_muhe <- array(0, dim=dimenzije)
zdrava_goveda <- array(0, dim=dimenzije)
okuzena_goveda <- array(0, dim=dimenzije)

####################################################
for(kraj in kraji_okuzbe) {
  mesto_okuzbe = geocode.cache(kraj)
  okuzena_goveda[koord2indeks(mesto_okuzbe)] <- stevilo_okuzenih
}
#################################################


### Govedo #### 
okuzene_muhe <- (okuzena_goveda * st_muh) / 0.9
for(k in 1:length(govedo$lon)){
  indeks = koord2indeks(govedo[k, ])
  zdrava_goveda[indeks] <- govedo$stevilo[k] / 0.9
  zdrave_muhe[indeks] <- govedo$stevilo[k] * st_muh / 0.9  
}




#####################################################################3
############################## SISTEM DIF: ENAČB-PREPISI!!!! ##################

for(i in 1:stevilo.dni) {
  # F = (X,Y) je vektorsko polje vetra
  X <- zonalniVeter[,,i] #zonalni veter v km/h (vzhod-zahod)
  Y <- meridionalniVeter[,,i] #meridionalni veter v km/h (jug-sever)
  T <- temperatura[,,i] # povprecna dnevna temperatura
  ZM <- zdrave_muhe
  OM <- okuzene_muhe
  ZG <- zdrava_goveda
  OG <- okuzena_goveda

  divergenca <- DIV(X,Y)
  gradient_z <- GRADF(ZM, X,Y)
  gradient_o <- GRADF(OM, X,Y)
  laplace_z <- LAPLACE(ZM)
  laplace_o <- LAPLACE(OM)
  ZM <- ZM + gamma * ZM -  c_2 *st_muh * OG + c_3 * gradient_z + c_4*divergenca*ZM + c_5 *laplace_z
  OM <- OM + c_2 * st_muh* OG + c_6* gradient_o + c_4*divergenca*OM + c_5 * laplace_o
  ZM <- ZM * matrikaNicel
  OM <- OM * matrikaNicel
  
  ZG <- ZG - (c_2 / st_muh) * OM
  OG <- OG + (c_2 / st_muh) * OM

  zdrave_muhe <- ZM
  okuzene_muhe <- OM
  zdrava_goveda <- ZG
  okuzena_goveda <- OG
}

#################################################################
##################################################################
indeksi <- which(OG > 0, arr.ind = TRUE)
lon_okuzenih <- lon_indeksov[indeksi[, 1]]
lat_okuzenih <- lat_indeksov[indeksi[, 2]]


newmap <- getMap(resolution = "high")
europe.limits <- geocode.cache(c("Salovci", "Črnomelj", "Lendava", "Kobarid", "Bovec", "Trst"))

plot(newmap,xlim= range(europe.limits$lon), ylim= range(europe.limits$lat), asp = 1, main = "POSKUS")
points(lon_okuzenih, lat_okuzenih, pch=19, col="green", cex = 1)
