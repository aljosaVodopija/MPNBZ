source("operatorji.R")
source("nastaviVeter.R")

geocode.cache <- function(mesta) {
  kode = read.csv('podatki/koordinateKrajev.csv', row.names = 1)
  for(mesto in mesta) {
    mesto = toupper(mesto)
    if(!mesto %in% rownames(kode)) {
      polozaj = geocode(paste(mesto, "SLOVENIJA"))
      kode[mesto, ] = polozaj
    }
  }
  write.csv(kode, 'podatki/koordinateKrajev.csv')
  return(kode[toupper(mesta), ])
}


load("podatki/zonalniVeter.RData")
load("podatki/meridionalniVeter.RData")
load("podatki/temperatura.RData")

# F =(X,Y) je vektorsko polje vetra
X = zonalniVeter[,,1] #zonalni veter v km/h (vzhod-zahod)
Y = meridionalniVeter[,,1] #meridionalni veter v km/h (jug-sever)
Tem = temperatura[,,1] # povprecna dnevna temperatura v kelvinih!!




load("podatki/matrikaNicel.RData")
#####################################

load("podatki/goveda.RData")
load("podatki/drobnica.RData")
load("podatki/prasici.RData")



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
n <- length(govedo$lon)
for(k in 1:n){
  prva <- which(abs(vsi_x-govedo$lon[k])==min(abs(vsi_x-govedo$lon[k])))
  druga <- which(abs(vsi_y-govedo$lat[k])==min(abs(vsi_y-govedo$lat[k])))
  matrika[prva, druga] <- (matrika[prva, druga] + (govedo$stevilo[k])/ (0.9))
  muhe_z[prva, druga] <- (muhe_z[prva, druga] + (govedo$stevilo[k]*st_muh)/ (0.9))  
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


newmap <- getMap(resolution = "high")
europe.limits <- geocode.cache(c("Salovci", "Črnomelj", "Lendava", "Kobarid", "Bovec", "Trst"))

plot(newmap,xlim= range(europe.limits$lon), ylim= range(europe.limits$lat), asp = 1, main = "POSKUS")
points(mapa_x, mapa_y, pch=19, col="green", cex = 1)
