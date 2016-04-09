

popravi.rob <- function(matrika) {
  matrika[1,] = matrika[2,]
  matrika[nrow(matrika),] = matrika[nrow(matrika) - 1,]
  matrika[,1] = matrika[,2]
  matrika[, ncol(matrika)] = matrika[, ncol(matrika) - 1]
  return(matrika)
}

levo <- function(matrika) {
  matrika[,-1] <- matrika[,-ncol(matrika)]
  return(matrika)
}

desno <- function(matrika) {
  matrika[,-ncol(matrika)] <- matrika[,-1]
  return(matrika)
}

zgoraj <- function(matrika) {
  matrika[-1,] <- matrika[-nrow(matrika),]
  return(matrika)
}

spodaj <- function(matrika) {
  matrika[-nrow(matrika),] <- matrika[-1,]
  return(matrika)
}

preseli.muhe <- function(muhe, veter.x, veter.y, dt) {
  muhe <- muhe + dt * (
    (veter.x + levo(veter.x)) / 2 * (muhe + levo(muhe)) / 2
    - (veter.x + desno(veter.x)) / 2 * (muhe + desno(muhe)) / 2
    + (veter.y + spodaj(veter.y)) / 2 * (muhe + spodaj(muhe)) / 2
    - (veter.y + zgoraj(veter.y)) / 2 * (muhe + zgoraj(muhe)) / 2
  )
  return(muhe)
}
################################################

# Dinamični del -----------------------------------------------------------

simuliraj <- function (kraji_okuzbe = c("Grosuplje"), stevilo_okuzenih = 200, st_muh=900, gamma0=0.5, prenos.govedo.na.muho=1,
                       stevilo.dni = 30, prenos.muha.na.govedo = 0.9) {
#   ############ Parametri #######################
#   st_muh <- 900 ## stevilo muh na eno žival
#   gamma <- 0 ### stopnja natalitete muh
#   stevilo.dni = 30### opazovalni cas okuzbe
#   
#   #######################################################
#   # če je več izbruhov, kopiramo tale del kode ###
#   ######################################################
#   ### Parametra naj bi bil mesto okuzbe - npr: Grosuplje
#   #####################################################
#   kraji_okuzbe <- c("Koper") ### lokacija, na kateri nastane okužba
#   
#   ########### Paramerer-stevilo okuzenih ##############
#   stevilo_okuzenih <- 200 ### stevilo okuzenih na dani lokaciji
  zdrave_muhe <- array(0, dim=dimenzije)
  okuzene_muhe <- array(0, dim=dimenzije)
  zdrava_goveda <- array(0, dim=dimenzije)
  okuzena_goveda <- array(0, dim=dimenzije)
  
  ### Govedo #### 
  for(k in 1:length(govedo$lon)){
    indeks = koord2indeks(govedo[k, ])
    zdrava_goveda[indeks] <- zdrava_goveda[indeks] + govedo$stevilo[k]
  }
  ####################################################
  for(kraj in kraji_okuzbe) {
    mesto_okuzbe = geocode.cache(kraj)
    indeks = koord2indeks(mesto_okuzbe)
    zdrava_goveda[indeks] <- zdrava_goveda[indeks] - stevilo_okuzenih
    okuzena_goveda[indeks] <- stevilo_okuzenih
  }
  #################################################
  zdrave_muhe <- (okuzena_goveda + zdrava_goveda) * st_muh

  
  
  #####################################################################3
  ############################## SISTEM DIF: ENAČB-PREPISI!!!! ##################
  zgodovina.okuzb <- array(NA, dim=c(dimenzije, stevilo.dni + 1))
  zgodovina.okuzb[,,1] <- okuzena_goveda
  for(i in 1:stevilo.dni) {
    # zonalni veter v km/h v smeri zahod-vzhod (pozitivna vrednost pomeni pihanje od zahoda proti vzhodu)
    # prvotni podatki so v smeri vzhod-zahod, zato matriko negiramo
    # TODO: preveri, ali so prvotni podatki res v smeri vzhod-zahod
    veter.x <- -t(zonalniVeter[,,i])
    # meridionalni veter v km/h v smeri jug-sever (pozitivna vrednost pomeni pihanje od juga proti severu)
    veter.y <- t(meridionalniVeter[,,i])
    # povprečna dnevna temperatura v stopinjah Celzija
    T <- t(temperatura[,,i])

    # TODO: gamma je v resnici odvisna od temperature
    gamma <- gamma0 # * sin(i / stevilo.dni * 2 * pi)
    novo_okuzene_muhe <- round(prenos.govedo.na.muho * zdrave_muhe * okuzena_goveda / (zdrava_goveda + okuzena_goveda))
    novo_okuzene_muhe[zdrava_goveda + okuzena_goveda == 0] <- 0
    novo_okuzena_goveda <- round(prenos.muha.na.govedo * zdrava_goveda * okuzene_muhe / (zdrave_muhe + okuzene_muhe))
    novo_okuzena_goveda[zdrave_muhe + okuzene_muhe == 0] <- 0
    
    zdrave_muhe <- zdrave_muhe + gamma * zdrave_muhe - novo_okuzene_muhe
    okuzene_muhe <- okuzene_muhe + gamma * okuzene_muhe + novo_okuzene_muhe

    stevilo.selitev <- 1
    for(i in 1:stevilo.selitev) {
      dt <- 24 / stevilo.selitev
      zdrave_muhe <- preseli.muhe(zdrave_muhe, veter.x, veter.y, dt)
      okuzene_muhe <- preseli.muhe(okuzene_muhe, veter.x, veter.y, dt)
    }
    zdrave_muhe <- zdrave_muhe * t(matrikaNicel)
    okuzene_muhe <- okuzene_muhe * t(matrikaNicel)

    zdrava_goveda <- zdrava_goveda - novo_okuzena_goveda
    okuzena_goveda <- okuzena_goveda + novo_okuzena_goveda

    #stopifnot(any(zdrave_muhe < 0))
    #stopifnot(any(okuzene_muhe < 0))
    #stopifnot(any(zdrava_goveda < 0))
    #stopifnot(any(okuzena_goveda < 0))
    
    zgodovina.okuzb[,,i + 1] <- okuzena_goveda
    zdrava_goveda <- levo(zdrava_goveda)
    #print(sum(okuzene_muhe))
  }
  return(zgodovina.okuzb)
}
