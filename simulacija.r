popravi.rob <- function(matrika) {
  matrika[1, ] = matrika[2, ]
  matrika[nrow(matrika), ] = matrika[nrow(matrika) - 1, ]
  matrika[, 1] = matrika[, 2]
  matrika[, ncol(matrika)] = matrika[, ncol(matrika) - 1]
  return(matrika)
}

levo <- function(matrika) {
  matrika[, -1] <- matrika[, -ncol(matrika)]
  return(matrika)
}

desno <- function(matrika) {
  matrika[, -ncol(matrika)] <- matrika[, -1]
  return(matrika)
}

zgoraj <- function(matrika) {
  matrika[-1, ] <- matrika[-nrow(matrika), ]
  return(matrika)
}

spodaj <- function(matrika) {
  matrika[-nrow(matrika), ] <- matrika[-1, ]
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

simuliraj <-
  function (kraji.okuzbe = c("Grosuplje"),
            stevilo.okuzenih = 200,
            stevilo.muh.na.govedo = 900,
            nataliteta.muh = 0.5,
            prenos.govedo.na.muho = 1,
            opazovalni.cas.okuzbe = 30,
            prenos.muha.na.govedo = 0.9) {
    zdrave.muhe <- array(0, dim = dimenzije)
    okuzene.muhe <- array(0, dim = dimenzije)
    zdrava.goveda <- array(0, dim = dimenzije)
    okuzena.goveda <- array(0, dim = dimenzije)
    
    ### Govedo ####
    for (k in 1:length(govedo$lon)) {
      indeks = koord2indeks(govedo[k,])
      zdrava.goveda[indeks] <-
        zdrava.goveda[indeks] + govedo$stevilo[k]
    }
    ####################################################
    for (kraj in kraji.okuzbe) {
      indeks = koord2indeks(geocode.cache(kraj))
      zdrava.goveda[indeks] <-
        zdrava.goveda[indeks] - stevilo.okuzenih
      okuzena.goveda[indeks] <- stevilo.okuzenih
    }
    #################################################
    zdrave.muhe <-
      (okuzena.goveda + zdrava.goveda) * stevilo.muh.na.govedo
    
    #####################################################################3
    ############################## SISTEM DIF: ENAČB-PREPISI!!!! ##################
    zgodovina.okuzb <-
      array(NA, dim = c(dimenzije, opazovalni.cas.okuzbe + 1))
    zgodovina.okuzb[, , 1] <- okuzena.goveda
    for (i in 1:opazovalni.cas.okuzbe) {
      # zonalni veter v km/h v smeri zahod-vzhod (pozitivna vrednost pomeni pihanje od zahoda proti vzhodu)
      # prvotni podatki so v smeri vzhod-zahod, zato matriko negiramo
      # TODO: preveri, ali so prvotni podatki res v smeri vzhod-zahod
      veter.x <- -t(zonalniVeter[, , i])
      # meridionalni veter v km/h v smeri jug-sever (pozitivna vrednost pomeni pihanje od juga proti severu)
      veter.y <- t(meridionalniVeter[, , i])
      # povprečna dnevna temperatura v stopinjah Celzija
      T <- t(temperatura[, , i])
      
      # TODO: gamma je v resnici odvisna od temperature
      gamma <-
        nataliteta.muh # * sin(i / opazovalni.cas.okuzbe * 2 * pi)
      novo.okuzene.muhe <-
        round(
          prenos.govedo.na.muho * zdrave.muhe * okuzena.goveda / (zdrava.goveda + okuzena.goveda)
        )
      novo.okuzene.muhe[zdrava.goveda + okuzena.goveda == 0] <- 0
      novo.okuzena.goveda <-
        round(
          prenos.muha.na.govedo * zdrava.goveda * okuzene.muhe / (zdrave.muhe + okuzene.muhe)
        )
      novo.okuzena.goveda[zdrave.muhe + okuzene.muhe == 0] <- 0
      
      zdrave.muhe <-
        zdrave.muhe + gamma * zdrave.muhe - novo.okuzene.muhe
      okuzene.muhe <-
        okuzene.muhe + gamma * okuzene.muhe + novo.okuzene.muhe
      
      stevilo.selitev <- 1
      for (i in 1:stevilo.selitev) {
        dt <- 24 / stevilo.selitev
        zdrave.muhe <- preseli.muhe(zdrave.muhe, veter.x, veter.y, dt)
        okuzene.muhe <-
          preseli.muhe(okuzene.muhe, veter.x, veter.y, dt)
      }
      zdrave.muhe <- zdrave.muhe * t(matrikaNicel)
      okuzene.muhe <- okuzene.muhe * t(matrikaNicel)
      
      zdrava.goveda <- zdrava.goveda - novo.okuzena.goveda
      okuzena.goveda <- okuzena.goveda + novo.okuzena.goveda
      
      zgodovina.okuzb[, , i + 1] <- okuzena.goveda
      zdrava.goveda <- levo(zdrava.goveda)
    }
    return(zgodovina.okuzb)
  }
