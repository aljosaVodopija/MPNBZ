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

# Dinamični del -----------------------------------------------------------

simuliraj <-
  function (kraji.okuzbe = c("Grosuplje"),
            stevilo.okuzenih = 200,
            stevilo.muh.na.govedo = 900,
            nataliteta.muh = 0.5,
            prenos.govedo.na.muho = 1,
            opazovalni.cas.okuzbe = 30,
            prenos.muha.na.govedo = 0.9) {
    stevilo <- list(
      zdrave.muhe = array(0, dim = dimenzije),
      okuzene.muhe = array(0, dim = dimenzije),
      zdrava.goveda = stalez.goveda,
      okuzena.goveda = array(0, dim = dimenzije)
    )
    
    ####################################################
    for (kraj in kraji.okuzbe) {
      indeks = koord2indeks(geocode.cache(kraj))
      stevilo$zdrava.goveda[indeks] <-
        stevilo$zdrava.goveda[indeks] - stevilo.okuzenih
      stevilo$okuzena.goveda[indeks] <- stevilo.okuzenih
    }
    #################################################
    stevilo$zdrave.muhe <-
      (stevilo$okuzena.goveda + stevilo$zdrava.goveda) * stevilo.muh.na.govedo
    
    #####################################################################3
    ############################## SISTEM DIF: ENAČB-PREPISI!!!! ##################
    zgodovina <- as.list(1 : (opazovalni.cas.okuzbe + 1))
    zgodovina[[1]] <- stevilo
    for (dan in 1:opazovalni.cas.okuzbe) {
      # zonalni veter v km/h v smeri zahod-vzhod (pozitivna vrednost pomeni pihanje od zahoda proti vzhodu)
      # prvotni podatki so v smeri vzhod-zahod, zato matriko negiramo
      # TODO: preveri, ali so prvotni podatki res v smeri vzhod-zahod
      veter.x <- -t(zonalniVeter[, , dan])
      # meridionalni veter v km/h v smeri jug-sever (pozitivna vrednost pomeni pihanje od juga proti severu)
      veter.y <- t(meridionalniVeter[, , dan])
      # povprečna dnevna temperatura v stopinjah Celzija
      T <- t(temperatura[, , dan])
      
      # TODO: gamma je v resnici odvisna od temperature
      gamma <-
        nataliteta.muh # * sin(i / opazovalni.cas.okuzbe * 2 * pi)
      novo.okuzene.muhe <-
        round(
          prenos.govedo.na.muho * stevilo$zdrave.muhe * stevilo$okuzena.goveda / (stevilo$zdrava.goveda + stevilo$okuzena.goveda)
        )
      novo.okuzene.muhe[stevilo$zdrava.goveda + stevilo$okuzena.goveda == 0] <- 0
      novo.okuzena.goveda <-
        round(
          prenos.muha.na.govedo * stevilo$zdrava.goveda * stevilo$okuzene.muhe / (stevilo$zdrave.muhe + stevilo$okuzene.muhe)
        )
      novo.okuzena.goveda[stevilo$zdrave.muhe + stevilo$okuzene.muhe == 0] <- 0
      
      stevilo$zdrave.muhe <-
        stevilo$zdrave.muhe + gamma * stevilo$zdrave.muhe - novo.okuzene.muhe
      stevilo$okuzene.muhe <-
        stevilo$okuzene.muhe + gamma * stevilo$okuzene.muhe + novo.okuzene.muhe
      
      stevilo.selitev <- 1
      for (selitev in 1:stevilo.selitev) {
        dt <- 24 / stevilo.selitev
        stevilo$zdrave.muhe <- preseli.muhe(stevilo$zdrave.muhe, veter.x, veter.y, dt)
        stevilo$okuzene.muhe <-
          preseli.muhe(stevilo$okuzene.muhe, veter.x, veter.y, dt)
      }
      stevilo$zdrave.muhe <- stevilo$zdrave.muhe * t(matrikaNicel)
      stevilo$okuzene.muhe <- stevilo$okuzene.muhe * t(matrikaNicel)
      
      stevilo$zdrava.goveda <- stevilo$zdrava.goveda - novo.okuzena.goveda
      stevilo$okuzena.goveda <- stevilo$okuzena.goveda + novo.okuzena.goveda
      
      zgodovina[[dan + 1]] <- stevilo
    }
    return(zgodovina)
  }
