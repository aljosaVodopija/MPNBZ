dodaj.rob <- function(matrika) {
  matrika <- rbind(0, matrika, 0)
  matrika <- cbind(0, matrika, 0)
  # matrika[1, ] = matrika[2, ]
  # matrika[nrow(matrika), ] = matrika[nrow(matrika) - 1, ]
  # matrika[, 1] = matrika[, 2]
  # matrika[, ncol(matrika)] = matrika[, ncol(matrika) - 1]
  return(matrika)
}

odstrani.rob <- function(matrika) {
  return(matrika[c(-1, -nrow(matrika)), c(-1, -ncol(matrika))])
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

preseli.muhe <- function(muhe, veter.x, veter.y, dt, trenje) {
  muhe <- dodaj.rob(muhe)
  veter.x <- dodaj.rob(veter.x)
  veter.y <- dodaj.rob(veter.y)
  muhe <- muhe + trenje * dt * (0
    + pmax(0, veter.x + levo(veter.x)) * levo(muhe)
    + pmin(0, veter.x + levo(veter.x)) * muhe
    - pmin(0, veter.x + desno(veter.x)) * desno(muhe)
    - pmax(0, veter.x + desno(veter.x)) * muhe
    + pmax(0, veter.y + spodaj(veter.y)) * spodaj(muhe)
    + pmin(0, veter.y + spodaj(veter.y)) * muhe
    - pmin(0, veter.y + zgoraj(veter.y)) * zgoraj(muhe)
    - pmax(0, veter.y + zgoraj(veter.y)) * muhe
  ) / 2
  muhe <- pmax(muhe, 0)
  muhe <- odstrani.rob(muhe)
  return(muhe)
}

# Dinamični del -----------------------------------------------------------
simuliraj.dan <- function(nataliteta.muh, dan, opazovalni.cas.okuzbe, prenos.govedo.na.muho, stevilo, prenos.muha.na.govedo, veter.x, veter.y) {
  gamma <- nataliteta.muh * T * (T - 10.4) * sin(dan / opazovalni.cas.okuzbe * 2 * pi)
  
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
  trenje <- 0.05
  for (selitev in 1:stevilo.selitev) {
    dt <- 24 / stevilo.selitev
    stevilo$zdrave.muhe <- preseli.muhe(stevilo$zdrave.muhe, veter.x, veter.y, dt, trenje)
    stevilo$okuzene.muhe <-
      preseli.muhe(stevilo$okuzene.muhe, veter.x, veter.y, dt, trenje)
  }
  stevilo$zdrave.muhe <- stevilo$zdrave.muhe * matrikaNicel
  stevilo$okuzene.muhe <- stevilo$okuzene.muhe * matrikaNicel
  
  stevilo$zdrava.goveda <- stevilo$zdrava.goveda - novo.okuzena.goveda
  stevilo$okuzena.goveda <- stevilo$okuzena.goveda + novo.okuzena.goveda
  return(stevilo)
}


simuliraj <-
  function (kraji.okuzbe = c("Grosuplje"),
            stevilo.okuzenih = 200,
            stevilo.muh.na.govedo = 900,
            nataliteta.muh = 0.0003,
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
      stevilo$okuzena.goveda[indeks] <- stevilo$zdrava.goveda[indeks]
      stevilo$zdrava.goveda[indeks] <- 0
    }
    #################################################
    stevilo$zdrave.muhe <-
      stevilo$zdrava.goveda * stevilo.muh.na.govedo
    stevilo$okuzene.muhe <-
      stevilo$okuzena.goveda * stevilo.muh.na.govedo
    
    #####################################################################3
    ############################## SISTEM DIF: ENAČB-PREPISI!!!! ##################
    zgodovina <- as.list(1 : (opazovalni.cas.okuzbe + 1))
    zgodovina[[1]] <- stevilo
    for (dan in 1:opazovalni.cas.okuzbe) {
      # zonalni veter v km/h v smeri zahod-vzhod (pozitivna vrednost pomeni pihanje od zahoda proti vzhodu)
      # prvotni podatki so v smeri vzhod-zahod, zato matriko negiramo
      # TODO: preveri, ali so prvotni podatki res v smeri vzhod-zahod
      veter.x <- t(zonalniVeter[, , dan])
      # meridionalni veter v km/h v smeri jug-sever (pozitivna vrednost pomeni pihanje od juga proti severu)
      veter.y <- t(meridionalniVeter[, , dan])
      # povprečna dnevna temperatura v stopinjah Celzija
      T <- t(temperatura[, , dan])
      stevilo <- simuliraj.dan(nataliteta.muh, dan, opazovalni.cas.okuzbe, prenos.govedo.na.muho, stevilo, prenos.muha.na.govedo, veter.x, veter.y)
      zgodovina[[dan + 1]] <- stevilo
    }
    return(zgodovina)
  }
