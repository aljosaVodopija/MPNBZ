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
  return(matrika[c(-1,-nrow(matrika)), c(-1,-ncol(matrika))])
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

preseli.muhe <- function(muhe, veter.x, veter.y, dt, trenje) {
  muhe <- dodaj.rob(muhe)
  veter.x <- dodaj.rob(veter.x)
  veter.y <- dodaj.rob(veter.y)
  muhe <- muhe + trenje * dt * (
    0
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
simuliraj.dan <-
  function(parametri,
           dan,
           stanje,
           vreme) {
    govedo <- stanje$zdrava.goveda + stanje$okuzena.goveda
    drobnica <- stanje$zdrava.drobnica + stanje$okuzena.drobnica

    # Število pikov
    stevilo.pikov.govedo <-
      parametri$stopnja.ugrizov * stanje$okuzene.muhe * (nagnjenost / (nagnjenost + 1)) * (1 / (govedo + drobnica))
    stevilo.pikov.govedo[govedo + drobnica == 0] <- 0
    stevilo.pikov.drobnica <-
      parametri$stopnja.ugrizov * stanje$okuzene.muhe * (1 / (nagnjenost + 1)) * (1 / (govedo + drobnica))
    stevilo.pikov.drobnica[govedo + drobnica == 0] <- 0
    novo.okuzena.goveda <- round(stanje$zdrava.goveda * (1 - (1 - parametri$prenos.vektor.na.gostitelj) ^ stevilo.pikov.govedo))
    novo.okuzena.drobnica <- round(stanje$zdrava.drobnica * (1 - (1 - parametri$prenos.vektor.na.gostitelj) ^ stevilo.pikov.drobnica))
    
    # Okužbe goveda
    stanje$zdrava.goveda <-
      stanje$zdrava.goveda - novo.okuzena.goveda
    stanje$okuzena.goveda <-
      stanje$okuzena.goveda + novo.okuzena.goveda
    stanje$zdrava.drobnica <-
      stanje$zdrava.drobnica - novo.okuzena.drobnica
    stanje$okuzena.drobnica <-
      stanje$okuzena.drobnica + novo.okuzena.drobnica
    
    # Okužbe muh
    novo.okuzene.muhe <-
      round(stanje$zdrave.muhe * parametri$prenos.gostitelj.na.vektor * parametri$stopnja.ugrizov * (stanje$okuzena.drobnica + stanje$okuzena.goveda) / (govedo + drobnica))
    novo.okuzene.muhe[govedo + drobnica == 0] <- 0
    stanje$zdrave.muhe <- stanje$zdrave.muhe - novo.okuzene.muhe
    stanje$okuzene.muhe <- stanje$okuzene.muhe + novo.okuzene.muhe
    
    # Nataliteta muh
    gamma <-
      parametri$nataliteta.muh * vreme$temperatura * (vreme$temperatura - 10.4) * sin(dan / parametri$opazovalni.cas.okuzbe * 2 * pi)
    stanje$zdrave.muhe <- stanje$zdrave.muhe + gamma * stanje$zdrave.muhe
    stanje$okuzene.muhe <-
      stanje$okuzene.muhe + gamma * stanje$okuzene.muhe
    
    # Preseljevanje muh
    stevilo.selitev <- 1
    trenje <- 1
    for (selitev in 1:stevilo.selitev) {
      dt <- 24 / stevilo.selitev
      stanje$zdrave.muhe <-
        preseli.muhe(stanje$zdrave.muhe,
                     vreme$veter.x,
                     vreme$veter.y,
                     dt,
                     trenje)
      stanje$okuzene.muhe <-
        preseli.muhe(stanje$okuzene.muhe,
                     vreme$veter.x,
                     vreme$veter.y,
                     dt,
                     trenje)
    }
    stanje$zdrave.muhe <- stanje$zdrave.muhe * matrikaNicel
    stanje$okuzene.muhe <- stanje$okuzene.muhe * matrikaNicel

    return(stanje)
  }

nagnjenost <- 9.4
stalez.drobnice <- 0 * stalez.goveda

simuliraj <-
  function (kraji.okuzbe = c("Škofja Loka"),
            stevilo.okuzenih = 200,
            stevilo.muh.na.govedo = 900,
            stevilo.muh.na.drobnico = 900 / nagnjenost,
            nataliteta.muh = 0.0003,
            prenos.gostitelj.na.vektor = 0.01, # med 0.001 in 0.15
            stopnja.ugrizov = 0.17,
            opazovalni.cas.okuzbe = 30,
            prenos.vektor.na.gostitelj = 0.9) {
    parametri = list(
      kraji.okuzbe = kraji.okuzbe,
      stevilo.okuzenih = stevilo.okuzenih,
      stevilo.muh.na.govedo = stevilo.muh.na.govedo,
      stevilo.muh.na.drobnico = stevilo.muh.na.drobnico,
      nataliteta.muh = nataliteta.muh,
      stopnja.ugrizov = stopnja.ugrizov,
      prenos.gostitelj.na.vektor = prenos.gostitelj.na.vektor,
      opazovalni.cas.okuzbe = opazovalni.cas.okuzbe,
      prenos.vektor.na.gostitelj = prenos.vektor.na.gostitelj
    )
    # Seznam, v katerem bomo hranili vse podatke simulacije
    stanje <- list()
    
    # Začetno število zdravega in okuženega goveda
    stanje$zdrava.goveda =
      stalez.goveda
    stanje$zdrava.drobnica =
      stalez.drobnice
    stanje$okuzena.drobnica =
      0 * stanje$zdrava.drobnica
    stanje$okuzena.goveda <-
      0 * stanje$zdrava.goveda
    for (kraj in parametri$kraji.okuzbe) {
      indeks = koord2indeks(geocode.cache(kraj))
      stanje$okuzena.goveda[indeks] <- parametri$stevilo.okuzenih
    }
    
    # Začetno število zdravih in okuženih muh
    stanje$zdrave.muhe <-
      stanje$zdrava.goveda * parametri$stevilo.muh.na.govedo + stanje$zdrava.drobnica * parametri$stevilo.muh.na.drobnico
    stanje$okuzene.muhe <-
      stanje$okuzena.goveda * parametri$stevilo.muh.na.govedo + stanje$okuzena.drobnica * parametri$stevilo.muh.na.drobnico
    
    # Seznam stanj za vsak dan
    zgodovina <- as.list(1:(parametri$opazovalni.cas.okuzbe + 1))
    zgodovina[[1]] <- stanje
    for (dan in 1:parametri$opazovalni.cas.okuzbe) {
      stanje <-
        simuliraj.dan(
          parametri,
          dan,
          stanje,
          list(
            veter.x = zonalniVeter[, , dan],
            veter.y = meridionalniVeter[, , dan],
            temperatura = temperatura[, , dan]
          )
        )
      zgodovina[[dan + 1]] <- stanje
    }
    return(zgodovina)
  }
