x.lim <- c(13.5 - 1 / 90, 16.5 + 1 / 90)
y.lim <- c(45.2 + 1 / 30 - 1.5 / 120,  47 + 1.5 / 120)
dx <- 1 / 90
dy <- 1 / 120
st.vrstic <- round((y.lim[2] - y.lim[1]) / dy) + 1
st.stolpcev <- round((x.lim[2] - x.lim[1]) / dx) + 1
dimenzije <- c(st.vrstic, st.stolpcev)

indeks.kraja <- function(mesto) {
  kode = read.csv('vmesni-podatki/koordinateKrajev.csv', row.names = 1)
  mesto = toupper(mesto)
  if(!mesto %in% rownames(kode)) {
    kode[mesto, ] = geocode(paste(mesto, "SLOVENIJA"))
  }
  write.csv(kode, 'vmesni-podatki/koordinateKrajev.csv')
  koordinate <- kode[mesto, ]
  stolpec <- round((koordinate$lon - x.lim[1]) / dx + 1 / 2)
  vrstica <- round((y.lim[2] - koordinate$lat) / dy + 1 / 2)
  return(matrix(c(vrstica, stolpec), ncol = 2))
}

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
  # veter.x <- 0 * veter.x + 5
  # veter.y <- 0 * veter.y + 5
  
  premik.x <- abs(veter.x * dt)
  premik.y <- abs(veter.y * dt)
  
  izstop.x <- muhe * premik.x * (1 - premik.y)
  izstop.y <- muhe * (1 - premik.x) * premik.y
  izstop.xy <- muhe * premik.x * premik.y
  pri.miru <- muhe - izstop.x - izstop.y - izstop.xy
  
  preseljene.muhe <-
    pri.miru +
    levo(izstop.x * (veter.x > 0)) +
    desno(izstop.y * (veter.x < 0)) +
    spodaj(izstop.y * (veter.y > 0)) +
    zgoraj(izstop.y * (veter.y < 0)) +
    levo(spodaj(izstop.xy * (veter.x > 0 && veter.y > 0))) +
    desno(spodaj(izstop.xy * (veter.x < 0 && veter.y > 0))) +
    levo(zgoraj(izstop.xy * (veter.x > 0 && veter.y < 0))) +
    desno(zgoraj(izstop.xy * (veter.x < 0 && veter.y < 0)))

  return(round(preseljene.muhe))
}

# Dinamični del -----------------------------------------------------------
simuliraj.dan <-
  function(parametri,
           dan,
           stanje,
           vreme) {
    for (korak in 1:natancnost) {
      govedo <- stanje$zdrava.goveda + stanje$okuzena.goveda
      drobnica <- stanje$zdrava.drobnica + stanje$okuzena.drobnica
      
      # Število pikov
      stevilo.pikov.govedo <-
        parametri$stopnja.ugrizov * stanje$okuzene.muhe * (nagnjenost / (nagnjenost + 1)) * (1 / (govedo + drobnica))
      stevilo.pikov.govedo[govedo + drobnica == 0] <- 0
      stevilo.pikov.drobnica <-
        parametri$stopnja.ugrizov * stanje$okuzene.muhe * (1 / (nagnjenost + 1)) * (1 / (govedo + drobnica))
      stevilo.pikov.drobnica[govedo + drobnica == 0] <- 0
      novo.okuzena.goveda <-
        round(stanje$zdrava.goveda * (
          1 - (1 - parametri$prenos.vektor.na.gostitelj) ^ stevilo.pikov.govedo
        ))
      novo.okuzena.drobnica <-
        round(stanje$zdrava.drobnica * (
          1 - (1 - parametri$prenos.vektor.na.gostitelj) ^ stevilo.pikov.drobnica
        ))
      
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
        round(
          stanje$zdrave.muhe * parametri$prenos.gostitelj.na.vektor * parametri$stopnja.ugrizov * (stanje$okuzena.drobnica + stanje$okuzena.goveda) / (govedo + drobnica)
        )
      novo.okuzene.muhe[govedo + drobnica == 0] <- 0
      stanje$zdrave.muhe <- stanje$zdrave.muhe - novo.okuzene.muhe
      stanje$okuzene.muhe <- stanje$okuzene.muhe + novo.okuzene.muhe
      
      # Nataliteta muh
      gamma <-
        parametri$nataliteta.muh * vreme$temperatura * (vreme$temperatura - 10.4) * sin(dan / parametri$opazovalni.cas.okuzbe * 2 * pi)
      stanje$zdrave.muhe <-
        round(stanje$zdrave.muhe + gamma * stanje$zdrave.muhe)
      stanje$okuzene.muhe <-
        round(stanje$okuzene.muhe + gamma * stanje$okuzene.muhe)
      
      # Preseljevanje muh
      trenje <- 1
      dt <- 24 / natancnost
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
      stanje$zdrave.muhe <- stanje$zdrave.muhe * matrikaNicel
      stanje$okuzene.muhe <- stanje$okuzene.muhe * matrikaNicel
    }
    return(stanje)
  }

nagnjenost <- 9.4

simuliraj <-
  function (kraji.okuzbe = c("Škofja Loka"),
            stevilo.okuzenih = 200,
            stevilo.muh.na.govedo = 900,
            stevilo.muh.na.drobnico = 900 / nagnjenost,
            nataliteta.muh = ((1 + 0.0003) ^ (1 / natancnost) - 1),
            prenos.gostitelj.na.vektor = 0.01,
            # med 0.001 in 0.15
            stopnja.ugrizov = 0.17 / natancnost,
            opazovalni.cas.okuzbe = 5,
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
      govedo
    stanje$zdrava.drobnica =
      0 * drobnica
    stanje$okuzena.drobnica =
      0 * stanje$zdrava.drobnica
    stanje$okuzena.goveda <-
      0 * stanje$zdrava.goveda
    for (kraj in parametri$kraji.okuzbe) {
      stanje$okuzena.goveda[indeks.kraja(kraj)] <- parametri$stevilo.okuzenih
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
