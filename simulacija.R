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
  premik.x <- abs(veter.x * dt * trenje)
  premik.y <- abs(veter.y * dt * trenje)
  
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

simuliraj.dan <-
  function(dan,
           stanje,
           vreme, parametri) {
    govedo <- stanje$zdrava.goveda + stanje$okuzena.goveda
    drobnica <- stanje$zdrava.drobnica + stanje$okuzena.drobnica
    
    # Število pikov
    stopnja.ugrizov <- parametri$stopnja.ugrizov / natancnost
    nagnjenost <-
      parametri$stevilo.muh.na.drobnico / parametri$stevilo.muh.na.govedo
    stevilo.pikov.govedo <-
      stopnja.ugrizov * stanje$okuzene.muhe * (nagnjenost / (nagnjenost + 1)) * (1 / (govedo + drobnica))
    stevilo.pikov.govedo[govedo + drobnica == 0] <- 0
    stevilo.pikov.drobnica <-
      stopnja.ugrizov * stanje$okuzene.muhe * (1 / (nagnjenost + 1)) * (1 / (govedo + drobnica))
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
        stanje$zdrave.muhe * parametri$prenos.gostitelj.na.vektor * stopnja.ugrizov * (stanje$okuzena.drobnica + stanje$okuzena.goveda) / (govedo + drobnica)
      )
    novo.okuzene.muhe[govedo + drobnica == 0] <- 0
    stanje$zdrave.muhe <- stanje$zdrave.muhe - novo.okuzene.muhe
    stanje$okuzene.muhe <- stanje$okuzene.muhe + novo.okuzene.muhe
    
    # Nataliteta muh
    nataliteta.muh <-
      ((1 + parametri$nataliteta.muh) ^ (1 / natancnost) - 1)
    gamma <-
      nataliteta.muh * vreme$temperatura * (vreme$temperatura - 10.4) * sin(dan / parametri$opazovalni.cas.okuzbe * 2 * pi)
    stanje$zdrave.muhe <-
      round(stanje$zdrave.muhe + gamma * stanje$zdrave.muhe)
    stanje$okuzene.muhe <-
      round(stanje$okuzene.muhe + gamma * stanje$okuzene.muhe)
    
    # Preseljevanje muh
    dt <- 24 / natancnost
    stanje$zdrave.muhe <-
      preseli.muhe(stanje$zdrave.muhe,
                   vreme$veter.x,
                   vreme$veter.y,
                   dt, parametri$trenje)
    stanje$okuzene.muhe <-
      preseli.muhe(stanje$okuzene.muhe,
                   vreme$veter.x,
                   vreme$veter.y,
                   dt, parametri$trenje)
    stanje$zdrave.muhe <- stanje$zdrave.muhe * matrikaNicel
    stanje$okuzene.muhe <- stanje$okuzene.muhe * matrikaNicel
    return(stanje)
  }

simuliraj <-
  function (parametri, updateProgress = NULL) {
    # Seznam, v katerem bomo hranili vse podatke simulacije
    stanje <- list()
    
    # Začetno število zdravega in okuženega goveda
    stanje$zdrava.goveda =
      govedo
    stanje$zdrava.drobnica =
      drobnica
    stanje$okuzena.drobnica =
      0 * stanje$zdrava.drobnica
    stanje$okuzena.goveda <-
      0 * stanje$zdrava.goveda
    for (kraj in strsplit(parametri$kraji.okuzbe, ",")[[1]]) {
      kraj <- trim(kraj)
      if (kraj != "")
        stanje$okuzena.goveda[indeks.kraja(kraj)] <-
          parametri$zacetno.stevilo.okuzenih
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
      veter.x <- zonalniVeter[, , dan]
      veter.y <- meridionalniVeter[, , dan]
      natancnost <- ceiling(24 * parametri$trenje * max(veter.x ^ 2 + veter.y ^ 2) ^ 0.5)
      for (korak in 1:natancnost) {
        stanje <-
          simuliraj.dan(
            dan,
            stanje,
            list(
              veter.x = veter.x,
              veter.y = veter.y,
              temperatura = temperatura[, , dan]
            ),
            parametri
          )
        text <-
          paste0("Dan ", dan, " / ", parametri$opazovalni.cas.okuzbe)
        detail <- paste0("", round(100 * korak / natancnost), "%")
        if (is.function(updateProgress)) {
          updateProgress(
            detail = detail,
            message = text,
            value = ((dan - 1) * natancnost + korak) / (parametri$opazovalni.cas.okuzbe * natancnost)
          )
        } else {
          if (korak == 1)
            cat(text)
          cat("...", detail, sep = "")
          if (korak == natancnost)
            cat("\n")
        }
      }
      zgodovina[[dan + 1]] <- stanje
    }
    save(zgodovina,
         file = file.path("izhodni-podatki", parametri$ime.datoteke))
    return(parametri$ime.datoteke)
  }
