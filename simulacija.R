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

preseliMuhe <- function(muhe, veterX, veterY, dt, trenje) {
  premikX <- abs(veterX * dt * trenje)
  premikY <- abs(veterY * dt * trenje)
  
  izstopX <- muhe * premikX * (1 - premikY)
  izstopY <- muhe * (1 - premikX) * premikY
  izstopXY <- muhe * premikX * premikY
  priMiru <- muhe - izstopX - izstopY - izstopXY
  
  preseljeneMuhe <-
    priMiru +
    levo(izstopX * (veterX > 0)) +
    desno(izstopY * (veterX < 0)) +
    spodaj(izstopY * (veterY > 0)) +
    zgoraj(izstopY * (veterY < 0)) +
    levo(spodaj(izstopXY * (veterX > 0 && veterY > 0))) +
    desno(spodaj(izstopXY * (veterX < 0 && veterY > 0))) +
    levo(zgoraj(izstopXY * (veterX > 0 && veterY < 0))) +
    desno(zgoraj(izstopXY * (veterX < 0 && veterY < 0)))
  
  return(round(preseljeneMuhe))
}

simulirajKorak <-
  function(dan,
           stanje,
           vreme, parametri, natancnost) {
    govedo <- stanje$zdravoGovedo + stanje$okuzenoGovedo
    drobnica <- stanje$zdravaDrobnica + stanje$okuzenaDrobnica
    
    # Število pikov
    stopnjaUgrizov <- parametri$stopnjaUgrizov / natancnost
    nagnjenost <-
      parametri$steviloMuhNaDrobnico / parametri$steviloMuhNaGovedo
    steviloPikovGovedo <-
      stopnjaUgrizov * stanje$okuzeneMuhe * (nagnjenost / (nagnjenost + 1)) * (1 / (govedo + drobnica))
    steviloPikovGovedo[govedo + drobnica == 0] <- 0
    steviloPikovDrobnica <-
      stopnjaUgrizov * stanje$okuzeneMuhe * (1 / (nagnjenost + 1)) * (1 / (govedo + drobnica))
    steviloPikovDrobnica[govedo + drobnica == 0] <- 0
    novoOkuzenoGovedo <-
      round(stanje$zdravoGovedo * (
        1 - (1 - parametri$prenosVektorNaGostitelj) ^ steviloPikovGovedo
      ))
    novoOkuzenaDrobnica <-
      round(stanje$zdravaDrobnica * (
        1 - (1 - parametri$prenosVektorNaGostitelj) ^ steviloPikovDrobnica
      ))
    
    # Okužbe goveda
    stanje$zdravoGovedo <-
      stanje$zdravoGovedo - novoOkuzenoGovedo
    stanje$okuzenoGovedo <-
      stanje$okuzenoGovedo + novoOkuzenoGovedo
    stanje$zdravaDrobnica <-
      stanje$zdravaDrobnica - novoOkuzenaDrobnica
    stanje$okuzenaDrobnica <-
      stanje$okuzenaDrobnica + novoOkuzenaDrobnica
    
    # Okužbe muh
    novoOkuzeneMuhe <-
      round(
        stanje$zdraveMuhe * parametri$prenosGostiteljNaVektor * stopnjaUgrizov * (stanje$okuzenaDrobnica + stanje$okuzenoGovedo) / (govedo + drobnica)
      )
    novoOkuzeneMuhe[govedo + drobnica == 0] <- 0
    stanje$zdraveMuhe <- stanje$zdraveMuhe - novoOkuzeneMuhe
    stanje$okuzeneMuhe <- stanje$okuzeneMuhe + novoOkuzeneMuhe
    
    # Nataliteta muh
    natalitetaMuh <-
      ((1 + parametri$natalitetaMuh) ^ (1 / natancnost) - 1)
    gamma <-
      natalitetaMuh * vreme$temperatura * (vreme$temperatura - 10.4) * sin(dan / parametri$opazovalniCasOkuzbe * 2 * pi)
    stanje$zdraveMuhe <-
      round(stanje$zdraveMuhe + gamma * stanje$zdraveMuhe)
    stanje$okuzeneMuhe <-
      round(stanje$okuzeneMuhe + gamma * stanje$okuzeneMuhe)
    
    # Preseljevanje muh
    dt <- 24 / natancnost
    stanje$zdraveMuhe <-
      preseliMuhe(stanje$zdraveMuhe,
                  vreme$veterX,
                  vreme$veterY,
                  dt,
                  parametri$trenje)
    stanje$okuzeneMuhe <-
      preseliMuhe(stanje$okuzeneMuhe,
                  vreme$veterX,
                  vreme$veterY,
                  dt,
                  parametri$trenje)
    stanje$zdraveMuhe <- stanje$zdraveMuhe * neprimernaObmocja
    stanje$okuzeneMuhe <- stanje$okuzeneMuhe * neprimernaObmocja
    return(stanje)
  }

simuliraj <-
  function (parametri, updateProgress = NULL) {
    # Seznam, v katerem bomo hranili vse podatke simulacije
    stanje <- list()
    
    # Začetno število zdravega in okuženega goveda
    stanje$zdravoGovedo =
      govedo
    stanje$zdravaDrobnica =
      drobnica
    stanje$okuzenaDrobnica =
      0 * stanje$zdravaDrobnica
    stanje$okuzenoGovedo <-
      0 * stanje$zdravoGovedo
    for (kraj in strsplit(parametri$krajiOkuzbe, ",")[[1]]) {
      kraj <- trim(kraj)
      if (kraj != "")
        stanje$okuzenoGovedo[indeksKraja(kraj)] <-
          parametri$zacetnoSteviloOkuzenih
    }
    
    # Začetno število zdravih in okuženih muh
    stanje$zdraveMuhe <-
      stanje$zdravoGovedo * parametri$steviloMuhNaGovedo + stanje$zdravaDrobnica * parametri$steviloMuhNaDrobnico
    stanje$okuzeneMuhe <-
      stanje$okuzenoGovedo * parametri$steviloMuhNaGovedo + stanje$okuzenaDrobnica * parametri$steviloMuhNaDrobnico
    
    # Seznam stanj za vsak dan
    zgodovina <- as.list(1:(parametri$opazovalniCasOkuzbe + 1))
    zgodovina[[1]] <- stanje
    for (dan in 1:parametri$opazovalniCasOkuzbe) {
      veterX <- vreme$zonalniVeter[, , dan]
      veterY <- vreme$meridionalniVeter[, , dan]
      natancnost <-
        max(ceiling(24 * parametri$trenje * max(veterX ^ 2 + veterY ^ 2) ^ 0.5), 1)
      for (korak in 1:natancnost) {
        stanje <-
          simulirajKorak(
            dan,
            stanje,
            list(
              veterX = veterX,
              veterY = veterY,
              temperatura = vreme$temperatura[, , dan]
            ),
            parametri,
            natancnost
          )
        text <-
          paste0("Dan ", dan, " / ", parametri$opazovalniCasOkuzbe)
        detail <- paste0("", round(100 * korak / natancnost), "%")
        if (is.function(updateProgress)) {
          updateProgress(
            detail = detail,
            message = text,
            value = ((dan - 1) * natancnost + korak) / (parametri$opazovalniCasOkuzbe * natancnost)
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
         file = file.path(mapaIzhodnihPodatkov, parametri$imeIzhodneDatoteke))
    return(parametri$imeIzhodneDatoteke)
  }
