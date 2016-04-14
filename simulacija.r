indeks.kraja <- function(mesto) {
  kode = read.csv('vmesni-podatki/koordinateKrajev.csv', row.names = 1)
  mesto = toupper(mesto)
  if (!mesto %in% rownames(kode)) {
    kode[mesto,] = geocode(paste(mesto, "SLOVENIJA"))
  }
  write.csv(kode, 'vmesni-podatki/koordinateKrajev.csv')
  koordinate <- kode[mesto,]
  stolpec <- round((koordinate$lon - x.lim[1]) / dx + 1 / 2)
  vrstica <- round((y.lim[2] - koordinate$lat) / dy + 1 / 2)
  return(matrix(c(vrstica, stolpec), ncol = 2))
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

simuliraj.dan <-
  function(dan,
           stanje,
           vreme) {
    govedo <- stanje$zdrava.goveda + stanje$okuzena.goveda
    drobnica <- stanje$zdrava.drobnica + stanje$okuzena.drobnica
    
    # Število pikov
    stevilo.pikov.govedo <-
      stopnja.ugrizov * stanje$okuzene.muhe * (nagnjenost / (nagnjenost + 1)) * (1 / (govedo + drobnica))
    stevilo.pikov.govedo[govedo + drobnica == 0] <- 0
    stevilo.pikov.drobnica <-
      stopnja.ugrizov * stanje$okuzene.muhe * (1 / (nagnjenost + 1)) * (1 / (govedo + drobnica))
    stevilo.pikov.drobnica[govedo + drobnica == 0] <- 0
    novo.okuzena.goveda <-
      round(stanje$zdrava.goveda * (1 - (1 - prenos.vektor.na.gostitelj) ^ stevilo.pikov.govedo))
    novo.okuzena.drobnica <-
      round(stanje$zdrava.drobnica * (1 - (1 - prenos.vektor.na.gostitelj) ^ stevilo.pikov.drobnica))
    
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
        stanje$zdrave.muhe * prenos.gostitelj.na.vektor * stopnja.ugrizov * (stanje$okuzena.drobnica + stanje$okuzena.goveda) / (govedo + drobnica)
      )
    novo.okuzene.muhe[govedo + drobnica == 0] <- 0
    stanje$zdrave.muhe <- stanje$zdrave.muhe - novo.okuzene.muhe
    stanje$okuzene.muhe <- stanje$okuzene.muhe + novo.okuzene.muhe
    
    # Nataliteta muh
    gamma <-
      nataliteta.muh * vreme$temperatura * (vreme$temperatura - 10.4) * sin(dan / opazovalni.cas.okuzbe * 2 * pi)
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
                   dt)
    stanje$okuzene.muhe <-
      preseli.muhe(stanje$okuzene.muhe,
                   vreme$veter.x,
                   vreme$veter.y,
                   dt)
    stanje$zdrave.muhe <- stanje$zdrave.muhe * matrikaNicel
    stanje$okuzene.muhe <- stanje$okuzene.muhe * matrikaNicel
    return(stanje)
  }

simuliraj <-
  function (updateProgress = NULL) {
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
    for (kraj in kraji.okuzbe) {
      stanje$okuzena.goveda[indeks.kraja(kraj)] <- stevilo.okuzenih
    }
    
    # Začetno število zdravih in okuženih muh
    stanje$zdrave.muhe <-
      stanje$zdrava.goveda * stevilo.muh.na.govedo + stanje$zdrava.drobnica * stevilo.muh.na.drobnico
    stanje$okuzene.muhe <-
      stanje$okuzena.goveda * stevilo.muh.na.govedo + stanje$okuzena.drobnica * stevilo.muh.na.drobnico
    
    # Seznam stanj za vsak dan
    zgodovina <- as.list(1:(opazovalni.cas.okuzbe + 1))
    zgodovina[[1]] <- stanje
    for (dan in 1:opazovalni.cas.okuzbe) {
      for (korak in 1:natancnost) {
        stanje <-
          simuliraj.dan(
            dan,
            stanje,
            list(
              veter.x = zonalniVeter[, , dan],
              veter.y = meridionalniVeter[, , dan],
              temperatura = temperatura[, , dan]
            )
          )
        text <- paste0("Dan ", dan, " / ", opazovalni.cas.okuzbe)
        detail <- paste0("", round(100 * korak / natancnost), "%")
        if (is.function(updateProgress)) {
          updateProgress(
            detail = detail,
            message = text,
            value = ((dan - 1) * natancnost + korak) / (opazovalni.cas.okuzbe * natancnost)
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
    return(zgodovina)
  }

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body { width: 100%; height: 100% }"),
  leafletOutput(
    outputId = "map",
    width = "100%",
    height = "100%"
  ),
  absolutePanel(
    sliderInput(
      inputId = "dan",
      label = "Dan",
      min = 0,
      max = opazovalni.cas.okuzbe,
      value = 0,
      step = 1
    ),
    top = 10,
    right = 10
  )
)

server <- function(input, output, session) {
  razpon <- function(razpredelnice, stolpec) {
    mini <-
      min(sapply(razpredelnice, function(dan)
        min(dan[[stolpec]])))
    maksi <-
      max(sapply(razpredelnice, function(dan)
        max(dan[[stolpec]])))
    return(c(mini, maksi))
  }
  
  # Rezultati simulacije
  podatki <- reactive({
    progress <- shiny::Progress$new()
    updateProgress <-
      function(value = NULL,
               detail = NULL,
               message = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
        }
        progress$set(value = value,
                     detail = detail,
                     message = message)
      }
    on.exit(progress$close())
    progress$set(message = "Zaganjam simulacijo", value = 0)
    simuliraj(updateProgress)
  })
  
  # Podatki dneva
  podatki.dneva <- reactive({
    podatki()[[input$dan + 1]]
  })
  
  # Barvna paleta
  paleta.okuzena.goveda <- reactive({
    colorNumeric(colorRamp(c("#fee0d2", "#de2d26")),
                 razpon(podatki(), "okuzena.goveda"),
                 na.color = "#00000000")
  })
  
  paleta.zdrava.goveda <- reactive({
    colorNumeric(colorRamp(c("#e5f5e0", "#31a354")),
                 razpon(podatki(), "zdrava.goveda"),
                 na.color = "#00000000")
  })
  
  paleta.okuzenih.muh <- reactive({
    colorNumeric(colorRamp(c("#deebf7", "#3182bd")),
                 razpon(podatki(), "okuzene.muhe"),
                 na.color = "#00000000")
  })
  
  # Osnovni zemljevid
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(
        lng1 = x.lim[1],
        lat1 = y.lim[1],
        lng2 = x.lim[2],
        lat2 = y.lim[2]
      )
  })
  
  # Raster
  observe({
    okuzene.muhe <- podatki.dneva()$okuzene.muhe
    okuzene.muhe[okuzene.muhe == 0] <- NA
    okuzena.goveda <- podatki.dneva()$okuzena.goveda
    okuzena.goveda[okuzena.goveda == 0] <- NA
    zdrava.goveda <- podatki.dneva()$zdrava.goveda
    zdrava.goveda[zdrava.goveda == 0] <- NA
    leafletProxy("map") %>%
      clearGroup("raster") %>%
      addRasterImage(
        raster(
          okuzene.muhe,
          xmn = x.lim[1],
          xmx = x.lim[2],
          ymn = y.lim[1],
          ymx = y.lim[2],
          crs = "+init=epsg:4326"
        ),
        colors = paleta.okuzenih.muh(),
        opacity = 0.9,
        group = "raster"
      ) %>%
      addRasterImage(
        raster(
          zdrava.goveda,
          xmn = x.lim[1],
          xmx = x.lim[2],
          ymn = y.lim[1],
          ymx = y.lim[2],
          crs = "+init=epsg:4326"
        ),
        colors = paleta.zdrava.goveda(),
        opacity = 0.9,
        group = "raster"
      ) %>%
      addRasterImage(
        raster(
          okuzena.goveda,
          xmn = x.lim[1],
          xmx = x.lim[2],
          ymn = y.lim[1],
          ymx = y.lim[2],
          crs = "+init=epsg:4326"
        ),
        colors = paleta.okuzena.goveda(),
        opacity = 0.9,
        group = "raster"
      )
  })
  
  # Legenda
  observe({
    leafletProxy("map") %>%
      clearControls() %>%
      addLegend(
        position = "bottomleft",
        pal = paleta.okuzenih.muh(),
        values = razpon(podatki(), "okuzene.muhe"),
        title = "Okužene muhe"
      ) %>%
      addLegend(
        position = "bottomright",
        pal = paleta.okuzena.goveda(),
        values = razpon(podatki(), "okuzena.goveda"),
        title = "Okuženo govedo"
      ) %>%
      addLegend(
        position = "bottomright",
        pal = paleta.zdrava.goveda(),
        values = razpon(podatki(), "zdrava.goveda"),
        title = "Zdravo govedo"
      )
  })
}
