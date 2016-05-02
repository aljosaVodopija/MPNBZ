function(input, output, session) {
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
    req(input$ime.datoteke)
    env <- new.env()
    load(file.path("izhodni-podatki", input$ime.datoteke), envir = env)
    updateSliderInput(session,
                      "dan",
                      max = length(env$zgodovina) - 1)
    env$zgodovina
  })
  
  simuli <- observe({
    if (input$pozeni.simulacijo == 0)
      return ()
    parametri <- isolate(
      list(
        kraji.okuzbe = input$kraji.okuzbe,
        zacetno.stevilo.okuzenih = input$zacetno.stevilo.okuzenih,
        stevilo.muh.na.govedo = input$stevilo.muh.na.govedo,
        stevilo.muh.na.drobnico = input$stevilo.muh.na.drobnico,
        nataliteta.muh = input$nataliteta.muh / 100,
        prenos.gostitelj.na.vektor = input$prenos.gostitelj.na.vektor / 100,
        stopnja.ugrizov = input$stopnja.ugrizov / 100,
        prenos.vektor.na.gostitelj = input$prenos.vektor.na.gostitelj / 100,
        trenje = input$trenje,
        opazovalni.cas.okuzbe = input$opazovalni.cas.okuzbe,
        ime.datoteke = input$ime.shrani
      )
    )
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
    ime <- simuliraj(parametri, updateProgress)
    datoteke <-
      c("Izberite model..." = "", dir(path = "izhodni-podatki"))
    updateSelectInput(
      session,
      inputId = "ime.datoteke",
      choices = datoteke,
      selected = ime
    )
    updateTabsetPanel(session, "tabi", "prikaz")
  })
  
  # Podatki dneva
  podatki.dneva <- reactive({
    podatki()[[min(input$dan + 1, length(podatki()))]]
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
