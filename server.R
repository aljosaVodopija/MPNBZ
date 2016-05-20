function(input, output, session) {
  razpon <- function(razpredelnice, stolpec) {
    maksi <-
      max(sapply(razpredelnice, function(dan)
        max(dan[[stolpec]])))
    return(maksi)
  }
  
  # Rezultati simulacije
  podatki <- reactive({
    req(input$imeVhodneDatoteke)
    env <- new.env()
    load(file.path(mapaIzhodnihPodatkov, input$imeVhodneDatoteke),
         envir = env)
    updateSliderInput(session,
                      "dan",
                      max = length(env$zgodovina) - 1)
    env$zgodovina
  })
  
  simuli <- observe({
    if (input$pozeniSimulacijo == 0)
      return ()
    parametri <- isolate(
      list(
        krajiOkuzbe = input$krajiOkuzbe,
        zacetnoSteviloOkuzenih = input$zacetnoSteviloOkuzenih,
        steviloMuhNaGovedo = input$steviloMuhNaGovedo,
        steviloMuhNaDrobnico = input$steviloMuhNaDrobnico,
        natalitetaMuh = input$natalitetaMuh / 100,
        prenosGostiteljNaVektor = input$prenosGostiteljNaVektor / 100,
        stopnjaUgrizov = input$stopnjaUgrizov / 100,
        prenosVektorNaGostitelj = input$prenosVektorNaGostitelj / 100,
        trenje = input$trenje,
        opazovalniCasOkuzbe = input$opazovalniCasOkuzbe,
        imeIzhodneDatoteke = input$imeIzhodneDatoteke
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
      c("Izberite model..." = "", dir(path = mapaIzhodnihPodatkov))
    updateSelectInput(
      session,
      inputId = "imeVhodneDatoteke",
      choices = datoteke,
      selected = ime
    )
    updateTabsetPanel(session, "tabi", "prikaz")
  })
  
  # Podatki dneva
  podatkiDneva <- reactive({
    podatki()[[min(input$dan + 1, length(podatki()))]]
  })
  
  # Barvna paleta
  paletaOkuzeneZivali <- reactive({
    colorNumeric(colorRamp(c("#fee0d2", "#de2d26")),
                 c(0, razpon(podatki(), "okuzenoGovedo") + razpon(podatki(), "okuzenaDrobnica")),
                 na.color = "#00000000")
  })
  
  paletaZdraveZivali <- reactive({
    colorNumeric(colorRamp(c("#e5f5e0", "#31a354")),
                 c(0, razpon(podatki(), "zdravoGovedo") + razpon(podatki(), "zdravaDrobnica")),
                 na.color = "#00000000")
  })
  
  paletaOkuzenihMuh <- reactive({
    colorNumeric(colorRamp(c("#deebf7", "#3182bd")),
                 c(0, razpon(podatki(), "okuzeneMuhe")),
                 na.color = "#00000000")
  })
  
  # Osnovni zemljevid
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(
        lng1 = lonRange$min,
        lat1 = latRange$min,
        lng2 = lonRange$max,
        lat2 = latRange$max
      )
  })
  
  # Raster
  observe({
    okuzeneMuhe <- podatkiDneva()$okuzeneMuhe
    okuzeneMuhe[okuzeneMuhe == 0] <- NA
    okuzeneZivali <- podatkiDneva()$okuzenoGovedo + podatkiDneva()$okuzenaDrobnica
    okuzeneZivali[okuzeneZivali == 0] <- NA
    zdraveZivali <- podatkiDneva()$zdravoGovedo + podatkiDneva()$zdravaDrobnica
    zdraveZivali[zdraveZivali == 0] <- NA
    leafletProxy("map") %>%
      clearGroup("raster") %>%
      addRasterImage(
        raster(
          okuzeneMuhe,
          xmn = lonRange$min,
          xmx = lonRange$max,
          ymn = latRange$min,
          ymx = latRange$max,
          crs = "+init=epsg:4326"
        ),
        colors = paletaOkuzenihMuh(),
        opacity = 0.9,
        group = "raster"
      ) %>%
      addRasterImage(
        raster(
          zdraveZivali,
          xmn = lonRange$min,
          xmx = lonRange$max,
          ymn = latRange$min,
          ymx = latRange$max,
          crs = "+init=epsg:4326"
        ),
        colors = paletaZdraveZivali(),
        opacity = 0.9,
        group = "raster"
      ) %>%
      addRasterImage(
        raster(
          okuzeneZivali,
          xmn = lonRange$min,
          xmx = lonRange$max,
          ymn = latRange$min,
          ymx = latRange$max,
          crs = "+init=epsg:4326"
        ),
        colors = paletaOkuzeneZivali(),
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
        pal = paletaOkuzenihMuh(),
        values = c(0, razpon(podatki(), "okuzeneMuhe")),
        title = "Okužene muhe"
      ) %>%
      addLegend(
        position = "bottomright",
        pal = paletaOkuzeneZivali(),
        values = c(0, razpon(podatki(), "okuzenoGovedo") + razpon(podatki(), "okuzenaDrobnica")),
        title = "Okužene živali"
      ) %>%
      addLegend(
        position = "bottomright",
        pal = paletaZdraveZivali(),
        values = c(0, razpon(podatki(), "zdravoGovedo") + razpon(podatki(), "zdravaDrobnica")),
        title = "Zdrave živali"
      )
  })
}
