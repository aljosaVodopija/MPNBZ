# Priprava ----------------------------------------------------------------

# Po potrebi je treba namestiti pakete s spodnjim ukazom:
# install.packages(c("shiny", "leaflet", "raster"))

# Naložimo knjižnice za delo z zemljevidi
library(shiny)
library(leaflet)
library(raster)

# Naložimo vnaprej izračunane podatke.
# Če jih ni, moramo najprej pognati program "predpriprava.r"
load("vmesni-podatki/zonalniVeter.RData")
load("vmesni-podatki/meridionalniVeter.RData")
load("vmesni-podatki/temperatura.RData")
load("vmesni-podatki/matrikaNicel.RData")
load("vmesni-podatki/govedo.RData")
load("vmesni-podatki/drobnica.RData")

# Nastavimo parametre
zonalniVeter <- 0 * zonalniVeter + 0.1
meridionalniVeter <- 0 * meridionalniVeter + 0.1
x.lim <- c(13.5 - 1 / 90, 16.5 + 1 / 90)
y.lim <- c(45.2 + 1 / 30 - 1.5 / 120,  47 + 1.5 / 120)
dx <- 1 / 90
dy <- 1 / 120
natancnost <- 10


indeks.kraja <- function(mesto) {
  kode = read.csv('vmesni-podatki/koordinateKrajev.csv', row.names = 1)
  mesto = toupper(mesto)
  if (!mesto %in% rownames(kode)) {
    kode[mesto, ] = geocode(paste(mesto, "SLOVENIJA"))
  }
  write.csv(kode, 'vmesni-podatki/koordinateKrajev.csv')
  koordinate <- kode[mesto, ]
  stolpec <- round((koordinate$lon - x.lim[1]) / dx + 1 / 2)
  vrstica <- round((y.lim[2] - koordinate$lat) / dy + 1 / 2)
  return(matrix(c(vrstica, stolpec), ncol = 2))
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
      for (korak in 1:natancnost) {
        stanje <-
          simuliraj.dan(
            dan,
            stanje,
            list(
              veter.x = zonalniVeter[, , dan],
              veter.y = meridionalniVeter[, , dan],
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
    save(zgodovina, file = file.path("izhodni-podatki", parametri$ime.datoteke))
    return(parametri$ime.datoteke)
  }

ui <- bootstrapPage(
  tags$style(
    type = "text/css",
    "div.outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0; }"
  ),
  tabsetPanel(
    id = "tabi",
    tabPanel(
      "Izračun modela",
      value = "izracun",
      fluidRow(
        column(
          3,
          offset = 1,
          sliderInput(
            inputId = "opazovalni.cas.okuzbe",
            label = "Opazovalni čas okužbe",
            min = 1,
            max = 31,
            value = 10
          ),
          textInput(
            inputId = "kraji.okuzbe",
            label = "Kraj okužbe",
            value = "Grosuplje, Ptuj"
          ),
          sliderInput(
            inputId = "zacetno.stevilo.okuzenih",
            label = "Začetno število okuženih živali",
            min = 0,
            max = 1000,
            value = 200
          )
        ),
        column(
          3,
          offset = 1,
          sliderInput(
            inputId = "stopnja.ugrizov",
            label = "Stopnja ugrizov",
            min = 0,
            max = 1,
            value = 0.17,
            step = 0.01
          ),
          sliderInput(
            inputId = "prenos.gostitelj.na.vektor",
            label = "Verjetnost prenosa z gostitelja na vektor",
            min = 0,
            max = 0.15,
            value = 0.01,
            step = 0.001
          ),
          sliderInput(
            inputId = "prenos.vektor.na.gostitelj",
            label = "Verjetnost prenosa z vektorja na gostitelj",
            min = 0,
            max = 1,
            value = 0.9,
            step = 0.01
          )
        ),
        column(
          3,
          offset = 1,
          sliderInput(
            inputId = "stevilo.muh.na.govedo",
            label = "Število muh na glavo goveda",
            min = 0,
            max = 5000,
            value = 900
          ),
          sliderInput(
            inputId = "stevilo.muh.na.drobnico",
            label = "Število muh na glavo drobnice",
            min = 0,
            max = 500,
            value = 100
          ),
          sliderInput(
            inputId = "nataliteta.muh",
            label = "Največja dnevna nataliteta muh",
            min = 0,
            max = 0.01,
            value = 0.003,
            step = 0.0001
          )
        )
      ),
      hr(),
      textInput(
        inputId = "ime.shrani",
        label = "Ime datoteke",
        value = format(Sys.time(), "Simulacija (%Y-%m-%d %H:%M).RData")
      ),
      actionButton(inputId = "pozeni.simulacijo",
                   label = "Poženi simulacijo")
    ),
    tabPanel(
      "Prikaz modela",
      value = "prikaz",
      div(
        class = "outer",
        leafletOutput(
          outputId = "map",
          width = "100%",
          height = "100%"
        ),
        absolutePanel(
          selectInput(
            inputId = "ime.datoteke",
            label = "Datoteka s simulacijo",
            choices = c("Izberite model..." = "", dir(path = "izhodni-podatki"))
          ),
          conditionalPanel(
            "input['ime.datoteke'] != ''",
            sliderInput(
              inputId = "dan",
              label = "Dan",
              min = 0,
              max = opazovalni.cas.okuzbe,
              value = 0,
              step = 1
            )
          ),
          top = 10,
          right = 10
        )
      )
    )
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
    req(input$ime.datoteke)
    env <- new.env()
    load(file.path("izhodni-podatki", input$ime.datoteke), envir = env)
    updateSliderInput(
      session,
      "dan",
      value = min(input$dan, length(env$zgodovina) - 1),
      max = length(env$zgodovina) - 1
    )
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
        nataliteta.muh = input$nataliteta.muh,
        prenos.gostitelj.na.vektor = input$prenos.gostitelj.na.vektor,
        stopnja.ugrizov = input$stopnja.ugrizov,
        prenos.vektor.na.gostitelj = input$prenos.vektor.na.gostitelj,
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

# Poženemo interaktivni vmesnik
shinyApp(ui, server)
