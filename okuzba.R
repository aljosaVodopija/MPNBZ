# Priprava ----------------------------------------------------------------

# Po potrebi je treba namestiti pakete s spodnjim ukazom:
# install.packages(c("shiny", "leaflet", "raster"))

# Naložimo knjižnice za delo z zemljevidi
library(shiny)
library(leaflet)
library(raster)

# Naložimo vnaprej izračunane podatke. Če jih ni, moramo najprej pognati
# program "predpriprava.r"
load("vmesni-podatki/zonalniVeter.RData")
load("vmesni-podatki/meridionalniVeter.RData")
load("vmesni-podatki/temperatura.RData")
load("vmesni-podatki/matrikaNicel.RData")
load("vmesni-podatki/goveda.RData")
stalez.drobnice <- 0 * stalez.goveda
natancnost <- 240
# Naložimo pomožne funkcije
source("simulacija.r")


# Interaktivni vmesnik ----------------------------------------------------

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
      min = 1,
      max = 31,
      value = 1,
      step = 1
    ),
    sliderInput(
      inputId = "prenos.vektor.na.gostitelj",
      label = "Verjetnost prenosa okužbe muha=>govedo",
      min = 0,
      max = 1,
      value = 0.9,
      step = 0.05
    ),
    sliderInput(
      inputId = "prenos.gostitelj.na.vektor",
      label = "Verjetnost prenosa okužbe govedo=>muha",
      min = 0,
      max = 1,
      value = 1,
      step = 0.05
    ),
    top = 10,
    right = 10
  )
)

server <- function(input, output, session) {
  razpon <- function(razpredelnice, stolpec) {
    mini <- min(sapply(razpredelnice, function(dan) min(dan[[stolpec]])))
    maksi <- max(sapply(razpredelnice, function(dan) max(dan[[stolpec]])))
    return(c(mini, maksi))
  }
  
  # Rezultati simulacije
  podatki <- reactive({
    simuliraj(
      prenos.vektor.na.gostitelj = input$prenos.vektor.na.gostitelj,
      prenos.gostitelj.na.vektor = input$prenos.gostitelj.na.vektor,
      kraji.okuzbe = c("Grosuplje", "Ptuj")
    )
  })

  # Podatki dneva
  podatki.dneva <- reactive({
    podatki()[[input$dan]]
  })
  
  # Barvna paleta
  paleta.goveda <- reactive({
    colorNumeric(colorRamp(c("#fee0d2", "#de2d26")), razpon(podatki(), "okuzena.goveda"), na.color = "#00000000")
  })
  
  paleta.zdrava.goveda <- reactive({
    colorNumeric(colorRamp(c("#e5f5e0", "#31a354")), razpon(podatki(), "zdrava.goveda"), na.color = "#00000000")
  })
  
  paleta.muh <- reactive({
    colorNumeric(colorRamp(c("#deebf7", "#3182bd")), razpon(podatki(), "okuzene.muhe"), na.color = "#00000000")
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
    muhe <- podatki.dneva()$okuzene.muhe
    muhe[muhe == 0] <- NA
    goveda <- podatki.dneva()$okuzena.goveda
    goveda[goveda == 0] <- NA
    zdrava.goveda <- podatki.dneva()$zdrava.goveda
    zdrava.goveda[zdrava.goveda == 0] <- NA
    leafletProxy("map") %>%
      clearGroup("raster") %>%
      addRasterImage(
        raster(
          muhe,
          xmn = x.lim[1],
          xmx = x.lim[2],
          ymn = y.lim[1],
          ymx = y.lim[2],
          crs = "+init=epsg:4326"
        ),
        colors = paleta.muh(),
        opacity = 0.7,
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
        opacity = 0.7,
        group = "raster"
      ) %>%
    addRasterImage(
        raster(
          goveda,
          xmn = x.lim[1],
          xmx = x.lim[2],
          ymn = y.lim[1],
          ymx = y.lim[2],
          crs = "+init=epsg:4326"
        ),
        colors = paleta.goveda(),
        opacity = 0.7,
        group = "raster"
      )
  })
  
  # Legenda
  observe({
    leafletProxy("map") %>%
      clearControls() %>%
      addLegend(position = "bottomright",
                pal = paleta.muh(),
                values = razpon(podatki(), "okuzene.muhe")) %>%
      addLegend(position = "bottomright",
                pal = paleta.goveda(),
                values = razpon(podatki(), "okuzena.goveda")) %>%
      addLegend(position = "bottomright",
                pal = paleta.zdrava.goveda(),
                values = razpon(podatki(), "zdrava.goveda"))
  })
}

shinyApp(ui, server)
