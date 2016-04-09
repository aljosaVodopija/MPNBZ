# Priprava ----------------------------------------------------------------

# Po potrebi je treba namestiti pakete s spodnjim ukazom:
# install.packages(c("ggmap", "leaflet", "raster"))

# Naložimo knjižnice za delo z zemljevidi
library(shiny)
library(leaflet)
library(RColorBrewer)
library(raster)


# Naložimo vnaprej izračunane podatke. Če jih ni, moramo najprej pognati
# program "predpriprava.r"
load("vmesni-podatki/zonalniVeter.RData")
load("vmesni-podatki/meridionalniVeter.RData")
load("vmesni-podatki/temperatura.RData")
load("vmesni-podatki/matrikaNicel.RData")
load("vmesni-podatki/goveda.RData")

# Naložimo pomožne funkcije
source("funkcije.r")
source("simulacija.r")

# Risanje zemljevidov -----------------------------------------------------

# Če želimo, lahko s spodnjimi ukazi narišemo zemljevide gospodarstev
# narisi(drobnica$lon, drobnica$lat, sqrt(drobnica$gospodarstva), "red")
# narisi(prasici$lon, prasici$lat, sqrt(prasici$gospodarstva) / 3, "blue")
# narisi(govedo$lon, govedo$lat, sqrt(govedo$gospodarstva) / 2, "green")


#####################################


############################################
############ dinamicen del #################


##############################################

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body { width: 100%; height: 100% }"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("dan", "Dan", 1, 31,
                            value = 1, step = 1
                ),
                sliderInput("prenos.muha.na.govedo", "Verjetnost prenosa okužbe muha=>govedo", 0, 1,
                            value = 0.9, step = 0.05
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                sliderInput("prenos.govedo.na.muho", "Verjetnost prenosa okužbe govedo=>muha", 0, 1,
                            value = 1, step = 0.05
                )
  )
)

server <- function(input, output, session) {
  podatki <- reactive({
    pod <- simuliraj(
      prenos.muha.na.govedo = input$prenos.muha.na.govedo,
      prenos.govedo.na.muho = input$prenos.govedo.na.muho,
      kraji_okuzbe=c("Metlika", "Koper", "Ptuj")
    )
    pod
  })

  filteredData <- reactive({
    podatki()[,,input$dan]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, filteredData())
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(x.lim[1], y.lim[1], x.lim[2], y.lim[2])
  })
  
  observe({
    data <- filteredData()
    map <- leafletProxy("map")
    r <- raster(data, xmn=x.lim[1], xmx=x.lim[2], ymn=y.lim[1], ymx=y.lim[2])
    crs(r) <- CRS("+init=epsg:4326")
    map %>% clearGroup("rastko")
    map %>% addRasterImage(r, opacity = 0.7, colors=colorpal(), group = "rastko")
  })

# Use a separate observer to recreate the legend as needed.
  observe({
    data <- filteredData()
    proxy <- leafletProxy("map")
#
#     # Remove any existing legend, and only if the legend is
#     # enabled, create a new one.
    proxy %>% clearControls()
      proxy %>% addLegend(position = "bottomright",
                          pal = colorpal(), values = data
      )
  })
}

shinyApp(ui, server)
