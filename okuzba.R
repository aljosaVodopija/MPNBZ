# Priprava ----------------------------------------------------------------

# Po potrebi je treba namestiti pakete s spodnjim ukazom:
# install.packages(c("ggmap", "leaflet"))

# Naložimo knjižnice za delo z zemljevidi
library(leaflet)
library(RColorBrewer)

# Naložimo pomožne funkcije
source("funkcije.r")

# Naložimo vnaprej izračunane podatke. Če jih ni, moramo najprej pognati
# program "predpriprava.r"
load("vmesni-podatki/zonalniVeter.RData")
load("vmesni-podatki/meridionalniVeter.RData")
load("vmesni-podatki/temperatura.RData")
load("vmesni-podatki/matrikaNicel.RData")
load("vmesni-podatki/goveda.RData")
load("vmesni-podatki/drobnica.RData")
load("vmesni-podatki/prasici.RData")


# Risanje zemljevidov -----------------------------------------------------

# Če želimo, lahko s spodnjimi ukazi narišemo zemljevide gospodarstev
narisi(drobnica$lon, drobnica$lat, sqrt(drobnica$gospodarstva), "red")
narisi(prasici$lon, prasici$lat, sqrt(prasici$gospodarstva) / 3, "blue")
narisi(govedo$lon, govedo$lat, sqrt(govedo$gospodarstva) / 2, "green")

x.lim <- c(13.5 - 1 / 90, 16.5 + 1 / 90)
y.lim <- c(45.2 + 1 / 30 - 1.5 / 120,  47 + 1.5 / 120)
dx <- 1 / 90
dy <- 1 / 120
st.vrstic <- round((y.lim[2] - y.lim[1]) / dy)
st.stolpcev <- round((x.lim[2] - x.lim[1]) / dx)
dimenzije <- c(st.vrstic + 1, st.stolpcev + 1)


razpredelnica.v.matriko <- function(razpredelnica) {
  matrika <- matrix(0, nrow = st.vrstic, ncol = st.stolpcev)
  for(indeks.vnosa in 1:nrow(razpredelnica)) {
    vnos <- razpredelnica[indeks.vnosa, ]
    stolpec <- round((vnos$lon - x.lim[1]) / dx + 1 / 2)
    vrstica <- round((y.lim[2] - vnos$lat) / dy + 1 / 2)
    if(1 <= vrstica && vrstica <= st.vrstic && 1 <= stolpec && stolpec <= st.stolpcev)
      matrika[vrstica, stolpec] <- matrika[vrstica, stolpec] + vnos$mag
  }
  return(matrika)
}

matrika.v.razpredelnico <- function(matrika) {
  return(data.frame(
    lon = as.vector(x.lim[1] + (col(matrika) - 1 / 2) * dx),
    lat = as.vector(y.lim[2] - (row(matrika) - 1 / 2) * dy),
    mag = as.vector(matrika)
  ))
}


#####################################



############################################
############ dinamicen del #################


##############################################

koord2indeks <- function(vnos) {
  stolpec <- round((vnos$lon - x.lim[1]) / dx + 1 / 2)
  vrstica <- round((y.lim[2] - vnos$lat) / dy + 1 / 2)
  return(matrix(c(vrstica, stolpec), ncol = 2))
}

popravi.rob <- function(matrika) {
  matrika[1,] = matrika[2,]
  matrika[nrow(matrika),] = matrika[nrow(matrika) - 1,]
  matrika[,1] = matrika[,2]
  matrika[, ncol(matrika)] = matrika[, ncol(matrika) - 1]
  return(matrika)
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
  muhe <- muhe + dt * (
    (veter.x + levo(veter.x)) / 2 * (muhe + levo(muhe)) / 2
    - (veter.x + desno(veter.x)) / 2 * (muhe + desno(muhe)) / 2
    + (veter.y + spodaj(veter.y)) / 2 * (muhe + spodaj(muhe)) / 2
    - (veter.y + zgoraj(veter.y)) / 2 * (muhe + zgoraj(muhe)) / 2
  )
  return(muhe)
}
################################################

# Dinamični del -----------------------------------------------------------

simuliraj <- function (kraji_okuzbe = c("Grosuplje"), stevilo_okuzenih = 200, st_muh=900, gamma0=0.5, prenos.govedo.na.muho=1,
                       stevilo.dni = 30, prenos.muha.na.govedo = 0.9) {
#   ############ Parametri #######################
#   st_muh <- 900 ## stevilo muh na eno žival
#   gamma <- 0 ### stopnja natalitete muh
#   stevilo.dni = 30### opazovalni cas okuzbe
#   
#   #######################################################
#   # če je več izbruhov, kopiramo tale del kode ###
#   ######################################################
#   ### Parametra naj bi bil mesto okuzbe - npr: Grosuplje
#   #####################################################
#   kraji_okuzbe <- c("Koper") ### lokacija, na kateri nastane okužba
#   
#   ########### Paramerer-stevilo okuzenih ##############
#   stevilo_okuzenih <- 200 ### stevilo okuzenih na dani lokaciji
  zdrave_muhe <- array(0, dim=dimenzije)
  okuzene_muhe <- array(0, dim=dimenzije)
  zdrava_goveda <- array(0, dim=dimenzije)
  okuzena_goveda <- array(0, dim=dimenzije)
  
  ### Govedo #### 
  for(k in 1:length(govedo$lon)){
    indeks = koord2indeks(govedo[k, ])
    zdrava_goveda[indeks] <- zdrava_goveda[indeks] + govedo$stevilo[k]
  }
  ####################################################
  for(kraj in kraji_okuzbe) {
    mesto_okuzbe = geocode.cache(kraj)
    indeks = koord2indeks(mesto_okuzbe)
    zdrava_goveda[indeks] <- zdrava_goveda[indeks] - stevilo_okuzenih
    okuzena_goveda[indeks] <- stevilo_okuzenih
  }
  #################################################
  zdrave_muhe <- (okuzena_goveda + zdrava_goveda) * st_muh

  
  
  #####################################################################3
  ############################## SISTEM DIF: ENAČB-PREPISI!!!! ##################
  zgodovina.okuzb <- array(NA, dim=c(dimenzije, stevilo.dni + 1))
  zgodovina.okuzb[,,1] <- okuzena_goveda
  for(i in 1:stevilo.dni) {
    # zonalni veter v km/h v smeri zahod-vzhod (pozitivna vrednost pomeni pihanje od zahoda proti vzhodu)
    # prvotni podatki so v smeri vzhod-zahod, zato matriko negiramo
    # TODO: preveri, ali so prvotni podatki res v smeri vzhod-zahod
    veter.x <- -t(zonalniVeter[,,i])
    # meridionalni veter v km/h v smeri jug-sever (pozitivna vrednost pomeni pihanje od juga proti severu)
    veter.y <- t(meridionalniVeter[,,i])
    # povprečna dnevna temperatura v stopinjah Celzija
    T <- t(temperatura[,,i])

    # TODO: gamma je v resnici odvisna od temperature
    gamma <- gamma0 # * sin(i / stevilo.dni * 2 * pi)
    novo_okuzene_muhe <- round(prenos.govedo.na.muho * zdrave_muhe * okuzena_goveda / (zdrava_goveda + okuzena_goveda))
    novo_okuzene_muhe[zdrava_goveda + okuzena_goveda == 0] <- 0
    novo_okuzena_goveda <- round(prenos.muha.na.govedo * zdrava_goveda * okuzene_muhe / (zdrave_muhe + okuzene_muhe))
    novo_okuzena_goveda[zdrave_muhe + okuzene_muhe == 0] <- 0
    
    zdrave_muhe <- zdrave_muhe + gamma * zdrave_muhe - novo_okuzene_muhe
    okuzene_muhe <- okuzene_muhe + gamma * okuzene_muhe + novo_okuzene_muhe

    stevilo.selitev <- 1
    for(i in 1:stevilo.selitev) {
      dt <- 24 / stevilo.selitev
      zdrave_muhe <- preseli.muhe(zdrave_muhe, veter.x, veter.y, dt)
      okuzene_muhe <- preseli.muhe(okuzene_muhe, veter.x, veter.y, dt)
    }
    zdrave_muhe <- zdrave_muhe * t(matrikaNicel)
    okuzene_muhe <- okuzene_muhe * t(matrikaNicel)

    zdrava_goveda <- zdrava_goveda - novo_okuzena_goveda
    okuzena_goveda <- okuzena_goveda + novo_okuzena_goveda

    #stopifnot(any(zdrave_muhe < 0))
    #stopifnot(any(okuzene_muhe < 0))
    #stopifnot(any(zdrava_goveda < 0))
    #stopifnot(any(okuzena_goveda < 0))
    
    zgodovina.okuzb[,,i + 1] <- okuzena_goveda
    zdrava_goveda <- levo(zdrava_goveda)
    #print(sum(okuzene_muhe))
  }
  return(zgodovina.okuzb)
}

library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("dan", "Dan", 1, 31,
                            value = 1, step = 1
                ),
                sliderInput("b", "Verjetnost prenosa okužbe muha=>govedo", 0, 1,
                            value = 0.9, step = 0.05
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                sliderInput("c2", "Verjetnost prenosa okužbe govedo=>muha", 0, 1,
                            value = 1, step = 0.05
                )
  )
)

server <- function(input, output, session) {
  podatki <- reactive({
    pod <- simuliraj(
      prenos.muha.na.govedo = input$b,
      prenos.govedo.na.muho = input$c2,
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
