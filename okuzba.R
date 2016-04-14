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
x.lim <- c(13.5 - 1 / 90, 16.5 + 1 / 90)
y.lim <- c(45.2 + 1 / 30 - 1.5 / 120,  47 + 1.5 / 120)
dx <- 1 / 90
dy <- 1 / 120
kraji.okuzbe <- c("Grosuplje", "Ptuj")
stevilo.okuzenih <- 200
stevilo.muh.na.govedo <- 900
nagnjenost <- 9.4
natancnost <- 10
stevilo.muh.na.drobnico <- 900 / nagnjenost
nataliteta.muh <- ((1 + 0.0003) ^ (1 / natancnost) - 1)
prenos.gostitelj.na.vektor <- 0.01 # med 0.001 in 0.15
stopnja.ugrizov <- 0.17 / natancnost
prenos.vektor.na.gostitelj <- 0.9
opazovalni.cas.okuzbe <- 4

# Naložimo pomožne funkcije
source("simulacija.r")

# Poženemo interaktivni vmesnik
shinyApp(ui, server)
