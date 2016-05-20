# Po potrebi je treba namestiti pakete s spodnjim ukazom:
# install.packages(c("shiny", "leaflet", "raster", "rgdal", "ggmap"))

# Naložimo knjižnice za delo z zemljevidi
library(shiny)
library(leaflet)
library(raster)
library(ggmap)

# Naložimo pomožne funkcije
source("pomozneFunkcije.R")

# Naložimo vnaprej izračunane podatke.
# Če jih ni, moramo najprej pognati program "predpriprava.r"
# source("predpriprava.R")
load("vmesni-podatki/vreme.RData")
load("vmesni-podatki/neprimernaObmocja.RData")
load("vmesni-podatki/govedo.RData")
load("vmesni-podatki/drobnica.RData")

source("simulacija.R")

source("ui.R")
source("server.R")

runApp()
