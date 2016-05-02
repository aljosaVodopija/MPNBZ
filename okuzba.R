# Priprava ----------------------------------------------------------------

# Po potrebi je treba namestiti pakete s spodnjim ukazom:
# install.packages(c("shiny", "leaflet", "raster"))

# Naložimo knjižnice za delo z zemljevidi
library(shiny)
library(leaflet)
library(raster)

# Naložimo vnaprej izračunane podatke.
# Če jih ni, moramo najprej pognati program "predpriprava.r"
# source("predpriprava.R")
load("vmesni-podatki/zonalniVeter.RData")
load("vmesni-podatki/meridionalniVeter.RData")
load("vmesni-podatki/temperatura.RData")
load("vmesni-podatki/matrikaNicel.RData")
load("vmesni-podatki/govedo.RData")
load("vmesni-podatki/drobnica.RData")

# Naložimo pomožne funkcije
source("pomozne.R")
source("simulacija.R")
source("server.R")
source("ui.R")

runApp()
