# install.packages("ggmap")
# install.packages("ncdf4")
# install.packages("plotrix")
# install.packages("raster")
# install.packages("RCurl")
# install.packages("rworldmap")
library(ncdf4)
library(ggmap)
library(rworldmap)
library(RCurl)
library(RJSONIO)
library(plyr)
library(plotrix)
library(RgoogleMaps)
library(raster)

source("operatorji.R")
source("nastaviVeter.R")
source("naloziZivali.R")

geocode.cache <- function(mesta) {
  kode = read.csv('vmesni-podatki/koordinateKrajev.csv', row.names = 1)
  for(mesto in mesta) {
    mesto = toupper(mesto)
    if(!mesto %in% rownames(kode)) {
      polozaj = geocode(paste(mesto, "SLOVENIJA"))
      kode[mesto, ] = polozaj
    }
  }
  write.csv(kode, 'vmesni-podatki/koordinateKrajev.csv')
  return(kode[toupper(mesta), ])
}


####### naložimo podatke #####
drobnica <- naloziZivali("vhodni-podatki/drobnica_stalez_3l.txt", 3)
save(drobnica, file = 'vmesni-podatki/drobnica.RData')
narisi(drobnica, "red", 200)
shraniZemljevid(drobnica, "izhodni-podatki/drobnica.png", "red", 50)

prasici <- naloziZivali("vhodni-podatki/prasici_stalez_3l.txt", 3)
save(prasici, file = 'vmesni-podatki/prasici.RData')
narisi(prasici, "blue", 700)
shraniZemljevid(prasici, "izhodni-podatki/prasici.png", "blue", 250)

govedo <- naloziZivali("vhodni-podatki/govedo_3leta.txt", 5)
save(govedo, file = 'vmesni-podatki/goveda.RData')
narisi(govedo, "green", 500)
shraniZemljevid(govedo, "izhodni-podatki/govedo.png", "purple", 150)
