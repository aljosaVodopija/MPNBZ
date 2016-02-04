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
  kode = read.csv('kode.csv', row.names = 1)
  for(mesto in mesta) {
    mesto = toupper(mesto)
    if(!mesto %in% rownames(kode)) {
      polozaj = geocode(paste(mesto, "SLOVENIJA"))
      kode[mesto, ] = polozaj
    }
  }
  write.csv(kode, 'kode.csv')
  return(kode[toupper(mesta), ])
}

####### naložimo podatke #####
drobnica <- naloziZivali("drobnica_stalez_3l.txt", 3)
save(drobnica, file = 'drobnica.txt')
narisi(drobnica, "red", 200)
shraniZemljevid(drobnica, "drobnica.png", "red", 50)

prasici <- naloziZivali("prasici_stalez_3l.txt", 3)
save(prasici, file = 'prasici.txt')
narisi(prasici, "blue", 700)
shraniZemljevid(prasici, "prasici.png", "blue", 250)

govedo <- naloziZivali("govedo_3leta.txt", 5)
save(govedo, file = 'goveda.txt')
narisi(govedo, "green", 500)
shraniZemljevid(govedo, "govedo.png", "purple", 150)
