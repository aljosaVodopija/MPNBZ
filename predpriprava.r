source("nastaviMatrikoNicel.R")
neugodnePovrsine <- c(1, 2, 3, 4, 5, 11, 15, 17)
maxVisina <- 1000
nastaviMatrikoNicel(maxVisina, neugodnePovrsine)

source("nastaviVreme.R")
nastaviVreme("vhodni-podatki/20150530.nc", 1)
