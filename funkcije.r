library(ggmap)
library(leaflet)

x.lim <- c(13.5 - 1 / 90, 16.5 + 1 / 90)
y.lim <- c(45.2 + 1 / 30 - 1.5 / 120,  47 + 1.5 / 120)
dx <- 1 / 90
dy <- 1 / 120
st.vrstic <- round((y.lim[2] - y.lim[1]) / dy) + 1
st.stolpcev <- round((x.lim[2] - x.lim[1]) / dx) + 1
dimenzije <- c(st.vrstic, st.stolpcev)

razpon <- function(razpredelnice, stolpec) {
  mini <- min(sapply(razpredelnice, function(dan) min(dan[[stolpec]])))
  maksi <- max(sapply(razpredelnice, function(dan) max(dan[[stolpec]])))
  razpon <- signif(c(mini, maksi), 0)
  return(razpon)
}

razpredelnica.v.matriko <- function(razpredelnica) {
  matrika <- matrix(0, nrow = st.vrstic, ncol = st.stolpcev)
  for(indeks.vnosa in 1:nrow(razpredelnica)) {
    vnos <- razpredelnica[indeks.vnosa, ]
    stolpec <- round((vnos$lon - x.lim[1]) / dx + 1 / 2)
    vrstica <- round((y.lim[2] - vnos$lat) / dy + 1 / 2)
    if(1 <= vrstica && vrstica <= st.vrstic && 1 <= stolpec && stolpec <= st.stolpcev)
      matrika[vrstica, stolpec] <- matrika[vrstica, stolpec] + vnos$stevilo
  }
  return(matrika)
}

naloziZivali <- function(datoteka, izpustiVrstice) {
  stalez <- read.table(datoteka, fill = TRUE, col.names = c(".", "kraj", ".", ".", "x", "y", ".", "stevilo"), fileEncoding = "windows-1250", dec = ",", colClasses = c
                       ("NULL", "character", "NULL", "NULL", "double", "double", "NULL", "double"), as.is = TRUE, skip = izpustiVrstice)
  stalez$lon.kraja <- NA
  stalez$lat.kraja <- NA
  for(kraj in unique(stalez$kraj)) {
    koordinate.kraja <- geocode.cache(kraj)
    stalez$lon.kraja[stalez$kraj == kraj] <- as.numeric(koordinate.kraja$lon)
    stalez$lat.kraja[stalez$kraj == kraj] <- as.numeric(koordinate.kraja$lat)
  }
  lon <- fitted(lm(lon.kraja ~ y, stalez))
  lon <- (lon - min(lon)) * 8 / 6.4 + min(lon) - 1.3
  lat <- fitted(lm(lat.kraja ~ x, stalez))
  stalez$stevilo[is.na(stalez$stevilo)] <- 0
  return(data.frame(
    stevilo = stalez$stevilo,
    lat = lat,
    lon = lon
  )) 
}

nastaviVeter <- function (u, N, M) {

  dim <- c((N - 2) * 3, (M - 2) * 4)
  X <- matrix(rep(0, dim[1] * dim[2]), dim[1])
  for (i in 2:(N - 1)) {
    for (j in 2:(M - 1)) {
      B <- matrix(rep(0, 4*3), 3)

      B[1, 1] <- 1/2*(u[(i - 1), (j - 1)] + u[i, j])
      B[1, 2] <- 1/2*(u[(i - 1), j] + u[i, j])
      B[1, 3] <- 1/2*(u[(i - 1), j] + u[i, j])
      B[1, 4] <- 1/2*(u[(i - 1), (j + 1)] + u[i, j])

      B[2, 1] <- 1/2*(u[i, (j - 1)] + u[i, j])
      B[2, 2] <- u[i, j]
      B[2, 3] <- u[i, j]
      B[2, 4] <- 1/2*(u[i, (j + 1)] + u[i, j])

      B[3, 1] <- 1/2*(u[(i + 1), (j - 1)] + u[i, j])
      B[3, 2] <- 1/2*(u[(i + 1), j] + u[i, j])
      B[3, 3] <- 1/2*(u[(i + 1), j] + u[i, j])
      B[3, 4] <- 1/2*(u[(i + 1), (j + 1)] + u[i, j])

      X[(3 * (i - 2) + 1):(3 * (i - 2) + 3), (4 * (j - 2) + 1):(4 * (j - 2) + 4)] <- B
    }
  }
  return(X)
}

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

koord2indeks <- function(vnos) {
  stolpec <- round((vnos$lon - x.lim[1]) / dx + 1 / 2)
  vrstica <- round((y.lim[2] - vnos$lat) / dy + 1 / 2)
  return(matrix(c(vrstica, stolpec), ncol = 2))
}


