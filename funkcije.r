library(ggmap)
library(leaflet)

naloziZivali <- function(datotekaPremikov, izpustiVrstice) {
    premiki <- read.table(datotekaPremikov, fill = TRUE)
    premiki <- premiki[izpustiVrstice:length(premiki[, 1]),]

    obcine <- sapply(premiki$V2, as.character)
    obcine <- iconv(obcine, to="windows-1251")
    prestej <- premiki$V7
    prestej <- data.matrix(prestej)
    bum <-iconv(obcine, to="windows-1251")
    bum <- as.data.frame(table(bum))

    stevilo <-  rep(0, length(bum$bum))

    for(i in 1:length(bum$bum)){
      stevilo[i] = sum(na.omit(as.numeric(prestej[which(obcine==bum$bum[i])])))
    }

    lon <- rep(0, length(bum$bum))
    lat <- rep(0, length(bum$bum))

    for(i in 1:length(bum$bum)){
      mesto <- bum$bum[i]
      lon[i] <- as.numeric(geocode.cache(mesto)$lon)
      lat[i] <- as.numeric(geocode.cache(mesto)$lat)
    }

    gospodarstva <- as.numeric(bum$Freq)


    tabela <- data.frame(lon = lon, lat = lat, stevilo = stevilo, gospodarstva = gospodarstva)
    rownames(tabela) <- as.character(bum$bum)
    return(tabela[tabela$stevilo > 0,])
}

narisi <- function(lon, lat, cex, barva) {
    ## zemljevid slovenije
    m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(lng=lon, lat=lat, radius=cex, color=barva)
    m  # Print the map
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
