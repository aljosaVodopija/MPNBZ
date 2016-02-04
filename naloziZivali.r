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
    return(tabela)
}

narisi <- function(tabela, barva, faktor) {
    ## zemljevid slovenije
    newmap <- getMap(resolution = "high")
    europe.limits <- geocode.cache(c("Salovci", "Črnomelj", "Lendava", "Kobarid", "Bovec", "Trst"))
    plot(newmap, xlim= range(europe.limits$lon), ylim= range(europe.limits$lat), asp = 1)
    points(tabela$lon, tabela$lat, pch=19, col=barva, cex = tabela$gospodarstva/faktor)

}

shraniZemljevid <- function(tabela, imeDatoteke, barva, faktor) {
    lim<- geocode.cache(c("Salovci", "Črnomelj", "Lendava", "Kobarid", "Bovec", "Trst"))

    lonn <- c(min(lim$lon), max(lim$lon)) #define our map's ylim
    latt <- c(min(lim$lat), max(lim$lat)) #define our map's xlim
    #markers = paste0("&markers=color:blue|label:S|",latt[1],", ",lonn[1],"&markers=color:",
    #                 "green|label:G|",latt[2],", ",lonn[2])
    markers <- ""
    center = c(geocode.cache("Celje")$lat, geocode.cache("Ljubljana")$lon)  #tell what point to center on
    zoom <- 8 
    #### Drobnica grafični prikaz, razmerje 1:50
    try(terrmap <- GetMap(center=center, zoom=zoom, markers=markers,maptype= "roadmap"), silent=TRUE)
    png(imeDatoteke, type='cairo-png')
    tmp <- PlotOnStaticMap(terrmap, tabela$lat, tabela$lon, cex=tabela$gospodarstva/faktor,pch=20,col=barva)
    dev.off()
}
