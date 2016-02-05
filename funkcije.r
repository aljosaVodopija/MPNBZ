#Zacetne nastavitve
hx = 0.9 #longitude razdalja
hy = 1 #latitude razdalja


#DIV izracuna diskretno divergenco F=(F1,F2) (vektorsko polje vetra)
#Prejme metriko X (jakost zonalnega vetra) in matriko Y (jakost meridionalnega vetra),
#pri tem velja dim(Y)=dim(X). 
#Izračuna matriki F1x in F2y ter vrne matriko F1x + F2y. 

DIV = function(X,Y){

return(odvodx(X,hx)+odvody(Y,hy))
}

#GRADF izracuna s. p. med grad(m)*F (m skalarno polje, F vektorsko polje)
#Prejme metriko M in matriki X, Y  
#Izračuna matriki Mx, My in vrne iskani s. p. 

GRADF = function(M,X,Y){

return(odvodx(M,hx)*X+odvody(M,hy)*Y)
}

#Diskretiziran Laplaceov operator skalarnega polja
#Prejme matriko M (skalarno polje)
#Izracuna Mxx in Myy ter vrne Mxx + Myy

LAPLACE = function(M){
 
return(odvodxx(M,hx)+odvodyy(M,hy))
}

#Odvod skalarnega polja F po y (Fy), pri tem uporablja sim. dif.

odvody = function(F,h){

n = dim(F)[1]
m = dim(F)[2]
Fy = matrix(1:(n*m),n)

Fy[,2:(m-1)] = 1/(2*h)*(F[,3:m]-F[,1:(m-2)])
Fy[,1] = 1/(2*h)*(F[,2]-F[,1])
Fy[,m] = 1/(2*h)*(F[,m]-F[,(m-1)])

return(Fy)
}

#Odvod skalarnega polja F po x (Fx), pri tem uporablja sim. dif.

odvodx = function(F,h){

n = dim(F)[1]
m = dim(F)[2]
Fx = matrix(1:(n*m),n)

Fx[2:(n-1),] = 1/(2*h)*(F[3:n,]-F[1:(n-2),])
Fx[1,] = 1/(2*h)*(F[2,]-F[1,])
Fx[n,] = 1/(2*h)*(F[n,]-F[(n-1),])

return(Fx)
}

#Drugi odvod skalrnega polja F po y (Fyy)

odvodyy = function(F,h){

n = dim(F)[1]
m = dim(F)[2]
Fyy = matrix(1:(n*m),n)

Fyy[,2:(m-1)] = 1/(h^2)*(F[,3:m]+F[,1:(m-2)]-2*F[,2:(m-1)])
Fyy[,1] = 1/(h^2)*(F[,2]-F[,1])
Fyy[,m] = 1/(h^2)*(F[,(m-1)]-F[,m])

return(Fyy)
}

#Drugi odvod skalarnega polja F po x (Fxx)

odvodxx = function(F,h){

n = dim(F)[1]
m = dim(F)[2]
Fxx = matrix(1:(n*m),n)

Fxx[2:(n-1),] = 1/(h^2)*(F[3:n,]+F[1:(n-2),]-2*F[2:(n-1),])
Fxx[1,] = 1/(h^2)*(F[2,]-F[1,])
Fxx[n,] = 1/(h^2)*(F[(n-1),]-F[n,])

return(Fxx)
}

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

narisi <- function(lon, lat, cex, barva, googleMaps = FALSE) {
    ## zemljevid slovenije
    lim <- geocode.cache(c("Salovci", "Črnomelj", "Lendava", "Kobarid", "Bovec", "Trst"))
    if (googleMaps) {
      lonn <- c(min(lim$lon), max(lim$lon)) #define our map's ylim
      latt <- c(min(lim$lat), max(lim$lat)) #define our map's xlim
      #markers = paste0("&markers=color:blue|label:S|",latt[1],", ",lonn[1],"&markers=color:",
      #                 "green|label:G|",latt[2],", ",lonn[2])
      markers <- ""
      center = c(geocode.cache("Celje")$lat, geocode.cache("Ljubljana")$lon)  #tell what point to center on
      zoom <- 8 
      #### Drobnica grafični prikaz, razmerje 1:50
      terrmap <- GetMap(center=center, zoom=zoom, markers=markers,maptype= "terrain")
      PlotOnStaticMap(terrmap, lat, lon, cex=cex,pch=20,col=barva)
    } else {
      newmap <- getMap(resolution = "high")
      plot(newmap, xlim= range(europe.limits$lon), ylim= range(europe.limits$lat), asp = 1)
      points(lon, lat, pch=19, col=barva, cex = cex)
    }
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
