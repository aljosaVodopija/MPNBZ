# install.packages("ncdf4")

geocode.cache <- function(mesta) {
  kode = read.csv('vmesni-podatki/koordinateKrajev.csv', row.names = 1)
  for (mesto in mesta) {
    mesto = toupper(mesto)
    if (!mesto %in% rownames(kode)) {
      polozaj = geocode(paste(mesto, "SLOVENIJA"))
      kode[mesto, ] = polozaj
    }
  }
  write.csv(kode, 'vmesni-podatki/koordinateKrajev.csv')
  return(kode[toupper(mesta), ])
}

nastaviVeter <- function (u, N, M) {
  dim <- c((N - 2) * 3, (M - 2) * 4)
  X <- matrix(rep(0, dim[1] * dim[2]), dim[1])
  for (i in 2:(N - 1)) {
    for (j in 2:(M - 1)) {
      B <- matrix(rep(0, 4 * 3), 3)
      
      B[1, 1] <- 1 / 2 * (u[(i - 1), (j - 1)] + u[i, j])
      B[1, 2] <- 1 / 2 * (u[(i - 1), j] + u[i, j])
      B[1, 3] <- 1 / 2 * (u[(i - 1), j] + u[i, j])
      B[1, 4] <- 1 / 2 * (u[(i - 1), (j + 1)] + u[i, j])
      
      B[2, 1] <- 1 / 2 * (u[i, (j - 1)] + u[i, j])
      B[2, 2] <- u[i, j]
      B[2, 3] <- u[i, j]
      B[2, 4] <- 1 / 2 * (u[i, (j + 1)] + u[i, j])
      
      B[3, 1] <- 1 / 2 * (u[(i + 1), (j - 1)] + u[i, j])
      B[3, 2] <- 1 / 2 * (u[(i + 1), j] + u[i, j])
      B[3, 3] <- 1 / 2 * (u[(i + 1), j] + u[i, j])
      B[3, 4] <- 1 / 2 * (u[(i + 1), (j + 1)] + u[i, j])
      
      X[(3 * (i - 2) + 1):(3 * (i - 2) + 3), (4 * (j - 2) + 1):(4 * (j - 2) + 4)] <-
        B
    }
  }
  return(X)
}


nastaviMatrikoNicel <- function (maxVisina, neugodnePovrsine) {
  #odpremo podatke
  povrsine <- "vhodni-podatki/geo_em.d02.nc"
  fid = ncdf4::nc_open(povrsine, write = FALSE)
  lu = ncdf4::ncvar_get(fid, "LU_INDEX")[61:153, 53:108] #LU index
  visina = ncdf4::ncvar_get(fid, "HGT_M")[61:153, 53:108]  # nadmorska visina
  ncdf4::nc_close(fid)
  
  #dimenzije
  N <- 93
  M <- 56
  dim <- c((N - 2) * 3, (M - 2) * 4)
  matrikaNicel <- matrix(rep(1, dim[1] * dim[2]), dim[1])
  
  for (i in 2:(N - 1)) {
    for (j in 2:(M - 1)) {
      #preveri ali je visina visja od max oz. ali je povrsina neugodna in
      #v pozitivnem primeru nastavi nicle
      if ((visina[i, j] >= maxVisina)) {
        # && !(lu[i, j] %in% neugodnePovrsine)){
        matrikaNicel[(3 * (i - 2) + 1):(3 * (i - 2) + 3), (4 * (j - 2) +
                                                             1):(4 * (j - 2) + 4)] <-
          0
      }
    }
  }
  matrikaNicel <- t(matrikaNicel[, 216:1])
  save(matrikaNicel, file = "vmesni-podatki/matrikaNicel.RData")
}

# nastaviMatrikoNicel(1000, c(1, 2, 3, 4, 5, 11, 15, 17))

nastaviVreme <- function (podatkiVreme, stDni) {
  N <- 93
  M <- 56
  dim <- c((M - 2) * 4, (N - 2) * 3)
  
  zonalniVeter <- array(0, dim = c(dim[1], dim[2], stDni))
  meridionalniVeter <- array(0, dim = c(dim[1], dim[2], stDni))
  temperatura <- array(0, dim = c(dim[1], dim[2], stDni))
  
  for (i in 1:stDni) {
    fid <- ncdf4::nc_open(podatkiVreme, write = FALSE)
    #preberemo vetrovni komponenti in temperaturo
    ut <- ncdf4::ncvar_get(fid, "u10")[61:153, 53:108, ]
    vt <- ncdf4::ncvar_get(fid, "v10")[61:153, 53:108, ]
    tt <- ncdf4::ncvar_get(fid, "t2")[61:153, 53:108, ]
    ncdf4::nc_close(fid)
    
    u <- round(apply(ut, c(1, 2), mean), 1)
    v <- round(apply(vt, c(1, 2), mean), 1)
    t <- round(apply(tt, c(1, 2), mean), 1)
    
    # ker so podatki za zonalni veter v smeri vzhod-zahod,
    # mi pa jih želimo v smeri zahod-vzhod, matriko najprej negiramo
    zonalniVeter[, , i] <- -t(round(3.6 * nastaviVeter(u, N, M), 1))
    meridionalniVeter[, , i] <-
      t(round(3.6 * nastaviVeter(v, N, M), 1))
    temperatura[, , i] <- t(round(nastaviVeter(t, N, M) - 273, 1))
  }
  
  save(zonalniVeter, file = "vmesni-podatki/zonalniVeter.RData")
  save(meridionalniVeter, file = "vmesni-podatki/meridionalniVeter.RData")
  save(temperatura, file = "vmesni-podatki/temperatura.RData")
}

# nastaviVreme("vhodni-podatki/20150530.nc", 30)


# --- Matriki goveda in drobnice ----------------------------------------------

# Ta del kode iz tekstovnih datotek s staležem (v katerih je v vsaki vrstici
# opisano eno gospodarstvo, njegove koordinate in število živali) naredi
# matriko, v kateri piše koliko živali je na vsakem kvadratnem kilometru
# Slovenije:
# * datoteko "govedo_3leta.txt" pretvorimo v matriko "govedo",
# * datoteko "drobnica_stalez_3l.txt" pa v matriko "drobnica".

# Funkcija, ki razpredelnico s staležem pretvori v matriko.
matrika.staleza <- function(stalez) {
  # Ne vem, kaj točno pomenijo koordinate x in y v staležu, zato sem te
  # koeficiente nastavil tako, da dobimo približno sliko Slovenije.
  # Kdor ve, kaj so, naj jih ustrezno popravi.
  stalez$lon <- 8.539508 + 1.292487e-05 * stalez$y
  stalez$lat <- 45.21132 + 8.413512e-06 * stalez$x
  
  # Nekaterih podatkov o številu ne moremo prebrati, zato jih ignoriramo.
  stalez$stevilo[is.na(stalez$stevilo)] <- 0
  
  # Naredimo prazno matriko in vanjo seštejemo vsa gospodarstva.
  # To se da verjetno bolj elegantno narediti brez zanke for.
  matrika <- matrix(0, nrow = st.vrstic, ncol = st.stolpcev)
  for (indeks.vnosa in 1:nrow(stalez)) {
    vnos <- stalez[indeks.vnosa, ]
    stolpec <- round((vnos$lon - x.lim[1]) / dx + 1 / 2)
    vrstica <- round((y.lim[2] - vnos$lat) / dy + 1 / 2)
    if (1 <= vrstica &&
        vrstica <= st.vrstic &&
        1 <= stolpec && stolpec <= st.stolpcev)
      matrika[vrstica, stolpec] <-
      matrika[vrstica, stolpec] + vnos$stevilo
  }
  
  # Da bomo za rezultate imeli cela števila, na koncu vse še zaokrožimo.
  return(round(matrika))
}

stalez.goveda <-
  read.table(
    "vhodni-podatki/govedo_3leta.txt",
    fileEncoding = "windows-1250",
    skip = 4,
    col.names = c(".", ".", ".", ".", "x", "y", ".", "stevilo"),
    colClasses = c
    (
    "NULL",
    "NULL",
    "NULL",
    "NULL",
    "double",
    "double",
    "NULL",
    "double"
    ),
    dec = ",",
    fill = TRUE,
    as.is = TRUE
  )
govedo <- matrika.staleza(stalez.goveda)
save(govedo, file = 'vmesni-podatki/govedo.RData')

stalez.drobnice <-
  read.table(
    "vhodni-podatki/drobnica_stalez_3l.txt",
    fileEncoding = "windows-1250",
    skip = 2,
    col.names = c(".", ".", ".", ".", "x", "y", "stevilo"),
    colClasses = c
    ("NULL",
    "NULL",
    "NULL",
    "NULL",
    "double",
    "double",
    "double"),
    dec = ",",
    fill = TRUE,
    as.is = TRUE
  )
# Ignoriramo 6 gospodarstev, ki nimajo nastavljene Y koordinate
stalez.drobnice <- stalez.drobnice[!is.na(stalez.drobnice$y),]
# Pri drobnici nimamo izračunanega povprečja, zato skupno število delimo s 3.
stalez.drobnice$stevilo <- stalez.drobnice$stevilo / 3
drobnica <- matrika.staleza(stalez.drobnice)
save(drobnica, file = 'vmesni-podatki/drobnica.RData')
