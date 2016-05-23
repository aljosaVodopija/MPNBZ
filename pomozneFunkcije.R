# Nastavimo parametre
lonRange <- list(min = 13.5 - 1 / 90, max = 16.5 + 1 / 90)
latRange <- list(min = 45.2 + 1 / 30 - 1.5 / 120, max = 47 + 1.5 / 120)
dx <- 1 / 90
dy <- 1 / 120
steviloVrstic <- round((latRange$max - latRange$min) / dy) + 1
steviloStolpcev <- round((lonRange$max - lonRange$min) / dx) + 1
dimenzije <- c(steviloVrstic, steviloStolpcev)
mapaIzhodnihPodatkov <- "izhodni-podatki"

if (!dir.exists(mapaIzhodnihPodatkov))
  dir.create(mapaIzhodnihPodatkov)

indeksKraja <- function(mesto) {
  kode = read.csv("vmesni-podatki/koordinateKrajev.csv", row.names = 1)
  mesto = toupper(stringi::stri_trans_general(mesto,"latin-ascii"))
  if (!mesto %in% rownames(kode)) {
    kode[mesto,] = geocode(paste(mesto, "SLOVENIJA"))
    write.csv(kode, "vmesni-podatki/koordinateKrajev.csv")
  }
  koordinate <- kode[mesto,]
  stolpec <- round((koordinate$lon - lonRange$min) / dx + 1 / 2)
  vrstica <- round((latRange$max - koordinate$lat) / dy + 1 / 2)
  return(matrix(c(vrstica, stolpec), ncol = 2))
}

# Funkcija, ki razpredelnico s staležem pretvori v matriko.
matrikaStaleza <- function(stalez) {
  # Ne vem, kaj točno pomenijo koordinate x in y v staležu, zato sem te
  # koeficiente nastavil tako, da dobimo približno sliko Slovenije.
  # Kdor ve, kaj so, naj jih ustrezno popravi.
  stalez$lon <- 8.539508 + 1.292487e-05 * stalez$y
  stalez$lat <- 45.21132 + 8.413512e-06 * stalez$x
  
  # Nekaterih podatkov o številu ne moremo prebrati, zato jih ignoriramo.
  stalez$stevilo[is.na(stalez$stevilo)] <- 0
  
  # Naredimo prazno matriko in vanjo seštejemo vsa gospodarstva.
  # To se da verjetno bolj elegantno narediti brez zanke for.
  matrika <- matrix(0, nrow = steviloVrstic, ncol = steviloStolpcev)
  for (indeksVnosa in 1:nrow(stalez)) {
    vnos <- stalez[indeksVnosa,]
    stolpec <- round((vnos$lon - lonRange$min) / dx + 1 / 2)
    vrstica <- round((latRange$max - vnos$lat) / dy + 1 / 2)
    if (1 <= vrstica &&
        vrstica <= steviloVrstic &&
        1 <= stolpec && stolpec <= steviloStolpcev)
      matrika[vrstica, stolpec] <-
      matrika[vrstica, stolpec] + vnos$stevilo
  }
  
  # Da bomo za rezultate imeli cela števila, na koncu vse še zaokrožimo.
  return(round(matrika))
}


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
    ut <- ncdf4::ncvar_get(fid, "u10")[61:153, 53:108,]
    vt <- ncdf4::ncvar_get(fid, "v10")[61:153, 53:108,]
    tt <- ncdf4::ncvar_get(fid, "t2")[61:153, 53:108,]
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
  
  return(
    list(
      zonalniVeter = zonalniVeter,
      meridionalniVeter = meridionalniVeter,
      temperatura = temperatura
    )
  )
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


nastaviNeprimernaObmocja <-
  function (datoteka, maxVisina, neugodnePovrsine) {
    #odpremo podatke
    povrsine <- datoteka
    fid = ncdf4::nc_open(povrsine, write = FALSE)
    lu = ncdf4::ncvar_get(fid, "LU_INDEX")[61:153, 53:108] #LU index
    visina = ncdf4::ncvar_get(fid, "HGT_M")[61:153, 53:108]  # nadmorska visina
    ncdf4::nc_close(fid)
    
    #dimenzije
    N <- 93
    M <- 56
    dim <- c((N - 2) * 3, (M - 2) * 4)
    matrika <- matrix(rep(1, dim[1] * dim[2]), dim[1])
    
    for (i in 2:(N - 1)) {
      for (j in 2:(M - 1)) {
        #preveri ali je visina visja od max oz. ali je povrsina neugodna in
        #v pozitivnem primeru nastavi nicle
        if ((visina[i, j] >= maxVisina) ||
            (lu[i, j] %in% neugodnePovrsine)) {
          matrika[(3 * (i - 2) + 1):(3 * (i - 2) + 3), (4 * (j - 2) +
                                                               1):(4 * (j - 2) + 4)] <-
            0
        }
      }
    }
    return(t(matrika[, 216:1]))
  }
