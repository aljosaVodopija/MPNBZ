# install.packages("ncdf4")

source('funkcije.r')

nastaviMatrikoNicel <- function (maxVisina, neugodnePovrsine) {
  #odpremo podatke
  povrsine <- "vhodni-podatki/geo_em.d02.nc"
  fid = ncdf4::nc_open(povrsine,write=FALSE)
  lu = ncdf4::ncvar_get(fid, "LU_INDEX")[61:153,53:108] #LU index
  visina = ncdf4::ncvar_get(fid, "HGT_M")[61:153,53:108]  # nadmorska visina
  ncdf4::nc_close(fid)

  #dimenzije
  N <- 93
  M <- 56
  dim <- c((N - 2) * 3, (M - 2) * 4)
  matrikaNicel <- matrix(rep(0, dim[1] * dim[2]), dim[1])

  for (i in 2:(N - 1)){
    for (j in 2:(M - 1)){
      #preveri ali je visina visja od max oz. ali je povrsina neugodna in
      #v pozitivnem primeru nastavi nicle
      if((visina[i, j] < maxVisina) && !(lu[i, j] %in% neugodnePovrsine)){
        matrikaNicel[(3*(i-2)+1):(3*(i-2)+3),(4*(j-2)+1):(4*(j-2)+4)] <- 1
      }
    }
  }
  save(matrikaNicel, file = "vmesni-podatki/matrikaNicel.RData")
}

nastaviMatrikoNicel(1000, c(1, 2, 3, 4, 5, 11, 15, 17))

nastaviVreme <- function (podatkiVreme, stDni) {

  N <- 93
  M <- 56
  dim <- c((N - 2) * 3, (M - 2) * 4)

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

    u <- round(apply(ut, c(1,2), mean), 1)
    v <- round(apply(vt, c(1,2), mean), 1)
    t <- round(apply(tt, c(1,2), mean), 1)

    zonalniVeter[, , i] <- round(3.6 * nastaviVeter(u, N, M), 1)
    meridionalniVeter[, , i] <- round(3.6 * nastaviVeter(v, N, M), 1)
    temperatura[, , i] <- round(nastaviVeter(t, N, M) - 273, 1)
  }

  save(zonalniVeter, file = "vmesni-podatki/zonalniVeter.RData")
  save(meridionalniVeter, file = "vmesni-podatki/meridionalniVeter.RData")
  save(temperatura, file = "vmesni-podatki/temperatura.RData")
}

nastaviVreme("vhodni-podatki/20150530.nc", 1)

drobnica <- naloziZivali("vhodni-podatki/drobnica_stalez_3l.txt", 3)
save(drobnica, file = 'vmesni-podatki/drobnica.RData')

prasici <- naloziZivali("vhodni-podatki/prasici_stalez_3l.txt", 3)
save(prasici, file = 'vmesni-podatki/prasici.RData')

govedo <- naloziZivali("vhodni-podatki/govedo_3leta.txt", 5)
save(govedo, file = 'vmesni-podatki/goveda.RData')
