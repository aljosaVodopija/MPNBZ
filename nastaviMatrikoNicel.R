nastaviMatrikoNicel <- function (maxVisina, neugodnePovrsine) {
  #odpremo podatke
  povrsine <- "geo_em.d02.nc"
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
  save(matrikaNicel, file = "matrikaNicel.RData")
}
