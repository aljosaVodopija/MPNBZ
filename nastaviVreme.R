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

  save(zonalniVeter, file = "zonalniVeter.RData")
  save(meridionalniVeter, file = "meridionalniVeter.RData")
  save(temperatura, file = "temperatura.RData")
}
