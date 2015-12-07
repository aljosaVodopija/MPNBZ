nastaviVreme <- function (podatkiVreme) {
  fid <- open.ncdf(podatkiVreme, write = FALSE)
  #preberemo vetrovni komponenti in temperaturo
  ut <- get.var.ncdf(fid, "u10")[61:153, 53:108, ]
  vt <- get.var.ncdf(fid, "v10")[61:153, 53:108, ]
  tt <- get.var.ncdf(fid, "t2")[61:153, 53:108, ]
  close.ncdf(fid)

  #dimenzije
  N <- dim(ut)[1]
  M <- dim(ut)[2]
  dim <- c((N - 2) * 3, (M - 2) * 4)

  #nastavi meteoroloske podatke
  delitevUre <- 3 #na pol
  delitevDneva <- 24 * delitevUre

  # F =(X, Y) je vektorsko polje vetra, T je temperatura
  zonalniVeter <- array(0, dim = c(dim[1], dim[2], delitevDneva))
  meridionalniVeter <- array(0, dim = c(dim[1], dim[2], delitevDneva))
  temperatura <- array(0, dim = c(dim[1], dim[2], delitevDneva))

  for (i in 0:(delitevDneva - 1)) {
    time <- (i %% delitevUre) / delitevUre
    h <- floor(i / delitevUre)
    h1 <- ifelse(h == 0, 24, h)
    h2 <- h + 1
    u <- (1 - time) * ut[, , h1] + time * ut[, , h2]
    v <- (1 - time) * vt[, , h1] + time * vt[, , h2]
    t <- (1 - time) * tt[, , h1] + time * tt[, , h2]
    zonalniVeter[, , (i + 1)] <- round(3.6 * nastaviVeter(u, N, M), 1)
    meridionalniVeter[, , (i + 1)] <- round(3.6 * nastaviVeter(v, N, M), 1)
    temperatura[, , (i + 1)] <- round(nastaviVeter(t, N, M) - 273, 1)
  }

  save(zonalniVeter, file = "zonalniVeter.txt")
  save(meridionalniVeter, file = "meridionalniVeter.txt")
  save(temperatura, file = "temperatura.txt")
}
