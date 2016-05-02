# Nastavimo parametre
x.lim <- c(13.5 - 1 / 90, 16.5 + 1 / 90)
y.lim <- c(45.2 + 1 / 30 - 1.5 / 120,  47 + 1.5 / 120)
dx <- 1 / 90
dy <- 1 / 120
st.vrstic <- round((y.lim[2] - y.lim[1]) / dy) + 1
st.stolpcev <- round((x.lim[2] - x.lim[1]) / dx) + 1
dimenzije <- c(st.vrstic, st.stolpcev)

indeks.kraja <- function(mesto) {
  kode = read.csv('vmesni-podatki/koordinateKrajev.csv', row.names = 1)
  mesto = toupper(mesto)
  if (!mesto %in% rownames(kode)) {
    kode[mesto, ] = geocode(paste(mesto, "SLOVENIJA"))
  }
  write.csv(kode, 'vmesni-podatki/koordinateKrajev.csv')
  koordinate <- kode[mesto, ]
  stolpec <- round((koordinate$lon - x.lim[1]) / dx + 1 / 2)
  vrstica <- round((y.lim[2] - koordinate$lat) / dy + 1 / 2)
  return(matrix(c(vrstica, stolpec), ncol = 2))
}
