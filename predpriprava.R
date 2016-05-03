# install.packages("ncdf4")

neprimernaObmocja <-
  nastaviNeprimernaObmocja("vhodni-podatki/geo_em.d02.nc", 1000, c(1, 2, 3, 4, 5, 11, 15, 17))
save(neprimernaObmocja, file = "vmesni-podatki/neprimernaObmocja.RData")

vreme <- nastaviVreme("vhodni-podatki/20150530.nc", 30)
save(vreme, file = "vmesni-podatki/vreme.RData")


# --- Matriki goveda in drobnice ----------------------------------------------

# Ta del kode iz tekstovnih datotek s staležem (v katerih je v vsaki vrstici
# opisano eno gospodarstvo, njegove koordinate in število živali) naredi
# matriko, v kateri piše koliko živali je na vsakem kvadratnem kilometru
# Slovenije:
# * datoteko "govedo_3leta.txt" pretvorimo v matriko "govedo",
# * datoteko "drobnica_stalez_3l.txt" pa v matriko "drobnica".

stalezGovedo <-
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
govedo <- matrikaStaleza(stalezGovedo)
save(govedo, file = "vmesni-podatki/govedo.RData")

stalezDrobnica <-
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
stalezDrobnica <- stalezDrobnica[!is.na(stalezDrobnica$y), ]
# Pri drobnici nimamo izračunanega povprečja, zato skupno število delimo s 3.
stalezDrobnica$stevilo <- stalezDrobnica$stevilo / 3
drobnica <- matrikaStaleza(stalezDrobnica)
save(drobnica, file = "vmesni-podatki/drobnica.RData")
