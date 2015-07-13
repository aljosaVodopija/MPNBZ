library("ncdf")
setwd("C:\\Users\\Aljosa\\Desktop\\projekt\\mpnbz\\podatki_vreme_fiziki") 

NASTAVI_NICLE = function(){

#zacetne nastavitve
#odpremo podatke
file = "geo_em.d02.nc"
fid = open.ncdf(file,write=FALSE)
lu = get.var.ncdf(fid, "LU_INDEX")[61:153,53:108] #LU index
visina = get.var.ncdf(fid, "HGT_M")[61:153,53:108]  # nadmorska visina
close.ncdf(fid)
N = 93
M = 56
max_visina = 999 # maksimalna nadmorska visina
neugodne_povrsine = c(1,2,3,4,5,11,15,17)#neugodne povrsine
dim = c((N-2)*3,(M-2)*4)
X = matrix(rep(0, dim[1]*dim[2]), dim[1])

for (i in 2:(N-1)){ 

for (j in 2:(M-1)){

#preveri ali je visina visja od max oz. ali je povrsina neugodna in
#v pozitivnem primeru nastavi nicle
if((visina[i,j] > max_visina) || (lu[i,j] %in% neugodne_povrsine)){
X[(3*(i-2)+1):(3*(i-2)+3),(4*(j-2)+1):(4*(j-2)+4)]=0
} else {
X[(3*(i-2)+1):(3*(i-2)+3),(4*(j-2)+1):(4*(j-2)+4)]=1
}

}}

return(X)
}

matrika_nicel = NASTAVI_NICLE()
