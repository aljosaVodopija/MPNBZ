#za uporabo skripte si je potrebno naloziti paket "ncdf", instaliramo ga
#preprosto tako, da poklcemo 
#> install.packages("ncdf") 

#zacetne nastavitve

library("ncdf")
setwd("C:\\Users\\Aljosa\\Desktop\\projekt\\mpnbz\\podatki_vreme_fiziki") 
#to bomo sproti urejali odvisno kje bomo program poganjali

########################################################
########## od tod naprej pride v zanko s casom###########
### for time t

#odpremo datoteko
file="20150413.nc" #to bo nek datum v odvisnosti od t, t.j. datum(t), 
# ko bo zanka nrejena mi posljes pa bom popravil

fid=open.ncdf(file, write=FALSE)

#preberemo vetrovni komponenti
ut = get.var.ncdf(fid, "u10")[61:153,53:108,] 
vt = get.var.ncdf(fid, "v10")[61:153,53:108,]
# temperatura v odvisnosti od casa
tt = get.var.ncdf(fid, "t2")[61:153,53:108,] 
close.ncdf(fid)

#povprecimo veter cez dan
u = ut[,,8]
v = vt[,,8]
t = tt[,,8]

for (i in c(10,12,14,16,18,20,22)){
u = u + ut[,,i]
v = v + vt[,,i]
t = t + tt[,,i]
}

v = 1/8*v
u = 1/8*u
t = 1/8*t # povprecna dnevna temperatura

N = dim(u)[1] # to je v bistvu dim[1]+2 in dim[2]+2 v tvojem primeru
M = dim(u)[2] 

# to neki casa traja, po potrebi bom poenostavil in tako pohitril
# F =(X,Y) je vektorsko polje vetra
X = 3.6*NASTAVI_VETER(u,N,M) #zonalni veter v km/h (vzhod-zahod)
Y = 3.6*NASTAVI_VETER(v,N,M) #meridionalni veter v km/h (jug-sever)
T = NASTAVI_VETER(t,N,M) # povprecna dnevna temperatura v kelvinih!!
# T = T-273 ce potrebujes stopinje celzija 