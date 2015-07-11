#DIV izracuna divergenco F=(F1,F2) (vektorsko polje vetra)
#Prejme metriko X (jakost zonalnega vetra) in matriko Y (jakost meridionalnega vetra),
#pri tem velja dim(Y)=dim(X). 
#Izraèuna matriki F1x in F2y ter vrne matriko D = F1x + F2y. 
#lahko dodamo z-to komponento [x,y,z]

DIV = function(X,Y){

#zacetne nastavitve
hx = 0.9 #longitude razdalja
hy = 1 #latitude razdalja

return(odvodx(X,hx)+odvody(Y,hy))
}

#GRADF izracuna s. p. med grad(m)*F (m skalarno polje, F vektorsko polje)
#Prejme metriko M in matriki X, Y  
#Izraèuna matriki Mx, My in vrne iskani s. p. 

GRADF = function(M,X,Y){

#zacetne nastavitve
hx = 0.9 #longitude razdalja
hy = 1 #latitude razdalja

return(odvodx(M,hx)*X+odvody(M,hy)*Y)
}

#odvod vektorskega polja F po x (Fx), pri tem uporablja sim. dif.

odvodx = function(F,hx){

m = dim(F)[1]
n = dim(F)[2]
Fx = matrix(1:(n*m),m)

Fx[,2:(n-1)] = 1/(2*hx)*(F[,3:n]-F[,1:(n-2)])
Fx[,1] = 1/hx*(F[,2]-F[,1])
Fx[,n] = 1/hx*(F[,n]-F[,(n-1)])

return(Fx)
}

#odvod vektorskega polja F po y (Fy), pri tem uporablja sim. dif.

odvody = function(F,hy){

m = dim(F)[1]
n = dim(F)[2]
Fy = matrix(1:(n*m),m)

Fy[2:(m-1),] = 1/(2*hy)*(F[1:(m-2),]-F[3:m,])
Fy[1,] = 1/hy*(F[1,]-F[2,])
Fy[m,] = 1/hy*(F[(m-1),]-F[m,])

return(Fy)
}