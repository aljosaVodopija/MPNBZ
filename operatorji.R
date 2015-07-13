#Zacetne nastavitve
hx = 0.9 #longitude razdalja
hy = 1 #latitude razdalja


#DIV izracuna diskretno divergenco F=(F1,F2) (vektorsko polje vetra)
#Prejme metriko X (jakost zonalnega vetra) in matriko Y (jakost meridionalnega vetra),
#pri tem velja dim(Y)=dim(X). 
#Izraèuna matriki F1x in F2y ter vrne matriko F1x + F2y. 

DIV = function(X,Y){

return(odvodx(X,hx)+odvody(Y,hy))
}

#GRADF izracuna s. p. med grad(m)*F (m skalarno polje, F vektorsko polje)
#Prejme metriko M in matriki X, Y  
#Izraèuna matriki Mx, My in vrne iskani s. p. 

GRADF = function(M,X,Y){

return(odvodx(M,hx)*X+odvody(M,hy)*Y)
}

#Diskretiziran Laplaceov operator skalarnega polja
#Prejme matriko M (skalarno polje)
#Izracuna Mxx in Myy ter vrne Mxx + Myy

LAPLACE = function(M){
 
return(odvodxx(M,hx)+odvodyy(M,hy))
}

#Odvod skalarnega polja F po y (Fy), pri tem uporablja sim. dif.

odvody = function(F,h){

n = dim(F)[1]
m = dim(F)[2]
Fy = matrix(1:(n*m),n)

Fy[,2:(m-1)] = 1/(2*h)*(F[,3:m]-F[,1:(m-2)])
Fy[,1] = 1/(2*h)*(F[,2]-F[,1])
Fy[,m] = 1/(2*h)*(F[,m]-F[,(m-1)])

return(Fy)
}

#Odvod skalarnega polja F po x (Fx), pri tem uporablja sim. dif.

odvodx = function(F,h){

n = dim(F)[1]
m = dim(F)[2]
Fx = matrix(1:(n*m),n)

Fx[2:(n-1),] = 1/(2*h)*(F[3:n,]-F[1:(n-2),])
Fx[1,] = 1/(2*h)*(F[2,]-F[1,])
Fx[n,] = 1/(2*h)*(F[n,]-F[(n-1),])

return(Fx)
}

#Drugi odvod skalrnega polja F po y (Fyy)

odvodyy = function(F,h){

n = dim(F)[1]
m = dim(F)[2]
Fyy = matrix(1:(n*m),n)

Fyy[,2:(m-1)] = 1/(h^2)*(F[,3:m]+F[,1:(m-2)]-2*F[,2:(m-1)])
Fyy[,1] = 1/(h^2)*(F[,2]-F[,1])
Fyy[,m] = 1/(h^2)*(F[,(m-1)]-F[,m])

return(Fyy)
}

#Drugi odvod skalarnega polja F po x (Fxx)

odvodxx = function(F,h){

n = dim(F)[1]
m = dim(F)[2]
Fxx = matrix(1:(n*m),n)

Fxx[2:(n-1),] = 1/(h^2)*(F[3:n,]+F[1:(n-2),]-2*F[2:(n-1),])
Fxx[1,] = 1/(h^2)*(F[2,]-F[1,])
Fxx[n,] = 1/(h^2)*(F[(n-1),]-F[n,])

return(Fxx)
}