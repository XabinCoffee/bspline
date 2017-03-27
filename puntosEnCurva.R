#######################################################################
###
### GENERA PUNTOS SOBRE LA BSPLINE
### (por defecto, 20 PUNTOS)
### @params: bspline (data.frame)
### @params: nPuntos (integer o NULL)
###
### Devolvemos puntos (data.frame) sobre la bspline: coordenadas (x, y)
###
#######################################################################
puntosEnCurva <- function(bspline, nPuntos=NULL) {
	puntosControl <- bspline$puntosControl
	nudos <- bspline$nudos
	p <- bspline$p

  	### CODIGO A REALIZAR:
  	### Generamos las coordenadas (x, y) y las almacenamos en la variable puntos
  	puntos <- NULL
  	nPuntos <- dim(puntosControl)[1]
  	
  	#seq(from=0, to=1, by=1/(nDatos-p+2))
  	
  	#Calculamos el punto de u desde 0 hasta 0.999 (se excluye 1 por que puede dar problemas en el c?lculo)
  	
  	for(u in seq(from=0,to=0.999,by=1/100)){
  	  
  	  sum <- c(0,0)
  	  
  	  for(i in 1:nPuntos){
  	    sum <- sum + N(i,p,u,nudos)*puntosControl[i,]
  	  }
  	  puntos <- rbind(puntos, sum)
  	}
  	
  	
  	print(puntos)

  	return(puntos)
}