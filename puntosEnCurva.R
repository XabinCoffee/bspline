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


	puntos <- data.frame(x=double(), y=double())
	
	
	n <- nrow(puntosControl)
	
	num <- 1/nPuntos
	
	puntos[1,1] <- puntosControl[1,1]
	puntos[1,2] <- puntosControl[1,2]
	puntos[nPuntos,1] <- puntosControl[n,1]
	puntos[nPuntos,2] <- puntosControl[n,2]
	

	return(puntos)
}