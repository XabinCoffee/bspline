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

	# rownames(puntos) <- NULL
	# colnames(puntos) <- c("x", "y")
	# puntos <- data.frame(puntos)

	return(puntos)
}