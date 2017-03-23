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
	 
	  num <- N(3,p,0.5,nudos)
	  print(num)

	  rownames(puntos) <- NULL
	  colnames(puntos) <- c("x", "y")
	  
	  return(puntos)
}