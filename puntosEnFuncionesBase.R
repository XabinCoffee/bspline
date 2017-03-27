#######################################################################
###
### GENERA PUNTOS A PARTIR DE LAS FUNCIONES BASE DE LA  BSPLINE 
### (por defecto, 20 PUNTOS)
### @params: bspline (data.frame)
### @params: nPuntos (integer o NULL)
###
### Devolvemos puntos (data.frame) a partir de las funciones base de
### la bspline
### Cada punto de la funcion base N(i, p, u, nudos): 
### coordenadas (x, y) y color (i == indice de la funcion base) 
###
#######################################################################
puntosEnFuncionesBase <- function(bspline, nPuntos=NULL) {
	puntosControl <- bspline$puntosControl
	nudos <- bspline$nudos
	p <- bspline$p

	### CODIGO A REALIZAR:
    ### Generamos los puntos (x, y, color) y los almacenamos en la variable puntos
    puntos <- NULL
    puntos<-data.frame(x=double(), y=double())
    
    for(u in seq(from=0,to=0.999,by=1/100)){
      
      sum <- c(0,0)
      
      for(i in 1:nPuntos){
        sum <- sum + N(i,p,u,nudos)*puntosControl[i,]
      }
      puntos <- rbind(puntos, sum)
    }

	# rownames(puntos) <- NULL
	# colnames(puntos) <- c("x", "y", "color")
	# puntos <- data.frame(puntos)
	# puntos$color <- factor(puntos$color)
		
	return(puntos)
}