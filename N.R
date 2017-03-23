#######################################################################
###
### FUNCION COX-DE-BOOR
### @params: i (indice)
### @params: p (grado de la curva)
### @params: u (parametro)
### @params: nudos (vector)
###
### Devolvemos el valor de la funcion base N(i,p,u)
###
#######################################################################
N <- function (i, p, u, nudos) {
	### CODIGO A REALIZAR
    ### Calcular el valor de la funcion base N(i,p,u)
    nip <- 0
    
    if (p==0){
      nip<-1
    }
    else {
      
      nip <- ((u - nudos[i])/(nudos[i+p] - nudos[i])) * N(i,p-1,u,nudos) + ((nudos[i+p+1] - u)/(nudos[i+p+1] - nudos[i+1])) * N(i+1,p-1,u,nudos)
      
    }
    
  	return(nip)
}