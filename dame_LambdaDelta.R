#######################################################################
###
### CALCULA LAMBDA-DELTA (PASO FORDWARD 2)
### @params: datos (data.frame)
### @params: alfa, beta, gamma
###
### Devolvemos lambda (vector) y beta (data.frame)
###
#######################################################################
dame_LambdaDelta <- function (datos, alfa, beta, gamma) {
  	n <- length(alfa)
  	lambda <- array(0, dim = c(n-1,1))
  	delta  <- data.frame(x=double(), y=double())
  	
    ### CODIGO A REALIZAR:
    ### Calcular lambda y delta 

  	#Tratamos el primer elemento
  	lambda[1] <- gamma[1]
  	delta[1,] <- datos[1,]
  	delta[1,] <- datos[1,]
  	  
  	
  	#Calculo de lambda
  	#Calculamos los elementos de lambda a partir del 2º elemento hasta n-1
  	#Hay que comprobar que el denominador no sea 0
  	
  	for (i in 2:(n-1)){
  	  div <- beta[i]-(alfa[i]*lambda[i-1])
  	    
    	  if (div > 1.0e-8){
    	    lambda[i] <- gamma[i] / div
    	 
    	  }else{
    	    lambda[i] <- 0
    	       }
  	}
  	
  	
  	
  	#Calculo de delta
  	#Calculamos el resto de los elementos de delta, otra vez hay que comprobar que el denominador no sea 0
  	
  	for (i in 2:n){
  	  div_delta <- beta[i] - (alfa[i] * lambda[i-1])
  	  if ( div_delta > 1.0e-8 ){
  	    delta[i,] <- (datos[i,] - (alfa[i] * delta[i-1,]))/ div_delta
  	   
  	  }
  	  
  	  else{
  	    delta[i,] <- 0
  	  }
  	  
  	}
  	
  	res        <- NULL
  	res$lambda <- lambda
  	res$delta  <- delta
  
  	return(res)
}