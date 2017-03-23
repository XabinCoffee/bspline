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

  	lambda[1] <- gamma[1]
  	delta[1,1] <- datos$x[1]
  	delta[1,2] <- datos$y[1]
  	  
  	
  	#Calculo de lambda
  	
  	for (i in 2:(n-1)){
  	  div <- beta[i]-(alfa[i]*lambda[i-1])
  	    
    	  if (div == 0){
    	    lambda[i] <- 0
    	  }else{
    	    lambda[i] <- gamma[i] / div
    	       }
  	}
  	
  	
  	
  	#Calculo de delta
  	
  	for (i in 2:n){
  	  div_delta <- beta[i] - (alfa[i] * lambda[i-1])
  	  if ( div_delta == 0 ){
  	    delta[i,] <- 0
  	  }
  	  
  	  else{
  	    delta[i,] <- (datos[i,] - (alfa[i] * delta[i-1,]))/ div_delta
  	  }
  	  
  	}
  	
  	res        <- NULL
  	res$lambda <- lambda
  	res$delta  <- delta
  
  	return(res)
}