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

  	lambda[1] <- gamma[1]/beta[1]
  	delta[1,1] <- datos$x[1]/beta[1]
  	delta[1,2] <- datos$y[1]/beta[1]
  	  
  	
  	
  	for (i in 2:n){
  	  
  	  if ((beta[i]-(alfa[i]*gamma[i-1])) == 0){
  	    lambda[i] <- 0
  	  }else{
  	    lambda[i] <- gamma[i] / (beta[i]-(alfa[i]*gamma[i-1]))
  	  }
  	}
  	
  	for (i in 2:(n-1)){
  	  
  	  if ((beta[i] - (alfa[i] * lambda[i-1])) == 0){
  	    delta[i,1] <- 0
  	    delta[i,2] <- 0
  	  }
  	  
  	  else{
  	    delta[i,1] <- (datos$x[i] - (alfa[i] * delta$x[i-1]))/ (beta[i] - (alfa[i] * lambda[i-1]))
  	    delta[i,2] <- (datos$y[i] - (alfa[i] * delta$y[i-1]))/ (beta[i] - (alfa[i] * lambda[i-1]))
  	  }
  	  
  	}
  	
  	res        <- NULL
  	res$lambda <- lambda
  	res$delta  <- delta
  
  	return(res)
}