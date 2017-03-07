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
  	delta[1] <- datos[1]/beta[1]
  	  
  	
  	
  	for (i in 2:n){
  	  
  	  if ((beta[i]-(alfa[i]*gamma[i-1])) == 0){
  	    lambda[i] <- 0
  	  }else{
  	    lambda[i] <- gamma[i] / (beta[i]-(alfa[i]*gamma[i-1]))
  	  }
  	}
  	
  	for (i in 2:n){
  	  delta[i] <- (datos[i] - (alfa[i] * delta[i-1]))/ (beta[i] - (alfa[i] * lambda[i-1]))
  	}
  	
  	res        <- NULL
  	res$lambda <- lambda
  	res$delta  <- delta
  
  	return(res)
}