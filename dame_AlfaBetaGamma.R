#######################################################################
###
### CALCULA ALFA, BETA, GAMMA (PASO FORDWARD 1)
### @params: n (integer)
### @params: nudos (vector)
###
### Devolvemos los vectores alfa, beta, gamma
###
#######################################################################
dame_AlfaBetaGamma <- function (n, nudos) {
  	alfa  <- array(0, dim = n)
  	beta  <- array(0, dim = n)
  	gamma <- array(0, dim = n)
  
  	### alfa[1] == alfa[n] == 0
  	### gamma[1] == gamma[n] == 0
    ### beta[1] == beta[n] == 1
  	
    ### CODIGO A REALIZAR:
    ### Calcular los vectores alfa, beta, gamma
  	
  	alfa[1] <- 0
  	beta[1] <- 1
  	gamma[1] <- 0

  	for (i in 2:(n-1)){
  	  
  	  denomA<-(nudos[i+4]-nudos[i+1])*(nudos[i+4]-nudos[i+2])
  	  denomB1<-(nudos[i+4]-nudos[i+1])*(nudos[i+4]-nudos[i+2]) 
  	  denomB2<-(nudos[i+5]-nudos[i+2])*(nudos[i+4]-nudos[i+2])
  	  denomG<-(nudos[i+4]-nudos[i+2])*(nudos[i+5]-nudos[i+2])
  	  
  	  alfa[i] <- ((nudos[i+4]-nudos[i+3])^2) / denomA
  	  beta[i] <- (((nudos[i+3]-nudos[i+1])*(nudos[i+4]-nudos[i+3]))/denomB1) + (((nudos[i+5]-nudos[i+3])*(nudos[i+3]-nudos[i+2]))/denomB2)
  	  gamma[i] <- ((nudos[i+3]-nudos[i+2])^2)/denomG
  	}
  
  	alfa[n] <- 0
  	beta[n] <- 1
  	gamma[n] <- 0 
  	  
  	res       <- NULL
  	res$alfa  <- alfa
  	res$beta  <- beta
  	res$gamma <- gamma
  	
  	return(res)

}