#######################################################################
###
### CALCULA X (PASO BACKWARD)
### @params: delta (data.frame)
### @params: lambda (vector)
###
### Devolvemos X (data.frame)
###
#######################################################################
dame_X <- function (delta, lambda) {
    
    n <- length(lambda)
    ### CODIGO A REALIZAR:
    ### Calcular X
    
    X  <- data.frame(x=double(), y=double())
    
    X[n-2,1]<-delta$x[n-2]
    X[n-2,2]<-delta$y[n-2]
    
    for (i in (n-3):1){
      X[i,1] <- delta$x[i] - lambda[i] * X[i+1,1]
      X[i,2] <- delta$y[i] - lambda[i] * X[i+1,2]
    }
    
    

    return(X)
}