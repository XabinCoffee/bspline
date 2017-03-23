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
    
    n <- length(lambda) + 1
    ### CODIGO A REALIZAR:
    ### Calcular X
    
    X  <- data.frame(x=double(), y=double())
    
    X[n,]<-delta[n,]
    
    
    for (i in (n-1):1){
      X[i,] <- delta[i,] - (lambda[i] * X[i+1,])
    }
    
    return(X)
}