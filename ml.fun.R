
ml.fun=function(Y,X){
  
  n = nrow(X); p = ncol(X) 
  beta = ( solve(crossprod(X,X)) %*% crossprod(X,Y) )
  est.sigma = norm(Y-X%*%beta) / (n-p-1)
  
  list(beta=beta, est.sigma=est.sigma)
  
}