
RL_fun=function(Y,X){
  
  n = nrow(X); p = ncol(X) 
  beta = ( solve(crossprod(X,X)) %*% crossprod(X,Y) ) # coeff. de reg
  est.sigma = norm(Y-X%*%beta) / (n-p-1) # estimateur de la variance du bruit
  est.Y = X%*%beta # val. ajustées 
  e = Y - est.Y # residus d'estimation
  R2= sum( (est.Y - rep(mean(Y),n))^2 ) / sum( (Y - rep(mean(Y),n))^2 ) # coeff. de determination
  
  list(beta=beta, est.sigma=est.sigma, est.Y=est.Y, e=e, R2=R2)
  
}