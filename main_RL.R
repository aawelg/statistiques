

### 1/
X1=rnorm(100)
X2=runif(100)
epsilon=0.5*rnorm(100)
Y=2+0.5*X1-3*X2+epsilon

# regression lineaire multiple
Y=as.matrix(Y)
X=matrix( c(rep(1,100),X1,X2), 100, 3 )
res=ml.fun(Y,X) # ou res=lm(Y~X1+X2); summary(res)

cat( sprintf("\n beta=") ); cat( sprintf("beta= \n %f",res$beta) ) 
cat( sprintf("\n R2=%f",res$R2) ); cat( sprintf("\n est.sigma=%f",res$est.sigma) )

### 2/ 

n=500
rho=0.1
eta=rnorm(n)
epsilon=sqrt(0.8)*rnorm(n)
X2=rnorm(n)
X1=X2+rho*eta
Y=2+0.5*X1-3*X2+epsilon
X=matrix( c(rep(1,n),X1,X2), n, 3 )
resc=ml.fun(Y,X)

# effet de l'omission d'une variable
k=1
sigma=list(); beta=list(); R2=list();

for(rho in c(0,0.5,1,2)){
  eta=rnorm(n)
  epsilon=sqrt(0.8)*rnorm(n)
  X2=rnorm(n)
  X1=X2+rho*eta
  Y=2+0.5*X1-3*X2+epsilon
  X=matrix( c(rep(1,n),X1), n, 2 )
  res=ml.fun(Y,X)
  beta[k]=list(res$beta); sigma[k]=res$est.sigma; R2[k]=res$R2
  k=k+1
}



