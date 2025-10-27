library(glmnet)
# Define the custom ruv_glmnet function
ruv_glmnet = function(y,x,nperm=100,lstop=exp(-3)){
  boot = table(sample(1:nrow(x),replace=T))
  boot = c(boot,rep(0,nrow(x)-length(boot)))
  pf = rep(Inf,ncol(x))
  pf[1:floor(ncol(x)/3)] = 1 # Apply penalty factor to a subset of features
  ffit = function(x,y,boot,pf){
    predict(glmnet::glmnet(y=y,x=x,weights=sample(boot),penalty.factor=sample(pf),lambda=lstop),
            x,
            s=lstop)[,1]
  }
  r = replicate(nperm,y-ffit(x,y,boot,pf))
  return(r)
}
