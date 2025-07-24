

############################################################
##### functions for response function specified by respf  
############################################################



# available: respf<-"NV", respf<-"logit", respf<-"Gumbel", respf<-"Gompertz"

### needed: density, cumulative function, derivative 
#####################################################

#################################### respfctd : d for density

respfctdNV <-function(x){
  r<-dnorm(x)
  return(r)  
}
respfctdlogit <-function(x){
  r<-exp(x)/(1+exp(x))^2 
  return(r)  
}
respfctdGumbel <-function(x){
  r<-exp(-x)*exp(-exp (-x))  
  return(r)  
}
respfctdGompertz <-function(x){
  r<-exp(x)*exp(-exp (x))  
  return(r)  
}

####################
#################################### respfctc : c for cumulative

respfctcNV <-function(x){
  r<-pnorm(x) 
  return(r)  
}
respfctclogit <-function(x){
  r<-exp(x)/(1+exp(x)) 
  return(r)  
}
respfctcGumbel <-function(x){
  r<-exp(-exp(-x)) 
  return(r)  
}
respfctcGompertz <-function(x){
  r<-1-exp(-exp(x)) 
  return(r)  
}

####################
#################################### respfctder : der for derivative of density

respfctderNV <-function(x){
  r<--x*dnorm(x) 
  return(r)  
}
respfctderlogit <-function(x){
  r<-(exp(x)-exp(2*x))/(1+exp(x))^3 
  return(r)  
}
respfctderGumbel <-function(x){
  r<-exp(-x)*exp(-exp(-x))*(exp(-x)-1) 
  return(r)  
}
respfctderGompertz <-function(x){
  r<-exp(x)*exp(-exp(x))*(1-exp(x)) 
  return(r)  
}


###### end response fct





