
############################################################
##### functions for difficulty function specified by lin  
############################################################



# available: lin<-"lin", lin<-"log",  lin<-"log1" (means log(1+.), lin<-"logit", 



###############################################

##### linear
diffunctlin <-function(intdiff,slopediff,y){
  r<- intdiff+slopediff*y
  return(r)  
}
derdiffunctlin <-function(intdiff,slopediff,y){
  r<- slopediff
  return(r)  
}
#########log
diffunctlog <-function(intdiff,slopediff,y){
  r<- intdiff+slopediff*log(y)
  return(r)  
}
derdiffunctlog <-function(intdiff,slopediff,y){
  r<- slopediff/y
  return(r)  
}
#########log1
diffunctlog1 <-function(intdiff,slopediff,y){
  r<- intdiff+slopediff*log(1+y)
  return(r)  
}
derdiffunctlog1 <-function(intdiff,slopediff,y){
  r<- slopediff/(1+y)
  return(r)  
}
#########logit
diffunctlogit <-function(intdiff,slopediff,y){
  r<- intdiff+slopediff*log(y/(1-y))
  return(r)  
}
derdiffunctlogit <-function(intdiff,slopediff,y){
  r<- slopediff/(y*(1-y))
  return(r)  
}

