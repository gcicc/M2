
library(spatstat)


library(diffIRT)
data("rotation")
summary(rotation)

rotfull<-na.omit(rotation)
datrot<-t(rotfull[,11:20])
datrot


### selection of difficulty function (lin) and response function (respfct)

## choose lin
lin <- "log"  

## choose respf (choose one of the following)

respf<- "NV"
#respf<- "Gumbel"
#respf<-"Gompertz"

##  run (defines functions)

if(lin =="lin") {diffunct<-diffunctlin
derdiffunct<-derdiffunctlin}
if(lin =="log") {diffunct<-diffunctlog
derdiffunct<-derdiffunctlog}
if(lin =="log1") {diffunct<-diffunctlog1
derdiffunct<-derdiffunctlog1}
if(lin =="logit") {diffunct<-diffunctlogit
derdiffunct<-derdiffunctlogit}

if(respf=="NV"){respfctd <- respfctdNV
respfctc<-respfctcNV
respfctder<-respfctderNV}
if(respf=="logit"){respfctd <- respfctdlogit
respfctc<-respfctclogit
respfctder<-respfctderlogit}
if(respf=="Gumbel"){respfctd <- respfctdGumbel
respfctc<-respfctcGumbel
respfctder<-respfctderGumbel}
if(respf=="Gompertz"){respfctd <- respfctdGompertz
respfctc<-respfctcGompertz
respfctder<-respfctderGompertz}



indicator<-"C"

## NV logarithmic 
Thrrot<-ThreshModFixedResp(datrot,commonslope="var" ,indicator=indicator, der="der",lin =lin, start, startind="no",respf)
Thrrot
LoglikN(Thrrot$par,datrot,dim(datrot)[1],indicator, lin=lin )


Thrrotgumb<-ThreshModFixedResp(datrot,commonslope="var" ,indicator=indicator, der="der",lin =lin, start, startind="no",respf)
Thrrotgumb

Thrrotgomp<-ThreshModFixedResp(datrot,commonslope="var" ,indicator=indicator, der="der",lin =lin, start, startind="no",respf)
Thrrotgomp

## commonslopes
Thrrotcom<-ThreshModFixedResp(datrot,commonslope="com" ,indicator=indicator, der="der",lin =lin, start, startind="no",respf)
Thrrotcom

Thrrotcom1<-ThreshModFixedResp(datrot,commonslope="com" ,indicator=indicator, der="der",lin =lin, start=Thrrotcom1$par,
                               startind="yes",respf)
Thrrotcom1

Thrrotgompt<-ThreshModFixedResp(datrot,commonslope="var" ,indicator=indicator, der="der",lin =lin, start=Thrrotcom1$par,
                                startind="yes",respf)
Thrrotgompt



#### Plots without alpha
parmatrest <- Thrrot$parmatrix ###
datn<-dat

min <- 0 #minnew #min(dat)-0.5
max <- 14 #maxnew  #max(dat)+0.5

#min <- 0.0 #minnew #min(dat)-0.5
#max <- 8.5

x<- seq(min,max,(max-min)/40)
pcdum <-c ('1','2','3','4','5','6')

theta <-  1.0  # specify theta

pcdum <-c ('1','2','3','4','5','6','7','8')
ylims <- c(0,0.7)
y <- seq(min,max,(max-min)/70)

I<-5


prob<-matrix(0,length(y),1)
delta<-matrix(0,length(y),1)

for (l in 1:length(y)){prob[l]<- respfctd(theta - diffunct(parmatrest[1,1],parmatrest[1,2],y[l]))*
  derdiffunct(parmatrest[1,1],parmatrest[1,2],y[l])
delta[l]<-diffunct(parmatrest[1,1],parmatrest[1,2],y[l])
}  
#plot(y,delta,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Difficulty",xlab ="y", type="b",lwd=1.2, ylab="", pch =pcdum[1])
sum(prob)*(y[2]-y[1]) 

plot(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Densities",xlab ="y", type="b",lwd=1.2,ylim=ylims,
     ylab="", pch =pcdum[1])

for (i in 2:I){
  
  for (l in 1:length(y))prob[l]<- {respfctd(theta - diffunct(parmatrest[i,1],parmatrest[i,2],y[l]))*
      derdiffunct(parmatrest[i,1],parmatrest[i,2],y[l])} 
  
  
  lines(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b",pch =pcdum[i],cex = 1)
}


### with discrimination
#ThreshModFixedAlphaResp(dat,commonslope ,indicator=indicator, der=der,lin =lin, start, startind, penalpha,respf)

Thrrotalpha<-ThreshModFixedAlphaResp(datrot,commonslope="var" ,indicator=indicator, der="der",lin =lin, start, startind="no",
                           penalpha=0,respf)
Thrrotalpha

 

  