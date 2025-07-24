
getwd() # check
#setwd("C:\\Users\\Gerhard Tutz\\LRZ Sync+Share\\TuRaschThresholdM\\R4Continuous\\")

####
#### at the end: extracts and fits data from Mair Physical, cognition, etc.
#### first fits and plots


library(MPsychoR)
library(plyr)
library(MASS)
library(car)
library(pracma)
library(plyr)
library(spatstat)
#library(memisc)

## run first:
setwd("~/R-Projects/MARCS/code/functions")
source("./Responsefunctions.R")
source("./Difficultyfunctions.R")
source("./ProgramsFixedResp.R")
source("./ProgramsFixedAlphaResp.R")

#source("C:/Users/Gerhard Tutz/LRZ Sync+Share/TuRaschThresholdM/R3/Responsefunctions.R")
#source("C:/Users/Gerhard Tutz/LRZ Sync+Share/TuRaschThresholdM/R3/Difficultyfunctions.R")


#### fitting  using  saved data

#dat<-read.table("aff1", header=TRUE,sep= " ")
#dat <-read.table("aff3", header=TRUE,sep= " ")
#dat <-read.table("physical1", header=TRUE,sep= " ")
#dat <-read.table("cognsums", header=TRUE,sep= " ")
setwd("~/R-Projects/MARCS/data")
dat <-read.table("physsums.txt", header=TRUE,sep= " ")

I <- dim(dat)[1]
P <- dim(dat)[2]




#### choose difficulty function (lin), response function (respf), and distribution continuous/discrete (indicator)

lin <- "lin"
lin <- "log"
lin <- "log1"
lin <- "logit"

respf<- "NV"
respf<- "Gumbel"
respf<- "Gompertz"

indicator<- "C"

### always run (1) and (2) after selection of lin and respfct   (logit in (2) is special, comes later)

####(1) define function for response function

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

####(2) define function for difficulty function

if(lin =="lin") {diffunct<-diffunctlin
derdiffunct<-derdiffunctlin}
if(lin =="log") {diffunct<-diffunctlog
derdiffunct<-derdiffunctlog}
if(lin =="log1") {diffunct<-diffunctlog1
derdiffunct<-derdiffunctlog1}


########################################
### only if(lin =="logit") compute first min, max, then define dificulty function

min<- min(dat)
max<- max(dat)
width<- max(dat)-min(dat)
c<- 0.10
minnew<-min-c*width
maxnew<-max+c*width


minnew
maxnew
diffunctlogit <-function(intdiff,slopediff,y){
  minnew<-1.06 #0.4
  maxnew<-7.54 #7.6
  r<- intdiff+slopediff*log((y-minnew)/(maxnew-y))
  return(r)
}
derdiffunctlogit <-function(intdiff,slopediff,y){
  minnew<-1.06 #0.4
  maxnew<-7.54 #7.6
  r<- slopediff*(maxnew-minnew)/((y-minnew)*(maxnew-y))
  return(r)
}

if(lin =="logit") {diffunct<-diffunctlogit
derdiffunct<-derdiffunctlogit}

### end only if(lin =="logit")
#########################################



### fixed alpha
Thrp<-ThreshModFixedResp(dat,commonslope="var" ,indicator=indicator, der="der",lin =lin, start, startind="no",respf)
Thrp

PosteriorEstimates <-function(grid,dat,I,indicator,lin =lin,parmatrest,stdest){ 
  
  ####computes centered posterior person parameters
  ##### parmatrest is prameter matrix
  
  numgrid <- length(grid)
  dens <- matrix(0,numgrid,1)
  P<-dim(dat)[2]
  esttheta <- matrix(0,P,1)
  
  for (p  in 1:P){obs<- dat[,p]
  for (l in 1:numgrid){
    theta <- grid[l]
    dens[l,1]<-prodfct(theta,datitem=obs,I=I,parmatr=parmatrest,slope=1,indicator=indicator, lin =lin)*dnorm(theta,0,stdest)
  }
  num<-  which.max(dens)
  esttheta[p]<- grid[num]
  }
  #### centering
  esttheta<-esttheta-mean(esttheta)
  
  return(esttheta)
}

check1 <-PosteriorEstimates(grid=seq(-3,5,.1),
                   dat = dat,
                   I=2,
                   indicator="C",
                   lin="lin",
                   parmatrest=Thrp$parmatrix,
                   stdest=Thrp$stdmixt)

hist(check1, breaks= 50)

require(GGally)
hist(check1)
pairs(data.frame(t(dat)) %>% mutate(`Aptitude` = as.vector(check1)))
temp_data <- data.frame(t(dat)) %>% mutate(`Aptitude` = as.vector(check1))
names(temp_data) <- c("Physical Metric 1", "Physical Metric 2", "Physical Metric 3", "Aptitude")
temp_data %>% ggpairs()
rownames(temp_data) <- NULL
print(colnames(temp_data)) 
data.frame(t(dat)) %>% mutate(est = check1) %>% ggplot(aes(x=phy1, y= phy2, color=est)) + geom_point()
data.frame(t(dat)) %>% mutate(est = check1) %>% ggplot(aes(x=phy1, y= phy3, color=est)) + geom_point()
data.frame(t(dat)) %>% mutate(est = check1) %>% ggplot(aes(x=phy2, y= phy3, color=est)) + geom_point()



### fixed alpha
tdat <- t(dat)
tdat
Thrp<-ThreshModFixedResp(dat,commonslope="var" ,indicator=indicator, der="der",lin =lin, start, startind="no",respf)
var(tdat)
colMeans(tdat)
require(mvtnorm)
for.sigma <- rbind(c(.77, -.43, .53),
      c(-.43, .61, .49),
      c(.53, .49, .68))
require(Matrix)

my.sigma <- nearPD(for.sigma <- rbind(c(.77, -.43, .53),
                          c(-.43, .61, .49),
                          c(.53, .49, .68)))$mat
my.sigma <- as.matrix(my.sigma)
my.dat <- rmvnorm(n = 194, mean = c(5.264433, 5.643041, 5.492784), sigma = my.sigma)
pairs(my.dat)

my.model.check <- ThreshModFixedResp(dat=t(my.dat),commonslope="var" ,indicator=indicator, der="der",lin =lin, start, startind="no",respf)
check2 <-PosteriorEstimates(grid=seq(-3,5,.1),
                            dat = t(my.dat),
                            I=3,
                            indicator="C",
                            lin="lin",
                            parmatrest=my.model.check$parmatrix,
                            stdest=my.model.check$stdmixt)
hist(check2, breaks=50)
data.frame(my.dat) %>% mutate(est=check1) %>% ggplot(aes(x=X1, y= X2, color=est)) + geom_point()
data.frame(my.dat) %>% mutate(est=check1) %>% ggplot(aes(x=X1, y= X3, color=est)) + geom_point()
data.frame(my.dat) %>% mutate(est=check1) %>% ggplot(aes(x=X2, y= X3, color=est)) + geom_point()
pairs(data.frame(my.dat) %>% mutate(est=check1))


my.dat.null <- rmvnorm(n = 194, mean = c(5.264433, 5.643041, 5.492784), sigma = diag(c(.85, .71, .78)))
my.model.check.null <- ThreshModFixedResp(dat=t(my.dat.null),commonslope="var" ,indicator=indicator, der="der",lin =lin, start, startind="no",respf)
check.null <-PosteriorEstimates(grid=seq(-3,5,.1),
                            dat = t(my.dat.null),
                            I=3,
                            indicator="C",
                            lin="lin",
                            parmatrest=my.model.check.null$parmatrix,
                            stdest=my.model.check.null$stdmixt)

pairs(my.dat.null)
hist(check.null, breaks= 50)

dat2 <- data.frame(t(dat))
dat2$phy1 <- (dat2$phy1 - mean(dat2$phy1)/sd(dat2$phy1))
dat2$phy2 <- (dat2$phy2 - mean(dat2$phy2)/sd(dat2$phy2))
dat2$phy3 <- (dat2$phy3 - mean(dat2$phy3)/sd(dat2$phy3))
dat2 <- t(dat2)
dat2
Thrp2<-ThreshModFixedResp(dat2,commonslope="var" ,indicator=indicator, der="der",lin =lin, start, startind="no",respf)

check2 <-PosteriorEstimates(grid=seq(-3,5,.1),
                            dat = dat2,
                            I=2,
                            indicator="C",
                            lin="lin",
                            parmatrest=Thrp2$parmatrix,
                            stdest=Thrp2$stdmixt)
plot(check1, check2)
t(dat2) %>% pairs()

### varying alpha
Thralpha<- ThreshModFixedAlphaResp(dat,commonslope="var" ,indicator=indicator, der="der",lin =lin, start, startind="no",
                                    penalpha=0,respf)
Thralpha

#discrete
#Thrd<-ThreshModFixedResp(dat,commonslope="var" ,indicator="D", der="der",lin =lin, start, startind="no",respf)








ThrGd<-ThreshModFixedResp(dat,commonslope="var" ,indicator=indicator, der="der",lin =lin, start, startind="no",respf)
ThrGd

# end fixed


#### transformed data
dattrans<- log(dat)
Thrt<-ThreshModFixedResp(dattrans,commonslope="var" ,indicator=indicator, der="der",lin =lin, start, startind="no",respf)
Thrt



####################################
#### Plots for fixed alpha

parmatrest <- Thrp$parmatrix ###
datn<-dat

min <- 1 #minnew #min(dat)-0.5
max <- 7 #maxnew  #max(dat)+0.5

#min <- 0.0 #minnew #min(dat)-0.5
#max <- 8.5

x<- seq(min,max,(max-min)/40)
pcdum <-c ('1','2','3','4','5','6')

theta <-0  # specify theta

pcdum <-c ('1','2','3','4','5','6')
ylims <- c(0,1.3)
y <- seq(min,max,(max-min)/70)



prob <- 0*y  ### only dummy
delta<-0*y  ## dummy

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


############################
#### Plots for varying alpha

parmatrest <- Thralpha$parmatrix ###
alphas<-Thralpha$alpha
datn<-dat


min <- 1 #minnew #min(dat)-0.5
max <- 7 #maxnew  #max(dat)+0.5

#min <- 0.0 #minnew #min(dat)-0.5
#max <- 8.5

x<- seq(min,max,(max-min)/40)
pcdum <-c ('1','2','3','4','5','6')

theta <-0  # specify theta

pcdum <-c ('1','2','3','4','5','6')
ylims <- c(0,2.4)
y <- seq(min,max,(max-min)/80)



prob <- 0*y  ### only dummy
delta<-0*y  ## dummy
#### with item slope, parameterization is alpha*theta -delta_oi+delta*a(y)

for (l in 1:length(y)){prob[l]<- respfctd(alphas[1]*theta - diffunct(parmatrest[1,1],parmatrest[1,2],y[l]))*
  derdiffunct(parmatrest[1,1],parmatrest[1,2],y[l])
delta[l]<-diffunct(parmatrest[1,1],parmatrest[1,2],y[l])
}
#plot(y,delta,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Difficulty",xlab ="y", type="b",lwd=1.2, ylab="", pch =pcdum[1])
sum(prob)*(y[2]-y[1])

plot(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Densities",xlab ="y", type="b",lwd=1.2,ylim=ylims,
     ylab="", pch =pcdum[1])

for (i in 2:I){

  for (l in 1:length(y))prob[l]<- {respfctd(alphas[i]*theta - diffunct(parmatrest[i,1],parmatrest[i,2],y[l]))*
      derdiffunct(parmatrest[i,1],parmatrest[i,2],y[l])}


  lines(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b",pch =pcdum[i],cex = 1)
  sum(prob)*(y[2]-y[1])
  }






#######################################
##### extract data and save


data("Lakes")
dat<-Lakes
summary(Lakes)

### affective now for rater 3
data <- Lakes[ which(Lakes$raterID=='3' & Lakes$subtest=='affective'), ]
rename(data, c("personID"="pers"))

summary(data)
table(data$pers,data$score)
table(data$pers,data$item)
dim(data)

dataw <- spread(data, item, score)
dataw

myvars <- names(dataw) %in% c("personID","raterID", "subtest")
dat <- dataw[!myvars]
dat<- na.omit(dat)

datm <- as.matrix(dat)
datt <- t(datm)
## for rater 1
aff3 <- datt #

write.table(aff3, file = "aff3", append = FALSE, sep = " ",
            row.names = TRUE, col.names = TRUE)


### physical
data <- Lakes[ which(Lakes$raterID=='1' & Lakes$subtest=='physical'), ]
rename(data, c("personID"="pers"))

summary(data)
table(data$pers,data$score)
table(data$pers,data$item)
dim(data)

dataw <- spread(data, item, score)
dataw

myvars <- names(dataw) %in% c("personID","raterID", "subtest")
dat <- dataw[!myvars]
dat<- na.omit(dat)

datm <- as.matrix(dat)
datt <- t(datm)
## for rater 1
physical1 <- datt #

write.table(physical1, file = "physical1", append = FALSE, sep = " ",
            row.names = TRUE, col.names = TRUE)


#### extract sums

data <- Lakes[ which(Lakes$subtest=='cognitive'), ]
#rename(data, c("personID"="pers"))

names(data)
data$personID<- as.numeric(data$personID)
data$raterID<- as.numeric(data$raterID)
summary(data)


dataw <- spread(data, item, score)
dataw<- na.omit(dataw)
dataw <- dataw[ , ! names(dataw) %in% c("subtest")]

dataw
summary(dataw)
lines<-dim(dataw)[1]

datnew<- 0*dataw
for (p in 1:194){
  count<-0
  for (l in 1:lines) if(dataw$personID[l]==p){datnew[p,]<-datnew[p,]+dataw[l,]
  count<-count+1}
  datnew[p,]<-datnew[p,]/count
  datnew$personID[p]<-p
}

datnew<-datnew[1:194,]
datnew <- datnew[ , ! names(dataw) %in% c("raterID")]

datm <- as.matrix(datnew)
dat  <- t(datm)
dat <- dat[2:7,]

write.table(dat, file = "cognsums", append = FALSE, sep = " ",
            row.names = TRUE, col.names = TRUE)


#### end extract cognitive

data <- Lakes[ which(Lakes$subtest=='physical'), ]
#rename(data, c("personID"="pers"))

names(data)
data$personID<- as.numeric(data$personID)
data$raterID<- as.numeric(data$raterID)
summary(data)


dataw <- spread(data, item, score)
dataw<- na.omit(dataw)
dataw <- dataw[ , ! names(dataw) %in% c("subtest")]

dataw
summary(dataw)

lines<-dim(dataw)[1]

datnew<- 0*dataw
for (p in 1:194){
  count<-0
  for (l in 1:lines) if(dataw$personID[l]==p){datnew[p,]<-datnew[p,]+dataw[l,]
  count<-count+1}
  datnew[p,]<-datnew[p,]/count
  datnew$personID[p]<-p
}
datnew<-datnew[1:194,]


datnew <- datnew[ , ! names(dataw) %in% c("raterID")]

datm <- as.matrix(datnew)
dat  <- t(datm)
dat <- dat[2:4,]

write.table(dat, file = "physsums", append = FALSE, sep = " ",
            row.names = TRUE, col.names = TRUE)





