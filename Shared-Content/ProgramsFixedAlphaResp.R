


################### General Fit function for fixed difficulty functions with varying
#################   response functions and discriminatory parameters  alpha
#######################################################################






ThreshModFixedAlphaResp <- function(dat,commonslope ,indicator=indicator, der=der,lin =lin, start, startind,
                                    penalpha,respf){

  #### with item slope, parameterization is alpha*theta -delta_oi+delta*a(y)
  #### item slope last item: 1


  #datt I x P matrix responses

  #commonslope
  ### com: common slope in difficulty functions
  ### var varying slopes

  #indicator  <- "D"
  ###   C: continuous
  ###   D: dicrete

  #lin <- "log"
  ### lin: linear item difficulties
  ### log: log(1+y)
  ### inv: logit for transformed data, only indicator == C

  #der <- "no"  #with derivative (only for varying slope, if discrete only for log difficulty)
  ###  der: with derivative
  ### no: without

  # start  are starting values, used if startind "yes"

  # penalpha: tuning parameter penalizes deviation of alpha from 1

  # output I x 2 matrix for splines = fixed
  #        I  x bnumbasis for splines =S

  datt <- dat
  I <- dim(datt)[1]
  P <- dim(datt)[2]

  stderr<-0
  itemsums <- rowSums(datt)/P
  deriv <- 0
  ### data transformation if lin= inv
  if(lin=="inv"){
    datorig <-datt
    width<- max(datt)-min(datt)
    norm<- 10
    mintr <- min(datt)- width/norm
    maxtr <- max(datt)+ width/norm
    datstd<- (datt -mintr)/(maxtr-mintr)
    datt <-datstd}

  ##########################
  ### for fixed slope without derivative

  if (commonslope == "com"){
    #par <- 0.2*seq(1,I+1,1)
    par1 <- -itemsums/3   #####  /3 included!!!!!!!
    par <- c(par1,1,1)
    par <-c(par,rep(1,I))  ## with item slope
    if(startind == "yes") par <- start

    ##### alt ohne stdmixt
    #fit <- optim(par, LoglikInt, gr = NULL,dat=datt,I=I, indicator ="D", method = "Nelder-Mead",
    #                lower = -Inf, upper = Inf, control = list(), hessian = FALSE)

    #incpt <- fit$par[1:I]
    #slope <- rep(fit$par[I+1],I)
    # parmatrest <- cbind(incpt,slope)

    #neu#


    fit <- optim(par, LoglikIntExtNAlpha, gr = NULL,dat=datt,I=I, indicator = indicator, lin =lin, method = "Nelder-Mead",
                 lower = -Inf, upper = Inf,
                 control = list(), hessian = FALSE)

    incpt <- fit$par[1:I]
    slope <- rep(fit$par[I+1],I)
    parmatrest <- cbind(incpt,slope)
    stdmixt <- fit$par[I+2]
    #parres<-fitd$par
  }

  ### for varying slope
  if (commonslope == "var"){

    par1 <- -itemsums/5  #####  /3 included!!!!!!!
    par2 <- rep(1,I)
    par <- c(par1,par2,1)
    par <-c(par,rep(1,I-1))  ## with item slope

    if(startind == "yes") par <- start

    if (der == "der"){
      #####   mit derivative

      ##partr<-par[8:10]<-c(1,2,3)
      #LoglikNAlpha(par ,dat,I,indicator, lin=lin )

      fit <- optim(par, LoglikNAlpha, gr = derLoglikNAlpha, dat=datt,I=I,indicator =indicator, lin =lin,  method = "BFGS",
                   lower = -Inf, upper = Inf,control = list(maxit=300), hessian = FALSE,penalpha)
      #print(fit)

      #fit <- optim(par, LoglikN, gr = derLoglikN, dat=datt,I=I,indicator =indicator, lin =lin,  method = "BFGS",
      #             lower = -Inf, upper = Inf,control = list(maxit=800,pgtol = 0, ndeps = rep(1e-6, 11), factr=0), hessian = FALSE)
      up<- 2*I
      parmatrest <- matrix(fit$par[1:up],I,2)
      up1<-up+1
      stdmixt <- fit$par[up1:up1]
      up2<-up1+1
      up3<-up2+I-2
      alpha<-fit$par[up2:up3]
      alpha<-c(alpha,1)



      #parres<-fit$par
      #penalisiert

      #fit <- optim(par, LoglikNPen, gr = derLoglikNPen, dat=datt,I=I,indicator =indicator, lin =lin, Pen=1010,
      #             method = "BFGS", lower = -Inf, upper = Inf,control = list(), hessian = FALSE)

    } ### end der

    if (der != "der"){
      ####

      fit <- optim(par, LoglikNAlpha, gr = NULL, dat=datt,I=I,indicator =indicator, lin =lin,  method = "Nelder-Mead",
                   lower = -Inf, upper = Inf, control = list(maxit=500), hessian = FALSE,penalpha)



      up<- 2*I
      parmatrest <- matrix(fit$par[1:up],I,2)
      up1<-up+1
      stdmixt <- fit$par[up1:up1]
      up2<-up1+1
      up3<-up2+I-2
      alpha<-fit$par[up2:up3]
      alpha<-c(alpha,1)
      #parres<-fit$par
    }#### end der

    #   ###derivative
    #
    #LoglikNAlpha(par,datt,I,indicator, lin =lin,penalpha=0)
    loglik <-LoglikNAlpha(fit$par,datt,I,indicator, lin =lin,penalpha=0)
    deriv<-0
    #if (indicator =="C")deriv<-derLoglikNAlpha(fit$par,datt,I,indicator, lin =lin,penalpha)
    deriv<-derLoglikNAlpha(fit$par,datt,I,indicator, lin =lin,penalpha)
  } ### end common slope var




  ### positive stddev
  stdmixt<-abs(stdmixt)
  fit$par[2*I+1] <-abs(fit$par[2*I+1])


  #### reparameterization
  parmatrestwithoutbrack<-parmatrest
  parmatrest[,1]<- parmatrestwithoutbrack[,1]/alpha
  parmatrest[,2]<- parmatrestwithoutbrack[,2]/alpha

  ##############################################

  ####
  AIC <-  2*fit$value +2*length(par)
  BIC <-  2*fit$value +2*log(length(par))

  newList <- list("parmatrix" = parmatrest,"parmatrixwithoutbrack" = parmatrestwithoutbrack,
                  "alpha"=alpha,"stdmixt"=stdmixt,"Loglik"=-loglik,
                  "Loglikpen"=-fit$value,"par"=fit$par,
                  "convergence"=fit$convergence,"AIC"=AIC, "BIC"=BIC, "deriv"=deriv)
  return(newList)
}
################################################################

############################# negative Loglikelihood function mit stdmixt
LoglikNAlpha <-function(par,dat,I,indicator, lin=lin,penalpha ){

  #### with penalty for negative alpha

  indmix<- 2*I
  sdnormal<- par[indmix+1]
  paroriginal <- par[1:indmix]
  parmatr <- matrix(paroriginal,I,2)

  lo<-indmix+2
  up<-indmix+1+I-1

  slope <- par[lo:up] ### slope fixed
  slope<- c(slope,1)  ### last item slope fixed

  P <- dim(dat)[2]
  sum <- 0
  for (p in 1:P){
    datitem <- dat[,p]

    int <-gauss.hermite(prodfctAlpha, mu = 0, sd = sdnormal,datitem=datitem,I=I,parmatr=parmatr,slope=slope,
                        indicator=indicator, lin =lin, order = 10)
    sum <- sum +log(int)

    #printres <- c("loglik-person",p, par,"log-int",log(int))
    #print(printres)
  }
  #prodfctAlpha<-function(theta,datitem=datitem,I=I,parmatr=parmatr,slope=slope,indicator=indicator, lin =lin)
  ###penalty for alpha
  #pen1<-0
  #for (i in 1:I){pen1  <-pen1 + slope[i]^2 *indicatt(-slope[i] , 0)}
  #sum <- sum -  100000*pen1 ### + weil neg

  pen2<-sum((slope-1)^2)
  #for (i in 2:I){ dif<-slope[i]-slope[i-1]
  #  pen2  <-pen2 + dif^2 *indicatt(-dif , 0)}
  sum <- sum -  penalpha*pen2 ### + weil neg

  #####

  sum<- -sum

  #printres <- c("loglik-par",par,"loglik-sum",sum)
  #print(printres)
  return(sum)}
###############

##################################
indicatt <-function(x,thresh){
  ret <-0
  if (x > thresh) ret <-1
  return(ret)}
#########################


prodfctAlpha<-function(theta,datitem=datitem,I=I,parmatr=parmatr,slope=slope,indicator=indicator, lin =lin){
  prod <-1
  if(indicator=="C"){
    for (i in 1:I){
      iteml <- parmatr[i,]
      prod <- prod*deltalinNVdensAlpha(datitem[i],iteml,slope[i] , theta, lin =lin)}
  } #end if

  if(indicator=="D"){
    for (i in 1:I){
      iteml <- parmatr[i,]
      prod <- prod*deltalinNVdensDiscrAlpha(datitem[i],iteml,slope[i] , theta, lin =lin)}
  } #end if

  return(prod)}

########################### here linear or log
deltalinNVdensAlpha <-function(y,iteml,slope , theta, lin=lin){
  #### computes density function continuous
  ### y vector of values
  ### iteml lineare item coefficienten oder log - auskommentieren notwendig
  DFval <- matrix(0,length(y),1)
  for (i in 1:length(y)){
    val <- diffunct(iteml[1],iteml[2],y[i])
    DFval[i,1]<-   respfctd(slope*theta - val)*derdiffunct(iteml[1],iteml[2],y[i])


    ##### linear
    #if(lin =="lin") {val <- iteml[1] + iteml[2]*y[i]
    #DFval[i,1]<-   dnorm(slope*theta - val)*iteml[2]}
    #####  log  for positive y
    #if(lin =="log"){val <- iteml[1] + iteml[2]*log(1+y[i])
    #DFval[i,1]<-   dnorm(slope*theta - val)*iteml[2]/(1+y[i])}  ###???

    #####  inv
    #if(lin =="inv"){val <- iteml[1] + iteml[2]*log(y[i]/(1-y[i]))
    #DFval[i,1]<-   dnorm(slope*theta - val)*iteml[2]/(y[i]*(1-y[i]))}

  }
  return(DFval)}
#########################

###########################
deltalinNVdensR <-function(y,iteml,slope , theta){
  #### computes density function binary Rasch, eigentlich ogive
  ### y vector of values
  ### iteml lineare item coefficienten
  DFval <- matrix(0,length(y),1)
  val <- iteml[1]              #################### hier
  for (i in 1:length(y)){
    # Rasch
    #exp1 <- exp((slope*theta - val))/(1+exp(slope*theta - val))
    #   DFval[i,1]<-  (1-y[i])*exp1+ y[i]*exp1)}
    # Ogive
    DFval[i,1]<-   (1-y[i])*pnorm(slope*theta - val)+y[i]*(1-pnorm(slope*theta - val))}
  return(DFval)}
#########################

########################### here  linear or log
deltalinNVdensDiscrAlpha <-function(y,iteml,slope , theta,lin=lin){
  #### computes discrete density for count data
  ### y vector of values
  ### iteml  item coefficienten

  DFval <- matrix(0,length(y),1)
  for (i in 1:length(y)){

    #val <- iteml[1] + iteml[2]*y[i]


    if(y[i] <= 0){val <- iteml[1]
    DFval[i,1] <- 1- pnorm(slope*theta - val)}

    if(y[i] > 0){
      ##### linear
      if(lin =="lin") {valr <- iteml[1] + iteml[2]*y[i]
      valr1 <- iteml[1] + iteml[2]*(y[i])}
      #####  log  for positive y
      if(lin =="log"){valr <- iteml[1] + iteml[2]*log(1+y[i])
      valr1 <- iteml[1] + iteml[2]*log(1+y[i]-1)}

      DFval[i,1]<-   pnorm(slope*theta - valr1)-pnorm(slope*theta - valr)}
  }
  return(DFval)}
#########################





############################# derivative negative Loglikelihood function
derLoglikNAlpha <-function(par,dat,I,indicator, lin =lin,penalpha){
  ## works for lin = lin or log (difficulty functions)
  ## N version with stdmixt
  ### wohl nur f?r stetige und varying slopes
  ### stmixt computed numerically, delta<- 0.0001

  delta<- 0.0001
  lpar <- 2*I+1+I-1

  der <- matrix(0,lpar,1)

  indmix<- 2*I
  paroriginal <- par[1:indmix]
  parmatr <- matrix(paroriginal,I,2)

  lo<-indmix+2
  up<-indmix+1+I-1

  slope <- par[lo:up] ### slope fixed
  slope<- c(slope,1)


  stdmixt <- par[2*I+1]
  P <- dim(dat)[2]

  ### nominator
  # intercepts

  for (p in 1:P){
    datitem <- dat[,p]
    ### denominator
    intn <-gauss.hermite(prodfctAlpha, mu = 0, sd = stdmixt,datitem=datitem,I=I,parmatr=parmatr,slope=slope,
                         indicator=indicator, lin =lin,order = 10)
    for (l in 1:I){
      comp <- l
      der[l,1] <-der[l,1]+gauss.hermite(prodfctderiv1Alpha, mu = 0, sd = stdmixt,datitem=datitem,I=I,parmatr=parmatr,
                                        slope=slope,indicator=indicator, lin =lin,order = 10,comp=comp)/intn
    }} # end p

  #slopes
  for (p in 1:P){
    datitem <- dat[,p]
    ### denominator
    intn <-gauss.hermite(prodfctAlpha, mu = 0, sd = stdmixt,datitem=datitem,I=I,parmatr=parmatr,slope=slope,
                         indicator=indicator, lin =lin,order = 10)
    for (l in  1:I){
      comp <- l
      der[I+l,1] <-der[I+l,1]+gauss.hermite(prodfctderiv2Alpha, mu = 0, sd = stdmixt,datitem=datitem,I=I,parmatr=parmatr,
                                            slope=slope,indicator=indicator, lin =lin,order = 10,comp=comp)/intn
    }} # end p


  # stdmixt with formula - not used
  omit <-1
  if(omit < 0){
    for (p in 1:P){
      datitem <- dat[,p]
      ### denominator
      intn <-gauss.hermite(prodfctAlpha, mu = 0, sd = stdmixt,datitem=datitem,I=I,parmatr=parmatr,slope=slope,
                           indicator=indicator, lin =lin,order = 10)
      smixt <- par[lpar]




      der[lpar,1] <-der[lpar,1]+gauss.hermite(prodfctderiv3Alpha, mu = 0, sd = stdmixt,datitem=datitem,I=I,parmatr=parmatr,
                                              slope=slope,indicator=indicator, lin =lin,order = 10,smixt)/intn
    } # end p
  } #end omit


  # alphas
  for (p in 1:P){
    datitem <- dat[,p]
    ### denominator
    intn <-gauss.hermite(prodfctAlpha, mu = 0, sd = stdmixt,datitem=datitem,I=I,parmatr=parmatr,slope=slope,
                         indicator=indicator, lin =lin,order = 10)
    I1<-I-1
    for (l in  1:I1){
      comp <- l
      der[2*I+1+l,1] <-der[2*I+1+l,1]+gauss.hermite(prodfctderivalphaAlpha, mu = 0, sd = stdmixt,datitem=datitem,I=I,parmatr=parmatr,
                                                    slope=slope,indicator=indicator, lin =lin,order = 10,comp=comp)/intn
    }} # end p



  #### now numeric

  vect1<- par
  vect1[2*I+1] <- vect1[2*I+1]+delta
  loglikorig  <- LoglikNAlpha(par,dat,I,indicator, lin=lin,penalpha)
  loglikorignow <- LoglikNAlpha(vect1,dat,I,indicator, lin=lin,penalpha)

  derivnow <- (loglikorignow-loglikorig)/delta
  der[2*I+1,1]<- -derivnow


  ##### end numeric


  der <- -der
  #printres <- c("der",par,"der",der)
  #print(printres)
  return(der)}
#################################################



##### GH function derivative   continuous
prodfctderiv1Alpha <-function(theta,datitem=datitem,I=I,parmatr=parmatr,slope=slope,indicator=indicator, lin=lin,
                              comp=comp){

  ###### 1 for intercepts
  ### comp is item
  ### only for indicator C

  set <- seq(1,I,1)

  if(indicator=="C"){


    #if(lin=="linoo"){
      iteml <- parmatr[comp,]
      val<-diffunct(iteml[1],iteml[2],datitem[comp])
      prod<- -respfctder(slope[comp]*theta - val)*derdiffunct(iteml[1],iteml[2],datitem[comp])

      nonset <- set[-comp]
      for (i in nonset){
        iteml <- parmatr[i,]
        slopel<-slope[i]
        prod <- prod*deltalinNVdensAlpha(datitem[i],iteml,slopel , theta, lin =lin) }
    #}# end if lin

    if(lin=="logoo"){
      iteml <- parmatr[comp,]
      arg <- slope[comp]*theta - iteml[1]-iteml[2]*log(1+datitem[comp])
      prod <- exp(- arg*arg/2)*arg*iteml[2]/((1+datitem[comp])*sqrt(2*pi))

      nonset <- set[-comp]
      for (i in nonset){
        iteml <- parmatr[i,]
        slopel<-slope[i]
        prod <- prod*deltalinNVdensAlpha(datitem[i],iteml,slopel , theta, lin =lin) }
    }# end if lin

  } #end if C


  #### trial Dec 21

  if(indicator=="D"){
    if(lin=="log"){
      iteml <- parmatr[comp,]
      if (datitem[comp]>0){
        arg1 <- slope[comp]*theta - iteml[1]-iteml[2]*log(1+datitem[comp]-1)
        arg2 <- slope[comp]*theta - iteml[1]-iteml[2]*log(1+datitem[comp])
        prod <- -dnorm(arg1,0,1)+dnorm(arg2,0,1)}
      if (datitem[comp]<=0){arg2 <- slope[comp]*theta - iteml[1]-iteml[2]*log(1+datitem[comp])
      prod <- dnorm(arg2,0,1)}

      nonset <- set[-comp]
      for (i in nonset){
        iteml <- parmatr[i,]
        slopel<-slope[i]
        prod <- prod*deltalinNVdensDiscr(datitem[i],iteml,slopel , theta, lin =lin) }
    } # end if lin
  } #end if

  return(prod)}

########################


##### GH function derivative   continuous
prodfctderiv2Alpha <-function(theta,datitem=datitem,I=I,parmatr=parmatr,slope=slope,indicator=indicator, lin=lin,
                              comp=comp){

  ###### 2 for slopes
  ### comp is item
  ### only for indicator C

  set <- seq(1,I,1)

  if(indicator=="C"){
    #### new general, rest neutralized
    iteml <- parmatr[comp,]
    val<-diffunct(iteml[1],iteml[2],datitem[comp])

    prod<- {-diffunct(0,1,datitem[comp])*respfctder(slope[comp]*theta - val)*derdiffunct(iteml[1],iteml[2],datitem[comp])+
        respfctd(slope[comp]*theta - val)*derdiffunct(0,1,datitem[comp])}

    nonset <- set[-comp]
    for (i in nonset){
      iteml <- parmatr[i,]
      slopel<-slope[i]
      prod <- prod*deltalinNVdens(datitem[i],iteml,slopel , theta, lin =lin)}
    #####


    if(lin=="lin00"){
      iteml <- parmatr[comp,]
      arg <- slope[comp]*theta - iteml[1]-iteml[2]*datitem[comp]  ####?
      expterm <- exp(- arg*arg/2)
      prod <- (expterm+ expterm*arg*iteml[2]*datitem[comp] )/sqrt(2*pi)

      nonset <- set[-comp]
      for (i in nonset){
        iteml <- parmatr[i,]
        slopel<-slope[i]
        prod <- prod*deltalinNVdensAlpha(datitem[i],iteml,slopel , theta, lin =lin) }
    } #end if lin

    if(lin=="log00"){
      iteml <- parmatr[comp,]
      arg <- slope[comp]*theta - iteml[1]-iteml[2]*log(1+datitem[comp])
      expterm <- exp(- arg*arg/2)
      prod <- (expterm+ expterm*arg*iteml[2]*log(1+datitem[comp]) )/((1+datitem[comp])*sqrt(2*pi))

      nonset <- set[-comp]
      for (i in nonset){
        iteml <- parmatr[i,]
        slopel<-slope[i]
        prod <- prod*deltalinNVdensAlpha(datitem[i],iteml,slopel , theta, lin =lin) }
    } #end if lin

  } #end if C


  if(indicator=="D"){
    if(lin=="log"){
      iteml <- parmatr[comp,]
      if (datitem[comp]>0){
        arg1 <- slope[comp]*theta - iteml[1]-iteml[2]*log(1+datitem[comp]-1)
        arg2 <- slope[comp]*theta - iteml[1]-iteml[2]*log(1+datitem[comp])
        prod <- -dnorm(arg1,0,1)*log(datitem[comp])+dnorm(arg2,0,1)*log(1+datitem[comp])}
      if (datitem[comp]<=0){arg2 <- slope[comp]*theta - iteml[1]-iteml[2]*log(1+datitem[comp])
      prod <- dnorm(arg2,0,1)*log(1+datitem[comp])}

      nonset <- set[-comp]
      for (i in nonset){
        iteml <- parmatr[i,]
        slopel<-slope[i]
        prod <- prod*deltalinNVdensDiscr(datitem[i],iteml,slopel , theta, lin =lin) }
    } # end if lin
  } #end if

  return(prod)}


###################################
prodfctderivalphaAlpha <-function(theta,datitem=datitem,I=I,parmatr=parmatr,slope=slope,indicator=indicator, lin=lin,
                                  comp=comp){

  ###### alpha for alpha
  ### comp is item
  ### only for indicator C

  set <- seq(1,I,1)

  #####
  iteml <- parmatr[comp,]
  val<-diffunct(iteml[1],iteml[2],datitem[comp])
  prod<- theta*respfctder(slope[comp]*theta - val)*derdiffunct(iteml[1],iteml[2],datitem[comp])

  #####

  if(indicator=="C"){
    #####
    iteml <- parmatr[comp,]
    val<-diffunct(iteml[1],iteml[2],datitem[comp])
    prod<- theta*respfctder(slope[comp]*theta - val)*derdiffunct(iteml[1],iteml[2],datitem[comp])

    nonset <- set[-comp]
    for (i in nonset){
      iteml <- parmatr[i,]
      slopel<-slope[i]
      prod <- prod*deltalinNVdensAlpha(datitem[i],iteml,slopel , theta, lin =lin)}
    #####


    if(lin=="lin00"){
      iteml <- parmatr[comp,]
      arg <- slope[comp]*theta - iteml[1]-iteml[2]*datitem[comp]  ####?
      prod <- -exp(- arg*arg/2)*arg*iteml[2]*theta/sqrt(2*pi)

      nonset <- set[-comp]
      for (i in nonset){
        iteml <- parmatr[i,]
        slopel<-slope[i]
        prod <- prod*deltalinNVdensAlpha(datitem[i],iteml,slopel , theta, lin =lin) }
    }# end if lin

    if(lin=="log00"){
      iteml <- parmatr[comp,]
      arg <- slope[comp]*theta - iteml[1]-iteml[2]*log(1+datitem[comp])
      prod <- -exp(- arg*arg/2)*arg*iteml[2]*theta/((1+datitem[comp])*sqrt(2*pi))

      nonset <- set[-comp]
      for (i in nonset){
        iteml <- parmatr[i,]
        slopel<-slope[i]
        prod <- prod*deltalinNVdensAlpha(datitem[i],iteml,slopel , theta, lin =lin) }
    }# end if lin

  } #end if C


  #### trial Dec 21

  if(indicator=="D"){
    if(lin=="log"){
      iteml <- parmatr[comp,]
      if (datitem[comp]>0){
        arg1 <- slope[comp]*theta - iteml[1]-iteml[2]*log(1+datitem[comp]-1)
        arg2 <- slope[comp]*theta - iteml[1]-iteml[2]*log(1+datitem[comp])
        prod <- theta*dnorm(arg1,0,1)-theta*dnorm(arg2,0,1)}
      if (datitem[comp]<=0){arg2 <- slope[comp]*theta - iteml[1]-iteml[2]*log(1+datitem[comp])
      prod <- -theta*dnorm(arg2,0,1)}

      nonset <- set[-comp]
      for (i in nonset){
        iteml <- parmatr[i,]
        slopel<-slope[i]
        prod <- prod*deltalinNVdensDiscr(datitem[i],iteml,slopel , theta, lin =lin) }
    } # end if lin
  } #end if

  return(prod)}

########################


#### wohl unn?tig, nicht modifiziert
derLoglikNAlphaDiscr <-function(par,dat,I,indicator, lin =lin,penalpha){
  ## works for lin = lin or log (difficulty functions)
  ## N version with stdmixt
  ### wohl nur f?r stetige und varying slopes
  ### stmixt computed numerically, delta<- 0.0001

  delta<- 0.0001
  lpar <- 2*I+1+I-1

  der <- matrix(0,lpar,1)

  indmix<- 2*I
  paroriginal <- par[1:indmix]
  parmatr <- matrix(paroriginal,I,2)

  lo<-indmix+2
  up<-indmix+1+I-1

  slope <- par[lo:up] ### slope fixed
  slope<- c(slope,1)


  stdmixt <- par[2*I+1]
  P <- dim(dat)[2]

  ### nominator
  # intercepts

  for (p in 1:P){
    datitem <- dat[,p]
    ### denominator
    intn <-gauss.hermite(prodfctAlpha, mu = 0, sd = stdmixt,datitem=datitem,I=I,parmatr=parmatr,slope=slope,
                         indicator=indicator, lin =lin,order = 10)
    for (l in 1:I){
      comp <- l
      der[l,1] <-der[l,1]+gauss.hermite(prodfctderiv1Alpha, mu = 0, sd = stdmixt,datitem=datitem,I=I,parmatr=parmatr,
                                        slope=slope,indicator=indicator, lin =lin,order = 10,comp=comp)/intn
    }} # end p

  #slopes
  for (p in 1:P){
    datitem <- dat[,p]
    ### denominator
    intn <-gauss.hermite(prodfctAlpha, mu = 0, sd = stdmixt,datitem=datitem,I=I,parmatr=parmatr,slope=slope,
                         indicator=indicator, lin =lin,order = 10)
    for (l in  1:I){
      comp <- l
      der[I+l,1] <-der[I+l,1]+gauss.hermite(prodfctderiv2Alpha, mu = 0, sd = stdmixt,datitem=datitem,I=I,parmatr=parmatr,
                                            slope=slope,indicator=indicator, lin =lin,order = 10,comp=comp)/intn
    }} # end p


  # stdmixt with formula - not used
  omit <-1
  if(omit < 0){
    for (p in 1:P){
      datitem <- dat[,p]
      ### denominator
      intn <-gauss.hermite(prodfctAlpha, mu = 0, sd = stdmixt,datitem=datitem,I=I,parmatr=parmatr,slope=slope,
                           indicator=indicator, lin =lin,order = 10)
      smixt <- par[lpar]




      der[lpar,1] <-der[lpar,1]+gauss.hermite(prodfctderiv3Alpha, mu = 0, sd = stdmixt,datitem=datitem,I=I,parmatr=parmatr,
                                              slope=slope,indicator=indicator, lin =lin,order = 10,smixt)/intn
    } # end p
  } #end omit


  # alphas
  for (p in 1:P){
    datitem <- dat[,p]
    ### denominator
    intn <-gauss.hermite(prodfctAlpha, mu = 0, sd = stdmixt,datitem=datitem,I=I,parmatr=parmatr,slope=slope,
                         indicator=indicator, lin =lin,order = 10)
    I1<-I-1
    for (l in  1:I1){
      comp <- l
      der[2*I+1+l,1] <-der[2*I+1+l,1]+gauss.hermite(prodfctderivalphaAlpha, mu = 0, sd = stdmixt,datitem=datitem,I=I,parmatr=parmatr,
                                                    slope=slope,indicator=indicator, lin =lin,order = 10,comp=comp)/intn
    }} # end p



  #### now numeric

  vect1<- par
  vect1[2*I+1] <- vect1[2*I+1]+delta
  loglikorig  <- LoglikNAlpha(par,dat,I,indicator, lin=lin,penalpha)
  loglikorignow <- LoglikNAlpha(vect1,dat,I,indicator, lin=lin,penalpha)

  derivnow <- (loglikorignow-loglikorig)/delta
  der[2*I+1,1]<- -derivnow


  ##### end numeric


  der <- -der
  #printres <- c("der",par,"der",der)
  #print(printres)
  return(der)}
#####



