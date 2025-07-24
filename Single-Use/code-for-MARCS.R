require(mcr)

# 2.1	Analysis Module: Bias and Linearity-------------------------------------
# Step 0: Define input dataset
# Is the intention to repeat this for each reader and/or use to pooled set?
data(creatinine,package="mcr")  # Replace with data set in

#### DEMING REGRESSION
deming.regression <- function(data.in = creatinine,
                              x = "serum.crea", mref.name = "Expert Panel",
                              y = "plasma.crea", mtest.name = "Human1"){


x <- data.in %>% pull(x)
y <- data.in %>% pull(y)

# Deming regression fit.
# The confidence intercals for regression coefficients
# are calculated with analytical method
output1 <- mcreg(x, y ,error.ratio = 1, method.reg = "Deming", method.ci = "analytical",
               mref.name = mref.name, mtest.name = "Human or ML Reader", na.rm=TRUE)

coefs <- data.frame(coef(output1))

intercept.check <- abs(coefs$EST[1]) < 0.5
slope.check <- coefs$EST[2] > 0.8 & coefs$EST[2] < 1.25
linearity.check <- intercept.check * slope.check

return(coef(output1), intercept.check, slope.check, linearity.check)
}


my.model <- deming.regression()
summary(my.model)
getCoefficients(my.model)
plot(my.model)

# Confidence intervals for paired differences----------------------------------
# the two-sided 95% confidence interval will be reported for the mean of the paired differences between
# the measured scores and their corresponding true values based on the measurements of the “truth panel.”

my.data <- data.frame(X = rep(rnorm(40, 0, 1), 3), Y = rnorm(120, 0 ,1), reader=rep(1:3, each = 40))
my.data
get.CI <- function(data.in = my.data, by.reader=TRUE){

  if(by.reader == TRUE){
    my.data %>%
      mutate(difference = Y - X) %>%
      group_by(reader) %>%
      summarize(mean.diff=mean(difference, na.rm=TRUE),
                sd.diff = sd(difference, na.rm=TRUE),
                n=n()) %>%
      mutate(LCI = mean.diff - qt(df = n-1, p = .975)*sd.diff/sqrt(n),
             UCI = mean.diff + qt(df = n-1, p = .975)*sd.diff/sqrt(n))
  }

  if(by.reader == FALSE){
    my.data %>%
      mutate(difference = Y - X) %>%
      summarize(mean.diff=mean(difference, na.rm=TRUE),
                sd.diff = sd(difference, na.rm=TRUE),
                n=n()) %>%
      mutate(LCI = mean.diff - qt(df = n-1, p = .975)*sd.diff/sqrt(n),
             UCI = mean.diff + qt(df = n-1, p = .975)*sd.diff/sqrt(n),
             reader = "Pooled Readers") %>%
      select(reader, everything())
  }

}

# Polynomial Trend-------------------------------------------
# Is the intention to do this on pooled data and/or by reader?
polynormial.regression <- function(data.in = creatinine,
                              x = "serum.crea", mref.name = "Expert Panel",
                              y = "plasma.crea", mtest.name = "Human1", sig.level=.05){


  x <- data.in %>% pull(x)
  x2 <- x^2
  x3 <- x^3
  y <- data.in %>% pull(y)

 poly.fit <- lm(y ~ x + x2 + x3)
 output1 <- tidy(summary(poly.fit))
 quad.check <- output1$p.value[3] > sig.level
 cubic.check <- output1$p.value[4] > sig.level
 linearity.check <- quad.check * cubic.check

 quad.magnitude = coefs$EST[3] > 0.8 & coefs$EST[3] < 1.25
 cubic.magnitude = coefs$EST[4] > 0.8 & coefs$EST[4] < 1.25
 magnitude.check <- quad.magnitude & cubic.magnitude


 return(list(output1, linearity.check, magnitude.check ))
}

# 2.2	Analysis Module: Repeatability and Reproducibility----------------------

 require(VCA)
 set.seed(23)
 dat.MS2 <- data.frame(  y=50 +
                           # 3 random effects for sites
                           rep(rnorm(3,0,2.5), rep(30, 3)) +
                           # 15 random effects for days
                           rep(rnorm(15,0, 2), rep(6, 15)) +
                           # 30 random effects for runs
                           rep(rnorm(30,0, 1), rep(3, 30)) +
                           # residual error (repeatability)
                           rnorm(90,0,1.5),
                         site = gl(3, 30, labels=paste0("Site_", 1:3)),
                         day = gl(5, 6, 90),
                         run =gl(2, 3, 90)
 )

 fit.MS2 <- fitVCA(y~(site+day)/run, dat.MS2)

 print(fit.MS2, digits=4)
 # •	Variability plot. The Y-axis is the MARCS score, the X-axis represents design factors: subject, test-retest, rater, replicates.
 varPlot(y~(site+day)/run, dat.MS2)

 # •	Scatter plot of variation versus mean. Y-axis is the SD of the MARCS score for each subject, X-axis is the Mean of the MARCS score for the corresponding subject.
 dat.MS2 %>% group_by(day) %>%
   mutate(mean.y = mean(y),
          sd.y = sd(y)) %>%
   ggplot(aes(x=mean.y, y= sd.y)) + geom_point()

 # •	Histograms & summary statistics. The distribution of the SD and the Mean of the MARCS score per subject will be illustrated by histogram, summary statistics (mean, CI) will be calculated.

 dat.MS2 %>% group_by(day) %>%
   mutate(mean.y = mean(y),
          sd.y = sd(y)) %>%
   ggplot(aes(x=mean.y)) + geom_histogram()

 dat.MS2 %>% group_by(day) %>%
   mutate(mean.y = mean(y),
          sd.y = sd(y)) %>%
   ggplot(aes(x=sd.y)) + geom_histogram()

 dat.MS2 %>% group_by(day) %>%
   summarize(mean.MARCS = mean(y),
             sd.MARCS = sd(y),
             n=n()) %>%
   mutate(LCI.mean = mean.MARCS - 1.96*sd.MARCS/sqrt(n),
          UCI.mean = mean.MARCS + 1.96*sd.MARCS/sqrt(n))


## 2.2.1	Minimum Detectable Difference (MDD)
# MDD is determined as a pooled variance, including both
 # test-retest and
 # re-read conditions.

get.MDD <- function(data.in1 = TEST.Retest, data.in2 = REREAD){
  sd.1 <- sd(data.in1$MARCS, na.rm = TRUE)
  sd.2 <- sd(data.in2$MARCS, na.rm = TRUE)
  n1 <- sum(!is.na(data.in1$MARCS))
  n2 <- sum(!is.na(data.in2$MARCS))
  pooled.sd <- sqrt((sd.1^2*(n1-1) + sd.2^2*(n2-1))/(n1 + n2 - 2))
  return(pooled.sd)
}

## 3.1	Analysis Module: Grounding the Underlying Categoric CE-VAST Score

data.in <- data.frame(CE-VAST = sample(c(0,1,2,3), size = 80, replace=TRUE),
                      MARSH = sample(c("0", "1", "2", "3a", "3b", "3c"), size = 80, replace=TRUE))
anchor <- function(data.in = data.in){
  # convert MARSH to ordinal
  data.in %>% mutate(MARSH.ord = case_when(
    MARSH %in% c("0", "1", "2") ~ 0,
    MARSH == "3a" ~ 1,
    MARSH == "3b" ~ 2,
    MARSH == "3c" ~ 3
  )) %>%
    summarize(mean.CE-VAST = mean(CE-VAST, na.rm=TRUE),
              sd.CE-VAST = sd(CE-VAST, na.rm=TRUE),
              mean.MARSH = mean(MARSH.ord, na.rm=TRUE),
              sd.MARSH = sd(MARSH.ord, na.rm = TRUE))

}


# 3.2	Analysis Module: Associations Relative to Accepted Clinical Endpoints
## This will be repeated on whole data + good agreemetn, poor agreement and Moderate agreement sets

# Create a dummy dataset
# GOLD = Vh:Cd (histology), CDSD score (symptomatology) or other continuous-scale measures such as IEL
# Exhaustive list needed
my.df <- data.frame(SUBJID = 1:80, MARCS = rnorm(80,0,1), GOLD = rnorm(80,0,1) )
my.df2 <- data.frame(SUBJID = 1:80, MARCS = rnorm(80,0,1), GOLD = sample(c("0", "1", "2", "3a","3b", "3c"), size=80, replace=TRUE))
diag.accuracy.continuous <- function(data.in = my.df){
# Compute the summands
phi <- c()
for(i in 1:80){
  for(j in 1:80){
    # 1 if GOLD_i > GOLD_j AND MARCS_i > MARCS_j
    # 0.5 if GOLD_i == GOLD_j OR MARCS_i == MARCS_j
    # 0 otherwise
    temp = ifelse(data.in$GOLD[i] > data.in$GOLD[j] & data.in$MARCS[i] > data.in$MARCS[j], 1,
                  ifelse(data.in$GOLD[i] == data.in$GOLD[j] | data.in$MARCS[i] == data.in$MARCS[j], .5, 0))
    phi <- c(phi, temp)
  }
}

# Estimator of diagnostic test accuracy
theta.hat = sum(phi)/(80*(80-1))

# Summands for calculation of variance of theta.hat
holdit <- apply(X = matrix(1:80), MARGIN = 1, FUN = function(x){
  i <- x
  phi.prime <- c()
  for(j in 1:80){
    temp = ifelse(data.in$GOLD[i] > data.in$GOLD[j] & data.in$MARCS[i] > data.in$MARCS[j], 1,
                  ifelse(data.in$GOLD[i] == data.in$GOLD[j] | data.in$MARCS[i] == data.in$MARCS[j], .5, 0))
    phi.prime <- c(phi.prime, temp)
  }
  phi.prime
})

# Adjustment since summands are over i != j:
diag(holdit) <- 0
# Variance of theta.hat
var.theta.hat <- 1/(80/2*(80/2 - 1))*sum(colSums(holdit)/79 - theta.hat)^2
sqrt(var.theta.hat)

# Confidence intervals

data.frame(theta.hat = theta.hat, LCI = theta.hat - 1.96*sqrt(var.theta.hat), UCI = theta.hat + 1.96*sqrt(var.theta.hat))

}


## Diagnosis Accuracy for Ordinal-Scale Gold Standard

# Alternative estimator
# Question: do we use the original MARSH or the collapsed MARSH?
# Gold standard outcomes take values 0, 1, 2, 3a, 3b, 3c


my.df2


# Estimator of diagnostic test accuracy
diag.accuracy.ordinal <- function(data.in = my.df2)

my.df2$GOLD <- factor(my.df2$GOLD)
my.df2$GOLD.ord <- as.numeric(my.df2$GOLD)

phi <- c()
for(i in 1:80){
  for(j in 1:80){
    # 1 if GOLD_i > GOLD_j AND MARCS_i > MARCS_j
    # 0.5 if GOLD_i == GOLD_j OR MARCS_i == MARCS_j
    # 0 otherwise
    temp = ifelse(my.df$GOLD[i] > my.df$GOLD[j] & my.df$MARCS[i] > my.df$MARCS[j], 1,
                  ifelse(my.df$GOLD[i] == my.df$GOLD[j] | my.df$MARCS[i] == my.df$MARCS[j], .5, 0))
    phi <- c(phi, temp)
  }
}




build.it.a <- expand.grid(SUBJID1 = 1:80, SUBJID2 = 1:80)
build.it.b <- data.frame(SUBJID1 = 1:80, GS1 = my.df2$GOLD.ord)
build.it.c <- data.frame(SUBJID2 = 1:80, GS2 = my.df2$GOLD.ord)
build <- left_join(left_join(build.it.a, build.it.b), build.it.c)

# 36 rows result
step1 <- build %>% mutate(summand = case_when(
  GS1 > GS2 ~ 1,
  GS1 == GS2 ~ 0.5,
  GS1 < GS2 ~ 0)) %>%
  group_by(GS1, GS2) %>%
  mutate(summand = ifelse(SUBJID1 == SUBJID2, 0, summand)) %>%

  summarize(theta.hat = sum(summand)*1/(n()*(n()-1))) %>%
  mutate(Loss = case_when(
    GS1 == GS2 ~ 0,
    abs(GS1 - GS2) == 1 ~ .2,
    abs(GS1 - GS2) == 2 ~ .4,
    abs(GS1 - GS2) == 3 ~ .6,
    abs(GS1 - GS2) == 4 ~ .8,
    abs(GS1 - GS2) == 5 ~ 1,
  ))


step2 <- step1 %>% left_join(
build %>% mutate(summand = case_when(
       GS1 > GS2 ~ 1,
      GS1 == GS2 ~ 0.5,
       GS1 < GS2 ~ 0)) %>% group_by(GS1) %>% tally(name="n_t")) %>%
  left_join(
    build %>% mutate(summand = case_when(
      GS1 > GS2 ~ 1,
      GS1 == GS2 ~ 0.5,
      GS1 < GS2 ~ 0)) %>% group_by(GS2) %>% tally(name="n_s")) %>%
  ungroup()

weight.scale <- step2 %>% filter(GS2 > GS1) %>% mutate(product = n_t*n_s) %>%
  summarize(denominator = sum(product)) %>% pull(denominator)

step3 <- step2 %>% mutate(weight = n_t*n_s/weight.scale)

step4 <- step3 %>% summarize(theta.prime = 1 - sum(weight*Loss*(1 - theta.hat)))


## Seemingly unrelated regression

install.packages("nnet")
install.packages("mgcv")
install.packages("quantreg")
install.packages("systemfit")
install.packages("foreign")
install.packages("car")
install.packages("Rcpp")
library(foreign)
library(systemfit)

hsb2 <- read.dta("https://stats.idre.ucla.edu/stat/stata/notes/hsb2.dta")

r1 <- read~female + as.numeric(ses) + socst
r2 <- math~female + as.numeric(ses) + science

fitsur <- systemfit(list(readreg = r1, mathreg = r2), data=hsb2)
summary(fitsur)

# We may be interested in comparing the effect of female on read, controlling for ses and socst, to the effect of female on math, controlling for ses and science. For this, we will use the linear.hypothesis command from the car package. To do this, we create a “restriction” on the model system. We will force the coefficient of female to be the same in both equations and then compare such a system fit to the one seen when the coefficients are not equal.

library(car)
restriction <- "readreg_femalefemale- mathreg_femalefemale"
linearHypothesis(fitsur, restriction, test = "Chisq")










