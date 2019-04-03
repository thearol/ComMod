### Chapter 6

# Loading and options
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(rethinking)
library(tidyverse)

library(ggplot2)

###
sppnames <- c( "afarensis","africanus","habilis","boisei",
               "rudolfensis","ergaster","sapiens")
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc , mass=masskg )

m6.1 <- lm( brain ~ mass , data=d )

1 - var(resid(m6.1))/var(d$brain)

summary(m6.1)

m6.2 <- lm( brain ~ mass + I(mass^2) , data=d )
m6.3 <- lm( brain ~ mass + I(mass^2) + I(mass^3) , data=d )
m6.4 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) ,
            data=d )
m6.5 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) +
              I(mass^5) , data=d )
m6.6 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) +
              I(mass^5) + I(mass^6) , data=d )

m6.7 <- lm( brain ~ 1 , data=d )

p <- c( 0.5 , 0.5 )
-sum( p*log(p) )

# fit model with lm
m6.1 <- lm( brain ~ mass , d )
# compute deviance by cheating
(-2) * logLik(m6.1)

## for running simulations
N <- 20
kseq <- 1:5
dev <- sapply( kseq , function(k) {
  print(k);
  r <- replicate( 1e4 , sim.train.test( N=N, k=k ) );
  c( mean(r[1,]) , mean(r[2,]) , sd(r[1,]) , sd(r[2,]) )
} )

plot( 1:5 , dev[1,] , ylim=c( min(dev[1:2,])-5 , max(dev[1:2,])+10 ) ,
      xlim=c(1,5.1) , xlab="number of parameters" , ylab="deviance" ,
      pch=16 , col=rangi2 )
mtext( concat( "N = ",N ) )
points( (1:5)+0.1 , dev[2,] )
for ( i in kseq ) {
  pts_in <- dev[1,i] + c(-1,+1)*dev[3,i]
  pts_out <- dev[2,i] + c(-1,+1)*dev[4,i]
  lines( c(i,i) , pts_in , col=rangi2 )
  lines( c(i,i)+0.1 , pts_out )
}

###  Questions
data(Howell1)
d <- Howell1
d$age <- (d$age - mean(d$age))/sd(d$age)
set.seed( 1000 )
i <- sample(1:nrow(d),size=nrow(d)/2)
d1 <- d[ i , ]
d2 <- d[ -i , ]

e6.1 <- rethinking::map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bA * age ,
    a ~ dnorm( 138 , 50 ) ,
    bA ~ dnorm( 0 , 10 ) ,
    sigma ~ dnorm( 0 , 10 )
  ) , data = d1 )

e6.2 <- rethinking::map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bA * age + bA2 * age^2,
    a ~ dnorm( 138 , 50 ) ,
    bA ~ dnorm( 0 , 10 ) ,
    bA2 ~ dnorm( 0 , 10 ) ,
    sigma ~ dnorm( 0 , 10 )
  ) , data = d1 )

e6.3 <- rethinking::map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bA * age + bA2 * age^2 + bA3 * age^3,
    a ~ dnorm( 138 , 50 ) ,
    bA ~ dnorm( 0 , 10 ) ,
    bA2 ~ dnorm( 0 , 10 ) ,
    bA3 ~ dnorm( 0 , 10 ) ,
    sigma ~ dnorm( 0 , 10 )
  ) , data = d1 )

e6.4 <- rethinking::map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bA * age + bA2 * age^2 + bA3 * age^3 + bA4 * age^4,
    a ~ dnorm( 138 , 50 ) ,
    bA ~ dnorm( 0 , 10 ) ,
    bA2 ~ dnorm( 0 , 10 ) ,
    bA3 ~ dnorm( 0 , 10 ) ,
    bA4 ~ dnorm( 0 , 10 ) ,
    sigma ~ dnorm( 0 , 10 )
  ) , data = d1 )

e6.5 <- rethinking::map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bA * age + bA2 * age^2 + bA3 * age^3 + bA4* age^4 + bA5 * age^5,
    a ~ dnorm( 138 , 50 ) ,
    bA ~ dnorm( 0 , 10 ) ,
    bA2 ~ dnorm( 0 , 10 ) ,
    bA3 ~ dnorm( 0 , 10 ) ,
    bA4 ~ dnorm( 0 , 10 ) ,
    bA5 ~ dnorm( 0 , 10 ) ,
    sigma ~ dnorm( 0 , 10 )
  ) , data = d1 )

e6.6<- rethinking::map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bA * age + bA2 * age^2 + bA3 * age^3 + bA4* age^4 + bA5 * age^5 + bA6 * age^6,
    a ~ dnorm( 138 , 50 ) ,
    bA ~ dnorm( 0 , 10 ) ,
    bA2 ~ dnorm( 0 , 10 ) ,
    bA3 ~ dnorm( 0 , 10 ) ,
    bA4 ~ dnorm( 0 , 10 ) ,
    bA5 ~ dnorm( 0 , 10 ) ,
    bA6 ~ dnorm( 0 , 10 ) ,
    sigma ~ dnorm( 0 , 10 )
  ) , data = d1 )

#6H1
compare(e6.1,e6.2,e6.3,e6.4,e6.5,e6.6)
#6H2
# Model 1
age.seq <- seq( from=-2 , to=3 , by=0.01 )
mu <- link( e6.1 , data=data.frame(age=age.seq) )
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.97 )

sim.height <- sim( e6.1 , data=list(age=age.seq) )

height.PI <- apply( sim.height , 2 , PI , prob=0.97 )

# plot raw data
plot( height ~ age , d1 , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( age.seq , mu.mean )
# draw HPDI region for line
shade( mu.HPDI , age.seq )
# draw PI region for simulated heights
shade( height.PI , age.seq )

# Model 2
age.seq <- seq( from=-2 , to=3 , by=0.01 )
mu <- link( e6.2 , data=data.frame(age=age.seq) )
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.97 )

sim.height <- sim( e6.2 , data=list(age=age.seq) )

height.PI <- apply( sim.height , 2 , PI , prob=0.97 )

# plot raw data
plot( height ~ age , d1 , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( age.seq , mu.mean )
# draw HPDI region for line
shade( mu.HPDI , age.seq )
# draw PI region for simulated heights
shade( height.PI , age.seq )

# Model 3
age.seq <- seq( from=-2 , to=3 , by=0.01 )
mu <- link( e6.3 , data=data.frame(age=age.seq) )
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.97 )

sim.height <- sim( e6.3 , data=list(age=age.seq) )

height.PI <- apply( sim.height , 2 , PI , prob=0.97 )

# plot raw data
plot( height ~ age , d1 , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( age.seq , mu.mean )
# draw HPDI region for line
shade( mu.HPDI , age.seq )
# draw PI region for simulated heights
shade( height.PI , age.seq )

# Model 4
age.seq <- seq( from=-2 , to=3 , by=0.01 )
mu <- link( e6.4 , data=data.frame(age=age.seq) )
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.97 )

sim.height <- sim( e6.4 , data=list(age=age.seq) )

height.PI <- apply( sim.height , 2 , PI , prob=0.97 )

# plot raw data
plot( height ~ age , d1 , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( age.seq , mu.mean )
# draw HPDI region for line
shade( mu.HPDI , age.seq )
# draw PI region for simulated heights
shade( height.PI , age.seq )

# Model 5
age.seq <- seq( from=-2 , to=3 , by=0.01 )
mu <- link( e6.5 , data=data.frame(age=age.seq) )
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.97 )

sim.height <- sim( e6.5 , data=list(age=age.seq) )

height.PI <- apply( sim.height , 2 , PI , prob=0.97 )

# plot raw data
plot( height ~ age , d1 , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( age.seq , mu.mean )
# draw HPDI region for line
shade( mu.HPDI , age.seq )
# draw PI region for simulated heights
shade( height.PI , age.seq )

# Model 6
age.seq <- seq( from=-2 , to=3 , by=0.01 )
mu <- link( e6.6 , data=data.frame(age=age.seq) )
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.97 )

sim.height <- sim( e6.6 , data=list(age=age.seq) )

height.PI <- apply( sim.height , 2 , PI , prob=0.97 )

# plot raw data
plot( height ~ age , d1 , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( age.seq , mu.mean )
# draw HPDI region for line
shade( mu.HPDI , age.seq )
# draw PI region for simulated heights
shade( height.PI , age.seq )

#6H3
# compute counterfactual predictions
# neocortex from 0.5 to 0.8
age.seq <- seq( from=-2 , to=3 , length.out=30 )
d.predict <- list(
  height = rep(0,30), # empty outcome
  age = age.seq # sequence of neocortex
)
pred.e6.4 <- link( e6.4 , data=d.predict )
mu <- apply( pred.e6.4 , 2 , mean )
mu.PI <- apply( pred.e6.4 , 2 , PI )
# plot it all
plot( height ~ age , d1 , col=col.alpha(rangi2,0.5) )
lines( age.seq , mu , lty=2 )
lines( age.seq , mu.PI[1,] , lty=2 )
lines( age.seq , mu.PI[2,] , lty=2 )

milk.ensemble <- ensemble(e6.1 , e6.2  , e6.3 , e6.4 ,e6.5 ,e6.6  , data=d.predict )
mu <- apply( milk.ensemble$link , 2 , mean )
mu.PI <- apply( milk.ensemble$link , 2 , PI )
lines( age.seq , mu )
shade( mu.PI , age.seq )

#6H4
theta <- coef(e6.1)

dev1 <- (-2)*sum( dnorm(
  d2$height ,
  mean=theta[1]+theta[2]*d2$age ,
  sd=theta[3] ,
  log=TRUE ) )

theta <- coef(e6.2)

dev2 <- (-2)*sum( dnorm(
  d2$height ,
  mean=theta[1]+theta[2]*d2$age+theta[3]*d2$age^2 ,
  sd=theta[4] ,
  log=TRUE ) )

theta <- coef(e6.3)

dev3 <- (-2)*sum( dnorm(
  d2$height ,
  mean=theta[1]+theta[2]*d2$age+theta[3]*d2$age^2+theta[4]*d2$age^3 ,
  sd=theta[5] ,
  log=TRUE ) )

theta <- coef(e6.4)

dev4 <- (-2)*sum( dnorm(
  d2$height ,
  mean=theta[1]+theta[2]*d2$age+theta[3]*d2$age^2+theta[4]*d2$age^3+theta[5]*d2$age^4 ,
  sd=theta[6] ,
  log=TRUE ) )

theta <- coef(e6.5)

dev5 <- (-2)*sum( dnorm(
  d2$height ,
  mean=theta[1]+theta[2]*d2$age+theta[3]*d2$age^2+theta[4]*d2$age^3+theta[5]*d2$age^4+theta[6]*d2$age^5 ,
  sd=theta[7] ,
  log=TRUE ) )

theta <- coef(e6.6)

dev6 <- (-2)*sum( dnorm(
  d2$height ,
  mean=theta[1]+theta[2]*d2$age+theta[3]*d2$age^2+theta[4]*d2$age^3+theta[5]*d2$age^4+theta[6]*d2$age^5+theta[7]*d2$age^6 ,
  sd=theta[8] ,
  log=TRUE ) )

dev1
dev2
dev3
dev4
dev5
dev6

compare(e6.1,e6.2,e6.3,e6.4,e6.5,e6.6)

#6H5
#6H6
e6.7<- rethinking::map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bA * age + bA2 * age^2 + bA3 * age^3 + bA4* age^4 + bA5 * age^5 + bA6 * age^6,
    a ~ dnorm( 0 , 100 ),
    bA ~ dnorm( 0 , 5 ) ,
    bA2 ~ dnorm( 0 , 5 ) ,
    bA3 ~ dnorm( 0 , 5 ) ,
    bA4 ~ dnorm( 0 , 5 ) ,
    bA5 ~ dnorm( 0 , 5 ) ,
    bA6 ~ dnorm( 0 , 5 ) ,
    sigma ~ dnorm( 0 , 100 )
  ) , data = d1 )

#plot
age.seq <- seq( from=-2 , to=3 , by=0.01 )
mu <- link( e6.7 , data=data.frame(age=age.seq) )
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.97 )

sim.height <- sim( e6.7 , data=list(age=age.seq) )

height.PI <- apply( sim.height , 2 , PI , prob=0.97 )

# plot raw data
plot( height ~ age , d1 , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( age.seq , mu.mean )
# draw HPDI region for line
shade( mu.HPDI , age.seq )
# draw PI region for simulated heights
shade( height.PI , age.seq )

# out of sample
theta <- coef(e6.7)

dev7 <- (-2)*sum( dnorm(
  d2$height ,
  mean=theta[1]+theta[2]*d2$age+theta[3]*d2$age^2+theta[4]*d2$age^3+theta[5]*d2$age^4+theta[6]*d2$age^5+theta[7]*d2$age^6 ,
  sd=theta[8] ,
  log=TRUE ) )

dev7

precis(e6.6)
compare(e6.1,e6.2,e6.3,e6.4,e6.5,e6.6)

dev4
dev5
dev6