" 
Does IDHM and Population are causally associated with number of sidewalk tickets?

"

library(rethinking)
library(stringr)
library(dplyr)



path <- "C://Users//Christian//Documents//GitHub//sidewalk_service_statistics//"
dataset <- read.csv(paste0(path, "Sub_IDHM_Population_Sidewalks.csv"))
df <- data.frame(dataset)
summary(df)
df <- df[df$IDHM_Income!=0,]

"
------------------------------------------------
Used this for sensitivity analysis by removing
removing the two largest tickets. Nothing changed.

max_sidewalk_tickets <- max(df$Sidewalk_Tickets)
df <- df[df$Sidewalk_Tickets!=max_sidewalk_tickets,] #Removed S� subprefecture
max_sidewalk_tickets <- max(df$Sidewalk_Tickets)
df <- df[df$Sidewalk_Tickets!=max_sidewalk_tickets,] #Removed Pinheiros subprefecture
-----------------------------------------------
"

df$Population <- scale(df$Population)
df$IDHM <- scale(df$IDHM)
df$Sidewalk_Tickets <- scale(df$Sidewalk_Tickets)


m1.IDHM <- quap(
  alist(
    Sidewalk_Tickets ~ dnorm( mu , sigma ) ,
    mu <- a + bi*IDHM,
    bi ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.IDHM)
#       mean   sd  5.5% 94.5%
# bi    0.76 0.11  0.58  0.94
# a     0.00 0.11 -0.18  0.18
# sigma 0.62 0.08  0.49  0.74

m1.IDHM_Income <- quap(
  alist(
    Sidewalk_Tickets ~ dnorm( mu , sigma ) ,
    mu <- a + bi*IDHM_Income,
    bi ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.IDHM_Income)
#        mean   sd  5.5% 94.5%
# bi     1.62 0.78  0.38  2.87
# a     -1.27 0.63 -2.28 -0.26
# sigma  0.85 0.12  0.66  1.04

m1.Population <- quap(
  alist(
    Sidewalk_Tickets ~ dnorm( mu , sigma ) ,
    mu <- a + bp*Population,
    bp ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.Population)
#       mean   sd  5.5% 94.5%
# bp    0.44 0.16  0.19  0.69
# a     0.00 0.15 -0.25  0.25
# sigma 0.86 0.11  0.69  1.04

m1.IDHM.Population <- quap(
  alist(
    Sidewalk_Tickets ~ dnorm( mu , sigma ) ,
    mu <- a + bp*Population + bi*IDHM,
    bp ~ dnorm( 0 , 1 ) ,
    bi ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.IDHM.Population)
#       mean   sd  5.5% 94.5%
# bp    0.32 0.10  0.16  0.48
# bi    0.71 0.10  0.55  0.87
# a     0.00 0.10 -0.15  0.15
# sigma 0.54 0.07  0.43  0.64

m1.IDHM_Income.Population <- quap(
  alist(
    Sidewalk_Tickets ~ dnorm( mu , sigma ) ,
    mu <- a + bp*Population + bi*IDHM_Income,
    bp ~ dnorm( 0 , 1 ) ,
    bi ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.IDHM_Income.Population)
#        mean   sd  5.5% 94.5%
# bp     0.41 0.13  0.19  0.62
# bi     1.78 0.77  0.55  3.02
# a     -1.41 0.63 -2.41 -0.41
# sigma  0.74 0.11  0.57  0.90

m1.IDHM.Population.Interaction <- quap(
  alist(
    Sidewalk_Tickets ~ dnorm( mu , sigma ) ,
    mu <- a + bp*Population + bi*IDHM_Income + bip*IDHM_Income*Population,
    bip ~ dnorm( 0 , 1 ) ,
    bp ~ dnorm( 0 , 1 ) ,
    bi ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.IDHM.Population.Interaction)
#        mean   sd  5.5% 94.5%
# bip    0.34 0.71 -0.79  1.46 NO
# bp     0.15 0.56 -0.74  1.04 NO
# bi     1.80 0.77  0.57  3.03
# a     -1.43 0.63 -2.43 -0.43
# sigma  0.73 0.11  0.56  0.90

rethinking::compare(m1.IDHM,m1.Population, m1.IDHM.Population,m1.IDHM_Income.Population, func=PSIS) 
#PSIS = Pareto-smoothed importance sampling
#                           PSIS    SE dPSIS   dSE pPSIS weight
# m1.IDHM.Population        68.4 24.41   0.0    NA   9.7   0.92
# m1.IDHM                   73.4 20.85   5.0  6.36   7.7   0.08
# m1.IDHM_Income.Population 82.4 19.24  13.9  9.50   6.7   0.00
# m1.Population             95.2 19.86  26.7 10.41   7.8   0.00

rethinking::compare(m1.IDHM,m1.Population, m1.IDHM.Population,m1.IDHM_Income.Population, func=WAIC) 
#                           WAIC    SE dWAIC   dSE pWAIC weight
# m1.IDHM.Population        63.8 19.92   0.0    NA   7.4   0.94
# m1.IDHM                   69.4 16.88   5.7  6.10   5.8   0.06
# m1.IDHM_Income.Population 80.4 17.04  16.6  8.41   5.7   0.00
# m1.Population             87.8 13.80  24.1 10.83   4.1   0.00

#----------------------------------------------------------
"Plot the Posterior with corresponding variance (shaded region)"

#Generate simulated input data
IDHM_seq <- seq( from=min(df$IDHM) , to=max(df$IDHM) , length.out=100 )
IDHM_Income_seq <- seq( from=min(df$IDHM_Income) , to=max(df$IDHM_Income) , length.out=100 )
Population_seq <- seq( from=min(df$Population) , to=max(df$Population) , length.out=100 )

#Plot the shade region with the variance

#sample from the posterior distribution, and then compute
#for each case in the data and sample from the posterior distribution.
mu <- link(m1.IDHM.Population, data = data.frame(Population=Population_seq,IDHM=IDHM_seq))
#Compute vectors of means
mu.mean = apply(mu,2,mean)
mu.HPDI = apply(mu,2,HPDI, prob=0.89) #mean with highest posterior density interval

#Simulates Sidewalk_Tickets by extracting from the posterior, but now also
#considers the variance
sim1.1 <- sim(m1.IDHM.Population, data=list(Population=Population_seq,IDHM=IDHM_seq)) 
Sidewalk_Tickets.PI = apply(sim1.1,2, PI, prob=0.89) #mean with the percentile intervals

plot(Sidewalk_Tickets ~ Population, df,col=col.alpha(rangi2,0.5)) #plot raw data
title(paste("m1.IDHM.Population posterior Sidewalk_Tickets ","Population"))

#plot the Map line and interval more visible
lines(Population_seq,mu.mean)

#plot the shaded region with 89% HPDI
shade(mu.HPDI,Population_seq)

#plot the shaded region with 89% PI
shade(Sidewalk_Tickets.PI,Population_seq)

#--------------------------------
plot(Sidewalk_Tickets ~ IDHM, df,col=col.alpha(rangi2,0.5)) #plot raw data
title(paste("m1.IDHM.Population posterior Sidewalk_Tickets ","IDHM"))
#plot the Map line and interval more visible
lines(IDHM_seq,mu.mean)

#plot the shaded region with 89% HPDI
shade(mu.HPDI,IDHM_seq)

#plot the shaded region with 89% PI
shade(Sidewalk_Tickets.PI,IDHM_seq)

#=============================================

#sample from the posterior distribution, and then compute
#for each case in the data and sample from the posterior distribution.
mu <- link(m1.IDHM, data = data.frame(IDHM=IDHM_seq))
#Compute vectors of means
mu.mean = apply(mu,2,mean)
mu.HPDI = apply(mu,2,HPDI, prob=0.89) #mean with highest posterior density interval

#Simulates Sidewalk_Tickets by extracting from the posterior, but now also
#considers the variance
sim2 <- sim(m1.IDHM, data=list(IDHM=IDHM_seq)) 
Sidewalk_Tickets.PI = apply(sim2,2, PI, prob=0.89) #mean with the percentile intervals

plot(Sidewalk_Tickets ~ IDHM, df,col=col.alpha(rangi2,0.5)) #plot raw data
title(paste("m1.IDHM posterior Sidewalk_Tickets ","IDHM"))

#plot the Map line and interval more visible
lines(IDHM_seq,mu.mean)

#plot the shaded region with 89% HPDI
shade(mu.HPDI,IDHM_seq)

#plot the shaded region with 89% PI
shade(Sidewalk_Tickets.PI,IDHM_seq)

#=============================================

#sample from the posterior distribution, and then compute
#for each case in the data and sample from the posterior distribution.
mu <- link(m1.IDHM_Income, data = data.frame(IDHM_Income=IDHM_Income_seq))
#Compute vectors of means
mu.mean = apply(mu,2,mean)
mu.HPDI = apply(mu,2,HPDI, prob=0.89) #mean with highest posterior density interval

#Simulates Sidewalk_Tickets by extracting from the posterior, but now also
#considers the variance
sim3 <- sim(m1.IDHM_Income, data=list(IDHM_Income=IDHM_Income_seq)) 
Sidewalk_Tickets.PI = apply(sim3,2, PI, prob=0.89) #mean with the percentile intervals

plot(Sidewalk_Tickets ~ IDHM_Income, df,col=col.alpha(rangi2,0.5)) #plot raw data
title(paste("m1.IDHM_Income posterior Sidewalk_Tickets ","IDHM_Income"))

#plot the Map line and interval more visible
lines(IDHM_Income_seq,mu.mean)

#plot the shaded region with 89% HPDI
shade(mu.HPDI,IDHM_Income_seq)

#plot the shaded region with 89% PI
shade(Sidewalk_Tickets.PI,IDHM_Income_seq)
