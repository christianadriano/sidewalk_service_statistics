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



