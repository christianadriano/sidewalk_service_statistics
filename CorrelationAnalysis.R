"
Analysis of sidewalk tickets by subprefecture and their population and IDHM

Tables for correlation strength
https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6107969/
"

path <- "C://Users//Christian//Documents//GitHub//sidewalk_service_statistics//"
dataset <- read.csv(paste0(path, "Sub_IDHM_Population_Sidewalks.csv"))
df <- data.frame(dataset)
summary(df)

shapiro.test(df$Population) #W = 0.95422, p-value = 0.1898
shapiro.test(df$IDHM) #W = 0.58862, p-value = 2.708e-08 NOT NORMAL
shapiro.test(df$Sidewalk_Tickets) #W = 0.91577, p-value = 0.01599 NOT NORMAL



cor.test(x=df$Population,y=df$IDHM,method="kendall")
"RESULT:
 NOT CORRELATED
"
# data:  df$Population and df$IDHM
# z = 1.5285, p-value = 0.1264
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
#       tau 
# 0.1930514 

cor.test(x=df$Population,y=df$Sidewalk_Tickets,method="kendall")
"RESULT:
 Statistical significant moderate to strong correlation, tau=0.403 
"
# data:  df$Population and df$Sidewalk_Tickets
# T = 348, p-value = 0.0009606
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
#   tau 
# 0.4032258 

cor.test(x=df$IDHM,y=df$Sidewalk_Tickets,method="kendall")
"RESULT:
 Statistical significant strong correlation, tau=0.61 
"
# data:  df$IDHM and df$Sidewalk_Tickets
# z = 4.8782, p-value = 1.071e-06
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
#   tau 
# 0.6161216

