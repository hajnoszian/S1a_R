library(pwr2ppl)
library(tidyverse)
library(Hmisc)
df_cor <- df %>% 
  select(KM_P, KM_CSR, KM_M, KM_PA, KM_L, pKM, RNS1, RNS2, RNF1, RNF2)
rcorr(as.matrix(df_cor))


###Power for final indirect effect mediation
#RNS

df_cor <- df %>% 
  select(pKM_Z, RNS1_Z, RNS2_Z)
df_cor$Cond <-as.numeric(df$Cond)-1 #Condition needs to be numeric for correlation table

rcorr(as.matrix(df_cor))

# (1)pKM + (2)Cond + (3)RNS1 -> (y)RNS2
MRC_all(
  ry1 = 0.25,
  ry2 = 0.12,
  ry3 = 0.76,
  r12 = 0.71,
  r13 = 0.14,
  r23 = 0.01,
  n = 293, alpha = .05, rep = 10000
  ) #gives individual power for each effect, as well as power for all in concert
#e.g. type II error rate for b2 is ~94% (94% of the time we would fail to declare an effect as sig if the alternative h is true)
#I.e. this is way higher than the 20% we usually want (i.e. 1-.80=.20)

#pKM power = 70%, means 30% of the time we would fail to declare an effect as sig if pKM is actually doing something
#i.e. we want more power here. Our finding effects may be precise and accurate to sample, but somewhat riskier than ideal (20%) to generalize
#For the purposes of b2, that makes complete sense power is terrible (the effect is near zero)


#RNF
df_cor <- df %>% 
  select(pKM_Z, RNF1_Z, RNF2_Z)
df_cor$Cond <-as.numeric(df$Cond)-1 #Condition needs to be numeric for correlation table

rcorr(as.matrix(df_cor))

# (1)pKM + (2)Cond + (3)RNF1 -> (y)RNF2
MRC_all(
  ry1 = -0.09,
  ry2 = -0.08,
  ry3 = 0.88,
  r12 = 0.71,
  r13 = -0.02,
  r23 = -0.03,
  n = 293, alpha = .05, rep = 10000
) #gives individual power for each effect, as well as power for all in concert

