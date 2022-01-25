###This is a copy of the code I used to do the mediation. It's a 3-step building of the mediation via simple regressions within JAGS--not a package or shiny app to facilitate the process



####Bayesian Estimation...attempt####


##XMY Mediation: X->M 

require(rjags)
M_Model <- "model {

#likelihood
for(i in 1:n){
M[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*X[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)


tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

M_data <- list(
  n = nrow(df),
  M = df$pKM_Z[,1],
  X = df$Cond,
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

M_inits <- list(
  list(beta0 = 0, beta1 = 2),
  list(beta0 = 10, beta1 = 0.01),
  list(beta0 = 5, beta1 = 0.5)
)

M_Model <- jags.model(file = textConnection(M_Model),
                      data = M_data,
                      inits = M_inits,
                      n.chains = 3,
                      quiet = TRUE)
update(M_Model, n.iter = 5000)
M_Model_res <- coda.samples(M_Model,
                            variable.names = c("beta0", "beta1", "sigma2"),
                            n.iter = 10000)
plot(M_Model_res) #Trace plots and parameter density

gelman.plot(M_Model_res) #BGR statistic, want closer to 1

effectiveSize(M_Model_res[[1]][,"beta0"])
effectiveSize(M_Model_res[[1]][,"beta1"])

autocorr.plot(M_Model_res[[1]][,"beta0"], main = "Intercept")
autocorr.plot(M_Model_res[[1]][,"beta1"], main = "Slope") #

summary(M_Model_res) #want MC error (Time-series SE) to be lower than 1/20th size of parameter SDs

##X-> Y_RNS
require(rjags)
RNS_Model <- "model {

#likelihood
for(i in 1:n){
Y[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*X[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)


tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

RNS_data <- list(
  n = nrow(df),
  Y = df$RNS2_Z[,1],
  X = df$Cond,
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

RNS_inits <- list(
  list(beta0 = 0, beta1 = 2),
  list(beta0 = 10, beta1 = 0.01),
  list(beta0 = 5, beta1 = 0.5)
)

RNS_Model <- jags.model(file = textConnection(RNS_Model),
                        data = RNS_data,
                        inits = RNS_inits,
                        n.chains = 3,
                        quiet = TRUE)
update(RNS_Model, n.iter = 5000)
RNS_Model_res <- coda.samples(RNS_Model,
                              variable.names = c("beta0", "beta1", "sigma2"),
                              n.iter = 10000)
plot(RNS_Model_res) #Trace plots and parameter density

gelman.plot(RNS_Model_res) #BGR statistic, want closer to 1

effectiveSize(RNS_Model_res[[1]][,"beta0"])
effectiveSize(RNS_Model_res[[1]][,"beta1"])

autocorr.plot(RNS_Model_res[[1]][,"beta0"], main = "Intercept")
autocorr.plot(RNS_Model_res[[1]][,"beta1"], main = "Slope") #

summary(RNS_Model_res)

##X-> Y_RNF
require(rjags)
RNF_Model <- "model {

#likelihood
for(i in 1:n){
Y[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*X[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)


tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

RNF_data <- list(
  n = nrow(df),
  Y = df$RNF2_Z[,1],
  X = df$Cond,
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

RNF_inits <- list(
  list(beta0 = 0, beta1 = 2),
  list(beta0 = 10, beta1 = 0.01),
  list(beta0 = 5, beta1 = 0.5)
)

RNF_Model <- jags.model(file = textConnection(RNF_Model),
                        data = RNF_data,
                        inits = RNF_inits,
                        n.chains = 3,
                        quiet = TRUE)
update(RNF_Model, n.iter = 5000)
RNF_Model_res <- coda.samples(RNF_Model,
                              variable.names = c("beta0", "beta1", "sigma2"),
                              n.iter = 10000)
plot(RNF_Model_res) #Trace plots and parameter density

gelman.plot(RNF_Model_res) #BGR statistic, want closer to 1

effectiveSize(RNF_Model_res[[1]][,"beta0"])
effectiveSize(RNF_Model_res[[1]][,"beta1"])

autocorr.plot(RNF_Model_res[[1]][,"beta0"], main = "Intercept")
autocorr.plot(RNF_Model_res[[1]][,"beta1"], main = "Slope") #

summary(RNF_Model_res)

##X + Cov -> Y_RNS
require(rjags)
RNS_C_Model <- "model {

#likelihood
for(i in 1:n){
Y[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*X[i] + beta2*C[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)
beta2 ~ dnorm(mu0, tau0)

tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

RNS_data <- list(
  n = nrow(df),
  Y = df$RNS2_Z[,1],
  X = df$Cond,
  C = df$RNS1_Z[,1],
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

RNS_inits <- list(
  list(beta0 = 0, beta1 = 2, beta2 = 5),
  list(beta0 = 10, beta1 = 0.01, beta2 = 0),
  list(beta0 = 5, beta1 = 0.5, beta2 = 0.01)
)

RNS_C_Model <- jags.model(file = textConnection(RNS_C_Model),
                          data = RNS_data,
                          inits = RNS_inits,
                          n.chains = 3,
                          quiet = TRUE)
update(RNS_C_Model, n.iter = 5000)
RNS_C_Model_res <- coda.samples(RNS_C_Model,
                                variable.names = c("beta0", "beta1", "beta2", "sigma2"),
                                n.iter = 10000)
plot(RNS_C_Model_res) #Trace plots and parameter density

gelman.plot(RNS_C_Model_res) #BGR statistic, want closer to 1

effectiveSize(RNS_C_Model_res[[1]][,"beta0"])
effectiveSize(RNS_C_Model_res[[1]][,"beta1"])

autocorr.plot(RNS_C_Model_res[[1]][,"beta0"], main = "Intercept")
autocorr.plot(RNS_C_Model_res[[1]][,"beta1"], main = "Slope") #

summary(RNS_C_Model_res)

##X + Cov -> Y_RNF
RNF_C_Model <- "model {

#likelihood
for(i in 1:n){
Y[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*X[i] + beta2*C[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)
beta2 ~ dnorm(mu0, tau0)

tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

RNF_data <- list(
  n = nrow(df),
  Y = df$RNF2_Z[,1],
  X = df$Cond,
  C = df$RNF1_Z[,1],
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

RNF_inits <- list(
  list(beta0 = 0, beta1 = 2, beta2 = 5),
  list(beta0 = 10, beta1 = 0.01, beta2 = 0),
  list(beta0 = 5, beta1 = 0.5, beta2 = 0.01)
)

RNF_C_Model <- jags.model(file = textConnection(RNF_C_Model),
                          data = RNF_data,
                          inits = RNF_inits,
                          n.chains = 3,
                          quiet = TRUE)
update(RNF_C_Model, n.iter = 5000)
RNF_C_Model_res <- coda.samples(RNF_C_Model,
                                variable.names = c("beta0", "beta1", "beta2", "sigma2"),
                                n.iter = 10000)
plot(RNF_C_Model_res) #Trace plots and parameter density

gelman.plot(RNF_C_Model_res) #BGR statistic, want closer to 1

effectiveSize(RNF_C_Model_res[[1]][,"beta0"])
effectiveSize(RNF_C_Model_res[[1]][,"beta1"])

autocorr.plot(RNF_C_Model_res[[1]][,"beta0"], main = "Intercept")
autocorr.plot(RNF_C_Model_res[[1]][,"beta1"], main = "Slope") #

summary(RNF_C_Model_res)


#====RNS
##XMY Mediation: XM -> Y

require(rjags)
RNS_Y_Model <- "model {

#likelihood
for(i in 1:n){
Y[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*M[i] + beta2*X[i] + beta3*C[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)
beta2 ~ dnorm(mu0, tau0)
beta3 ~ dnorm(mu0, tau0)

tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

RNS_Y_data <- list(
  n = nrow(df),
  Y = df$RNS2_Z[,1],
  M = df$pKM_Z[,1],
  X = df$Cond,
  C = df$RNS1_Z[,1],
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

RNS_Y_inits <- list(
  list(beta0 = 0, beta1 = 2, beta2 = 5, beta3 = 10),
  list(beta0 = 10, beta1 = 0.01, beta2 = 0, beta3 = 0.0001),
  list(beta0 = 5, beta1 = 0.5, beta2 = 0.01, beta3 = 0)
)

RNS_Y_Model <- jags.model(file = textConnection(RNS_Y_Model),
                          data = RNS_Y_data,
                          inits = RNS_Y_inits,
                          n.chains = 3,
                          quiet = TRUE)
update(RNS_Y_Model, n.iter = 5000)
RNS_Y_Model_res <- coda.samples(RNS_Y_Model,
                                variable.names = c("beta0", "beta1", "beta2", "beta3", "sigma2"),
                                n.iter = 100000) 
plot(RNS_Y_Model_res) #Trace plots and parameter densities

gelman.plot(RNS_Y_Model_res) #BGR statistic, quant measure of chain mixing, very close to 1

effectiveSize(RNS_Y_Model_res[[1]][,"beta0"]) #measure of how many 'effective' independent samples we got
effectiveSize(RNS_Y_Model_res[[1]][,"beta1"])
effectiveSize(RNS_Y_Model_res[[1]][,"beta2"])
effectiveSize(RNS_Y_Model_res[[1]][,"beta3"])

autocorr.plot(RNS_Y_Model_res[[1]][,"beta0"], main = "Intercept") #measure of whether we have independent sampling yet
autocorr.plot(RNS_Y_Model_res[[1]][,"beta1"], main = "pKM Slope") #
autocorr.plot(RNS_Y_Model_res[[1]][,"beta2"], main = "Cond Slope") #
autocorr.plot(RNS_Y_Model_res[[1]][,"beta3"], main = "T1 Slope") #



summary(RNS_Y_Model_res) #remember; beta0 = intercept (reflects neutral cond base), beta1 = pKM effect, beta2 = Cond effect, beta3 = T1 Covariate effect


#===RNF
##XMY Mediation: XM -> Y

require(rjags)
RNF_Y_Model <- "model {

#likelihood
for(i in 1:n){
Y[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*M[i] + beta2*X[i] + beta3*C[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)
beta2 ~ dnorm(mu0, tau0)
beta3 ~ dnorm(mu0, tau0)

tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

RNF_Y_data <- list(
  n = nrow(df),
  Y = df$RNF2_Z[,1],
  M = df$pKM_Z[,1],
  X = df$Cond,
  C = df$RNF1_Z[,1],
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

RNF_Y_inits <- list(
  list(beta0 = 0, beta1 = 2, beta2 = 5, beta3 = 10),
  list(beta0 = 10, beta1 = 0.01, beta2 = 0, beta3 = 0.0001),
  list(beta0 = 5, beta1 = 0.5, beta2 = 0.01, beta3 = 0)
)

RNF_Y_Model <- jags.model(file = textConnection(RNF_Y_Model),
                          data = RNF_Y_data,
                          inits = RNF_Y_inits,
                          n.chains = 3,
                          quiet = TRUE)
update(RNF_Y_Model, n.iter = 5000)
RNF_Y_Model_res <- coda.samples(RNF_Y_Model,
                                variable.names = c("beta0", "beta1", "beta2", "beta3", "sigma2"),
                                n.iter = 100000)
plot(RNF_Y_Model_res) #Trace plots and parameter densities

gelman.plot(RNF_Y_Model_res) #BGR statistic, want closer to 1

effectiveSize(RNF_Y_Model_res[[1]][,"beta0"])
effectiveSize(RNF_Y_Model_res[[1]][,"beta1"])
effectiveSize(RNF_Y_Model_res[[1]][,"beta2"])
effectiveSize(RNF_Y_Model_res[[1]][,"beta3"])

autocorr.plot(RNF_Y_Model_res[[1]][,"beta0"], main = "Intercept") #measure of whether we have independent sampling yet
autocorr.plot(RNF_Y_Model_res[[1]][,"beta2"], main = "Cond Slope") #
autocorr.plot(RNF_Y_Model_res[[1]][,"beta3"], main = "T1 Slope") #


summary(RNF_Y_Model_res) #remember; beta0 = intercept (reflects neutral cond base), beta1 = pKM effect, beta2 = Cond' effect, beta3 = T1 Covariate effect

plot(RNS_Y_Model_res)



###########Just to see without T2 covariate--how important was it that we used this design? Not for reporting, just to see design implications
##XMY Mediation: XM -> Y without T2 covariate


##XMY Mediation: XM -> Y
#RNS
require(rjags)
RNS_Yt_Model <- "model {

#likelihood
for(i in 1:n){
Y[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*M[i] + beta2*X[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)
beta2 ~ dnorm(mu0, tau0)

tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

RNS_Y_data <- list(
  n = nrow(df),
  Y = df$RNS2_Z[,1],
  M = df$pKM_Z[,1],
  X = df$Cond,
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

RNS_Y_inits <- list(
  list(beta0 = 0, beta1 = 2, beta2 = 5),
  list(beta0 = 10, beta1 = 0.01, beta2 = 0),
  list(beta0 = 5, beta1 = 0.5, beta2 = 0.01)
)

RNS_Yt_Model <- jags.model(file = textConnection(RNS_Yt_Model),
                           data = RNS_Y_data,
                           inits = RNS_Y_inits,
                           n.chains = 3,
                           quiet = TRUE)
update(RNS_Yt_Model, n.iter = 5000)
RNS_Yt_Model_res <- coda.samples(RNS_Yt_Model,
                                 variable.names = c("beta0", "beta1", "beta2", "sigma2"),
                                 n.iter = 100000) 
plot(RNS_Yt_Model_res) #Trace plots and parameter densities

gelman.plot(RNS_Yt_Model_res) #BGR statistic, quant measure of chain mixing, very close to 1

effectiveSize(RNS_Yt_Model_res[[1]][,"beta0"]) #measure of how many 'effective' independent samples we got
effectiveSize(RNS_Yt_Model_res[[1]][,"beta1"]) 
effectiveSize(RNS_Yt_Model_res[[1]][,"beta2"])
effectiveSize(RNS_Yt_Model_res[[1]][,"beta3"])

autocorr.plot(RNS_Yt_Model_res[[1]][,"beta0"], main = "Intercept") #measure of whether we have independent sampling yet, may need thinning with larger iteration
autocorr.plot(RNS_Yt_Model_res[[1]][,"beta1"], main = "pKM Slope") #
autocorr.plot(RNS_Yt_Model_res[[1]][,"beta2"], main = "Cond Slope") #

summary(RNS_Yt_Model_res)



#RNF
require(rjags)
RNF_Yt_Model <- "model {

#likelihood
for(i in 1:n){
Y[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*M[i] + beta2*X[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)
beta2 ~ dnorm(mu0, tau0)
beta3 ~ dnorm(mu0, tau0)

tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

RNF_Y_data <- list(
  n = nrow(df),
  Y = df$RNF2_Z[,1],
  M = df$pKM_Z[,1],
  X = df$Cond,
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

RNF_Y_inits <- list(
  list(beta0 = 0, beta1 = 2, beta2 = 5),
  list(beta0 = 10, beta1 = 0.01, beta2 = 0),
  list(beta0 = 5, beta1 = 0.5, beta2 = 0.01)
)

RNF_Yt_Model <- jags.model(file = textConnection(RNF_Yt_Model),
                           data = RNF_Y_data,
                           inits = RNF_Y_inits,
                           n.chains = 3,
                           quiet = TRUE)
update(RNF_Yt_Model, n.iter = 5000)
RNF_Yt_Model_res <- coda.samples(RNF_Yt_Model,
                                 variable.names = c("beta0", "beta1", "beta2", "sigma2"),
                                 n.iter = 100000)
plot(RNF_Yt_Model_res) #Trace plots and parameter densities

gelman.plot(RNF_Yt_Model_res) #BGR statistic, want closer to 1

effectiveSize(RNF_Yt_Model_res[[1]][,"beta0"]) 
effectiveSize(RNF_Yt_Model_res[[1]][,"beta1"])

autocorr.plot(RNF_Yt_Model_res[[1]][,"beta0"], main = "Intercept") 
autocorr.plot(RNF_Yt_Model_res[[1]][,"beta1"], main = "Slope") #

summary(RNF_Yt_Model_res) #remember; beta0 = intercept (reflects neutral cond base), beta1 = mediator pKM effect, beta2 = Cond' effect


#Step 1
summary(M_Model_res)

#Step 1.9
summary(RNS_Model_res)
summary(RNF_Model_res)

#Step 2
summary(RNS_C_Model_res)
summary(RNF_C_Model_res)

#Step 2.9
summary(RNS_Yt_Model_res)
summary(RNF_Yt_Model_res)

#Step 3
summary(RNS_Y_Model_res)
summary(RNF_Y_Model_res)
