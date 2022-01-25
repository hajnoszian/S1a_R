
library(brms)
library(rstan)
options(mc.cores = parallel::detectCores()) ##make sure to do this in the future for your code
rstan_options(auto_write = TRUE)
library(correlation)
library(bayestestR)
library(tidyverse)
library(ggplot2)
library(tidybayes)
library(ggthemes)

cor_table = correlation(df[, c('Age_1', 'pKM_Z', 'RNS1_Z', 'RNS2_Z',
                               'RNF1_Z', 'RNF2_Z', 'Attach_anx_Z',
                               'Attach_avd_Z')], bayesian = T)
cor_table[c(1:8)]

m1mod = bf(pKM_Z ~ Cond)
ymod = bf(RNS2_Z ~ Cond + pKM_Z)
med_model.1 = brm(ymod + m1mod + set_rescor(FALSE), warmup = 1000,
                iter = 8000, data = df, 
                prior = c(prior(normal(0,1), class = 'Intercept'),
                          prior(normal(0,1), class = 'Intercept', 
                                resp = 'pKMZ'),
                          prior(normal(0,1), class = 'Intercept', 
                                resp = 'RNS2Z'),
                          prior(normal(2,1), coef = 'CondKM', resp = 'pKMZ'),
                          prior(normal(2,1), coef = 'CondKM', resp = 'RNS2Z'),
                          prior(normal(2,1), coef = 'pKM_Z', resp = 'RNS2Z')),
                save_pars = save_pars(all = TRUE))

mediation(med_model.1)


m1mod = bf(pKM_Z ~ Cond + Gender)
ymod = bf(RNS2_Z ~ Cond + pKM_Z + Gender)
med_model.2 = brm(ymod + m1mod + set_rescor(FALSE), warmup = 1000,
                  iter = 8000, data = df, 
                  prior = c(prior(normal(0,1), class = 'Intercept'),
                            prior(normal(0,1), class = 'Intercept', 
                                  resp = 'pKMZ'),
                            prior(normal(0,1), class = 'Intercept', 
                                  resp = 'RNS2Z'),
                            prior(normal(2,1), coef = 'CondKM', resp = 'pKMZ'),
                            prior(normal(2,1), coef = 'CondKM', resp = 'RNS2Z'),
                            prior(normal(2,1), coef = 'pKM_Z', resp = 'RNS2Z'),
                            prior(normal(0,1), coef = 'GenderMale', 
                                  resp = 'pKMZ'),
                            prior(normal(0,1), coef = 'GenderMale',
                                  resp = 'RNS2Z')),
                  save_pars = save_pars(all = TRUE))

mediation(med_model.2)

m1mod = bf(pKM_Z ~ Cond + Gender + Attach_anx_Z + Attach_avd_Z)
ymod = bf(RNS2_Z ~ Cond + pKM_Z + Gender + Attach_anx_Z + Attach_avd_Z)
med_model.3 = brm(ymod + m1mod + set_rescor(FALSE), warmup = 1000,
                  iter = 8000, data = df, 
                  prior = c(prior(normal(0,1), class = 'Intercept'),
                            prior(normal(0,1), class = 'Intercept', 
                                  resp = 'pKMZ'),
                            prior(normal(0,1), class = 'Intercept', 
                                  resp = 'RNS2Z'),
                            prior(normal(2,1), coef = 'CondKM', resp = 'pKMZ'),
                            prior(normal(2,1), coef = 'CondKM', resp = 'RNS2Z'),
                            prior(normal(2,1), coef = 'pKM_Z', resp = 'RNS2Z'),
                            prior(normal(0,1), coef = 'GenderMale', 
                                  resp = 'pKMZ'),
                            prior(normal(0,1), coef = 'GenderMale',
                                  resp = 'RNS2Z'),
                            prior(normal(0,1), coef = 'Attach_anx_Z',
                                  resp = 'pKMZ'),
                            prior(normal(-2,1), coef = 'Attach_avd_Z',
                                  resp = 'pKMZ'),
                            prior(normal(-2,1), coef = 'Attach_anx_Z',
                                  resp = 'RNS2Z'),
                            prior(normal(-2,1), coef = 'Attach_avd_Z',
                                  resp = 'RNS2Z')),
                  save_pars = save_pars(all = TRUE))

mediation(med_model.3, method = 'HDI')

comp.loo = loo(med_model.1, med_model.2, med_model.3) # 3 is winner
comp.bfs = bayesfactor_models(med_model.1, med_model.2, med_model.3) # 3 winner

# plot R^2 values for  mediation model
bayes_R2(med_model.3, summary = F) %>%
  data.frame() %>%
  pivot_longer(everything()) %>%
  mutate(name = str_remove(name, 'R2')) %>%
  
  ggplot(aes(x = value, color = name, fill = name)) +
  geom_density(alpha = .5) + 
  scale_color_ptol() +
  scale_fill_ptol() +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = 0:1) +
  labs(title = expression(paste('Our ', italic(R)^{2}, 'distributions')), x = NULL) +
  theme_minimal() +
  theme(legend.title = element_blank())
