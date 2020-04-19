library(tidyverse)
library(reshape2)
library(rstan)
library(rstanarm)
library(tidybayes)


#  Read in male and female data
data = read.csv("Data/Numbers_at_age2.csv")


#-------------------------------------------------------------- 
#  Setup model data
#  Explore for females ages 5 & 6 only
#-------------------------------------------------------------- 

fem = data %>%
  filter(sex=="F" & age%in%c(4,5,6) & !is.na(age)) %>%
  select(brood_year, age, numatage)

#  Spread age data into wide-format and fill-in missing values for brood_years first with NA and then 0.
#rm(ages)
#ages = dcast(melt(fem, id.vars = c("brood_year", "age")), brood_year ~ age)

ages <- fem %>%
  spread(age,numatage) %>%
  right_join(data.frame(brood_year=min(fem$brood_year):max(fem$brood_year))) %>%
  replace(is.na(.),0) %>%
  dplyr::select(-brood_year)

#### These are parameters for stan
A = ncol(ages)
N = nrow(ages)
stock = rep(1, nrow(ages))
year = seq(1, nrow(ages))
S = 1 # number of stocks

############################################################## run stan model

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#  The next line is just some thing I found online. Not sure if it helps.
#Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

stan_data = list("A" = A, "S" = S, "N" = N, 
                 "year_id" = (year - min(year) + 1),
                 "ages" = ages, "alpha" = rep(1,A))
#stan_pars = c("sd_pro", "p", "muB", "p_mean", "b")
stan_pars = c("sd_pro", "p", "p_mean", "b")

female = stan(file = 'Stan/compositions_lm_timevarying_AR_EDub.stan',data = stan_data, 
              verbose = TRUE, chains = 3, thin = 1, 
              warmup = 2000, iter = 30000, pars = stan_pars, 
              control = list(adapt_delta=0.99999, max_treedepth=20))

saveRDS(female,"Female_3_ages_30000iter_delta_999999_tree20.RDS")

saveRDS(female,"Female_20000iter_delta_999_tree20.RDS")

extract(female)$sd_pro

sddat <- extract(female)$sd_pro %>% data.frame %>% 
  gather(year,value) %>% 
  mutate(year=as.numeric(gsub("X","",year))) %>% 
  arrange(year)

f2 <- sddat %>% 
  ggplot(aes(factor(year),value)) + 
  geom_boxplot() + 
  ylim(0,2.5)



############################################################## run stan model
#  Now for males 

rm(age)

masc = data %>%
  filter(sex=="M" & age%in%c(3,4,5,6) & !is.na(age)) %>%
  select(brood_year, age, numatage)

#  Spread age data into wide-format and fill-in missing values for brood_years first with NA and then 0.
#rm(ages)
#ages = dcast(melt(fem, id.vars = c("brood_year", "age")), brood_year ~ age)

ages <- masc %>%
  spread(age,numatage) %>%
  right_join(data.frame(brood_year=min(masc$brood_year):max(masc$brood_year))) %>%
  replace(is.na(.),0) %>%
  dplyr::select(-brood_year)

#### These are parameters for stan
A = ncol(ages)
N = nrow(ages)
stock = rep(1, nrow(ages))
year = seq(1, nrow(ages))
S = 1 # number of stocks

############################################################## run stan model

stan_data = list("A" = A, "S" = S, "N" = N, 
                 "year_id" = (year - min(year) + 1),
                 "ages" = ages, "alpha" = rep(1,A))
#stan_pars = c("sd_pro", "p", "muB", "p_mean", "b")
stan_pars = c("sd_pro", "p", "p_mean", "b")

mod = stan(file = 'Stan/compositions_lm_timevarying_AR_EDub.stan',data = stan_data, 
           verbose = TRUE, chains = 3, thin = 1, 
           warmup = 2000, iter = 15000, pars = stan_pars, 
           control = list(adapt_delta=0.99999, max_treedepth=20))

saveRDS(mod,"Male_15000iter_delta99999_tree20.RDS")


sddat <- extract(mod)$sd_pro %>% data.frame %>% 
  gather(year,value) %>% 
  mutate(year=as.numeric(gsub("X","",year))) %>% 
  arrange(year)

f2 <- sddat %>% 
  ggplot(aes(factor(year),value)) + 
  geom_boxplot() + 
  ylim(0,2.5)

pars <- extract(mod)

pars$sd_pro %>% 
  data.frame %>% 
  gather(year,value) %>% 
  mutate(year=as.numeric(gsub("X","",year))) %>% 
  arrange(year) %>% 
  ggplot() +
  geom_vridgeline(aes(x=factor(year), y=value, width = ..density..),stat="ydensity", trim=FALSE, alpha = 0.85, scale = 0.4) + 
  geom_point(aes(x=factor(year),y=median(value))) +
  scale_x_discrete(labels=as.character(1981:2013)) + 
  theme(axis.text.x=element_text(angle=90)) + 
  theme_bw()

pars$sd_pro %>% 
  data.frame %>% 
  gather(year,value) %>% 
  mutate(year=as.numeric(gsub("X","",year))) %>% 
  arrange(year) %>% 
  ggplot() +
  geom_violin(aes(x=factor(year), y=value)) + 
  scale_x_discrete(labels=as.character(1981:2013)) + 
  theme(axis.text.x=element_text(angle=90))



