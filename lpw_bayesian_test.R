library(tidyverse)
library(reshape2)
library(rstan)

#------------------------------------------------------------------------------------------
## Setup the numbers at age file for the stan model. 
## Skip this if you have the numbers_at_age2.csv file
#------------------------------------------------------------------------------------------

#recovernum <- read_csv("Data/All_Recovery_Estimated_Number.txt") %>%  
#  rename(stock=stock_location_name) %>%
#  mutate(age=(run_year-brood_year),
#         recoveryloc=ifelse(recovery_location_code=="1M1NE109 10","lpw","non-lpw")) %>%
#  filter(stock!="KETA R 101-30" & brood_year>=1981 & brood_year<=2013) %>%
#  mutate(stock=ifelse(stock=="CHICKAMIN R 101-71","Chickamin","Unuk"))

#data <- recovernum %>%
#  filter(stock!="Chickamin" &!is.na(sex) & age%in%c(3:8)) %>%
#  group_by(stock,brood_year,age,sex) %>%
#  tally() %>%
#  rename(numatage=n) %>%
#  ungroup

#data %>% 
#  write.csv("Data/Numbers_at_age2.csv")
#------------------------------------------------------------------------------------------
#  End chunk to create numbers_at_age2.csv
#------------------------------------------------------------------------------------------


#  Read in male and female data
data = read.csv("Data/Numbers_at_age2.csv")



############################################################## run stan model
#  Explore for females ages 3 & 4 only


fem = data %>%
  filter(sex=="F" & age%in%c(5,6) & !is.na(age)) %>%
  select(brood_year, age, numatage)

#  Spread age data into wide-format and fill-in missing values for brood_years first with NA and then 0.
rm(ages)
ages = dcast(melt(fem, id.vars = c("brood_year", "age")), brood_year ~ age)
ages <- fem %>%
  spread(age,numatage) %>%
  right_join(data.frame(brood_year=min(ages$brood_year):max(ages$brood_year))) %>%
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
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

stan_data = list("A" = A, "S" = S, "N" = N,
                 "year_id" = (year - min(year) + 1),
                 "ages" = ages, "alpha" = rep(1,A))
#stan_pars = c("sd_pro", "p", "muB", "p_mean", "b")
stan_pars = c("sd_pro", "p", "p_mean","pro_dev")

#mod = stan(file = 'compositions_ar.stan',data = stan_data,
#           verbose = TRUE, chains = 3, thin = 1,
#           warmup = 2000, iter = 4000, pars = stan_pars,
#           control = list(adapt_delta=0.99, max_treedepth=25))

mod = stan(file = 'compositions_lm_timevarying_AR_EDub.stan',data = stan_data,
           verbose = TRUE, chains = 1, thin = 1,
           warmup = 2000, iter = 4000, pars = stan_pars,
           control = list(adapt_delta=0.999, max_treedepth=15))

mod = stan(file = 'compositions_lm.stan',data = stan_data,
           verbose = TRUE, chains = 3, thin = 1,
           warmup = 2000, iter = 4000, pars = stan_pars,
           control = list(adapt_delta=0.99, max_treedepth=10))

pars = extract(mod)

saveRDS(mod,file="Data/Female_stan_lm_model_output2.RDS")
#saveRDS(mod,file="Data/Female_stan_lm_model_output.RDS")

print(mod)

summary(pars$b)

# look at mean proportions
df = data.frame("year" = rep(year, A), "age" = sort(rep(1:A, N)),
                "p_mean" = c(apply(pars$p, c(2,3), mean)),
                "low" = c(apply(pars$p, c(2,3), quantile, 0.025)),
                "hi" = c(apply(pars$p, c(2,3), quantile, 0.975)))

ggplot(df, aes(year, p_mean, color = as.factor(age), group=age)) +
  geom_line()

ggplot(df, aes(year, p_mean)) +
  geom_ribbon(aes(ymin=low, ymax=hi), alpha=0.3, fill="blue") +
  facet_wrap(~age, scale="free_y") + geom_line(col="blue") +
  ylab("Estimated proportion") +
  scale_x_continuous(breaks=seq(1,N,by=4),
                     labels=seq(min(data$brood_year),max(data$brood_year),by=4))


x11();
ages %>%
  gather(age,num) %>%
  mutate(year=rep(1:N,2)) %>%
  group_by(year) %>%
  mutate(propdata=num/sum(num),
         age=ifelse(age==5,1,2)) %>%
  inner_join(df) %>%
  ggplot(aes(year, p_mean)) +
  geom_ribbon(aes(ymin=low, ymax=hi), alpha=0.3, fill="blue") +
  geom_line(aes(year,propdata),color="red",linetype=2) +
  geom_point(aes(year,propdata)) +
  facet_wrap(~age, scale="free_y") + geom_line(col="blue") +
  ylab("Estimated proportion") +
  scale_x_continuous(breaks=seq(1,N,by=4),
                     labels=seq(min(data$brood_year),max(data$brood_year),by=4)) +
  geom_smooth()



#----------------------------------------------------------------------------------------
#  Now run the model for male ages 4,5,6
#----------------------------------------------------------------------------------------

masc = data %>%
  filter(sex=="M" & age%in%c(4,5,6) & !is.na(age)) %>%
  select(brood_year, age, numatage)

rm(ages)
ages = dcast(melt(masc, id.vars = c("brood_year", "age")), brood_year ~ age)
# fill in 0s in 2000
ages = data.frame("brood_year"=min(ages$brood_year):max(ages$brood_year)) %>%
  left_join(ages) %>%
  replace(is.na(.),0) %>%
  dplyr::select(-brood_year)


#### These are parameters for stan
A = ncol(ages)
#A = dim(ages)[2]
N = nrow(ages)
stock = rep(1, nrow(ages))
year = seq(1, nrow(ages))
S = 1 # number of stocks

############################################################## run stan model

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')


stan_data = list("A" = A, "S" = S, "N" = N,
                 "year_id" = (year - min(year) + 1),
                 "ages" = ages, "alpha" = rep(1,A))
#stan_pars = c("sd_pro", "p", "muB", "p_mean", "b")
stan_pars = c("sd_pro", "p", "p_mean","pro_dev")

mod = stan(file = 'compositions_lm.stan',data = stan_data,
           verbose = TRUE, chains = 3, thin = 1,
           warmup = 2000, iter = 4000, pars = stan_pars,
           control = list(adapt_delta=0.99, max_treedepth=20))

#saveRDS(mod,file="Z:\\Male_Bayesian_AR_Model_LPW.RDS")

pars_male = extract(mod)

saveRDS(mod,file="Data/Male_stan_lm_model_output.RDS")


saveRDS(mod,file="Z:\\Male_Bayesian_AR_Model_LPW.RDS")
test <- readRDS("Z:\\Male_Bayesian_AR_Model_LPW.RDS")







#------------------------------------------------------------------------------------------------
## Junk below here - female run for more than two age groups



fem = filter(data, sex=="F") %>%
  select(brood_year, age, numatage)

ages = dcast(melt(fem, id.vars = c("brood_year", "age")), brood_year ~ age)
# fill in 0s in 2000
ages = data.frame("brood_year"=min(ages$brood_year):max(ages$brood_year)) %>%
  left_join(ages) %>%
  replace(is.na(.),0) %>%
  dplyr::select(-brood_year)

#ages = ages[,paste(3:7)]
#for(i in 1:ncol(ages)) {
#  ages[which(is.na(ages[,i])),i] = 0
#}

#### These are parameters for stan
A = ncol(ages)
#A = dim(ages)[2]
N = nrow(ages)
stock = rep(1, nrow(ages))
year = seq(1, nrow(ages))
S = 1 # number of stocks

############################################################## run stan model

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_data = list("A" = A, "S" = S, "N" = N,
                 "year_id" = (year - min(year) + 1),
                 "ages" = ages, "alpha" = rep(1,A))
stan_pars = c("sd_pro", "p", "p_mean", "b")

mod = stan(file = 'compositions_lm.stan',data = stan_data,
           verbose = TRUE, chains = 3, thin = 1,
           warmup = 2000, iter = 4000, pars = stan_pars,
           control = list(adapt_delta=0.99, max_treedepth=20))

pars = extract(mod)

summary(pars$b)

# look at mean proportions
df = data.frame("year" = rep(year, A), "age" = sort(rep(1:A, N)),
                "p_mean" = c(apply(pars$p, c(2,3), mean)),
                "low" = c(apply(pars$p, c(2,3), quantile, 0.025)),
                "hi" = c(apply(pars$p, c(2,3), quantile, 0.975)))

ggplot(df, aes(year, p_mean, color = as.factor(age), group=age)) +
  geom_line()

ggplot(df, aes(year, p_mean)) +
  geom_ribbon(aes(ymin=low, ymax=hi), alpha=0.3, fill="blue") +
  facet_wrap(~age, scale="free_y") + geom_line(col="blue") +
  ylab("Estimated proportion")

