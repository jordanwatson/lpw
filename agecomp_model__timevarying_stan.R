############################################################## run stan model
#  Explore for females ages 5 & 6 only 

library(tidyverse)
library(reshape2)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

data = read.csv("Data/Numbers_at_age.csv")

fem = filter(data, sex=="F") %>% 
  select(brood_year, age, numatage)

ages = dcast(melt(fem, id.vars = c("brood_year", "age")), brood_year ~ age)
# fill in 0s in 2000
ages = left_join(data.frame("brood_year"=min(ages$brood_year):max(ages$brood_year)), ages)


fem = filter(data, sex=="F" & age%in%c(5,6) & !is.na(age)) %>% 
  select(brood_year, age, numatage)

#  Spread age data into wide-format and fill-in missing values for brood_years first with NA and then 0.
ages <- fem %>% 
  spread(age,numatage) %>% 
  right_join(data.frame(brood_year=min(ages$brood_year):max(ages$brood_year))) %>% 
  replace(is.na(.),0) %>% 
  dplyr::select(-brood_year)

#### These are parameters for stan
A = dim(ages)[2]
N = nrow(ages)
stock = rep(1, nrow(ages))
year = seq(1, nrow(ages))
S = 1 # number of stocks

############################################################## run stan model

stan_data = list("A" = A, "S" = S, "N" = N, 
                 "year_id" = (year - min(year) + 1),
                 "ages" = ages, "alpha" = rep(1,A))
#stan_pars = c("sd_pro", "p", "muB", "p_mean", "b")
stan_pars = c("sd_pro", "p", "p_mean", "b","sd_pro")

mod = stan(file = 'Stan/compositions_lm_timevarying_AR.stan',data = stan_data, 
           verbose = TRUE, chains = 3, thin = 1, 
           warmup = 2000, iter = 4000, pars = stan_pars, 
           control = list(adapt_delta=0.99, max_treedepth=10))

pars = extract(mod)

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



