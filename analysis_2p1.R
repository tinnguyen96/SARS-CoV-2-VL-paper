source("./utils.R")

# get data
bdata = 
  get_log10Load_data() %>%
  .[, PCR_Gender_Group := factor(paste0(PCR,"_",Gender,"_",Group))] %>%
  .[, PCR_Group := factor(paste0(PCR,"_",Group))] %>%
  .[, fAgeGroup := factor(AgeGroup, ordered = F)] %>% 
  .[, Male := ifelse(Gender == "M",1,ifelse(Gender == "F",0,.5))]


model_formula = 
  bf(log10Load ~ B117 + PCR + Male + Group + s(Age, by = Group) + (1 | TestCentreCategory/Hospitalized) ,
     family = student(),
     sigma ~ PCR + (1 | fAgeGroup) + (1 | TestCentreCategory)) 

# Specify weakly informative priors
prior = 
  c(prior(normal(6, 3), class = Intercept),
    prior(normal(0, 2), class = b),
    prior(normal(0, 2), class = sd),
    prior(normal(0, 1), class = b, dpar = "sigma"),
    prior(normal(0, 1), class = Intercept, dpar = "sigma"),
    prior(normal(0, 1), class = sd, dpar = "sigma"),
    prior(normal(0, 1), class = sds)
  )

# make output-skeleton to get parameter dimensions 
# in a convenient format to be used to set initial 
# parameter values
bfit = brm(model_formula,
           data = bdata,
           chains = 1,
           iter = 10,
           prior = prior,
           backend = "cmdstanr")

############ fit modified model ###########
my_inits = lapply(1:4,function(x) make_age_fit_inits(bfit))
bfit = brm(model_formula,
           data = bdata,
           chains = 4,
           iter = 2000,
           prior = prior,
           inits = my_inits,
           backend = "cmdstanr")

sampler_params = 
  nuts_params(bfit) %>% 
  data.table() %>% 
  dcast(Chain + Iteration ~ Parameter, value.var = "Value")

fn = here("FPT/splines_TP+.Rdata")
save(bfit,sampler_params,file = fn)
draws = as_draws(bfit$fit)
save(bfit,draws,sampler_params,file = fn)