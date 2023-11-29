source("./utils.R")

library(brms)
library(ggdist)
library(cmdstanr)
library(patchwork)

dirname <- "B117fits"
if (!dir.exists(dirname)) {
  dir.create(dirname)
}

bdata = 
  get_log10Load_data() %>%
  .[, PCR_Gender_Group := factor(paste0(PCR,"_",Gender,"_",Group))] %>%
  .[, PCR_Group := factor(paste0(PCR,"_",Group))] %>%
  .[, fAgeGroup := factor(AgeGroup, ordered = F)] %>% 
  .[, Male := ifelse(Gender == "M",1,ifelse(Gender == "F",0,.5))]

bdata.B117 = 
  bdata %>%
  .[, B117 := factor(B117, labels = c("non-B117","B117"))] %>% 
  .[, Sex := ifelse(Gender == "U",.5,ifelse(Gender == "M",1,0))]

B117data = 
  bdata.B117 %>%
  .[B117CentreDay1 == 1]
print(dim(B117data))
### with adjustment, non-B117 & B117 in centre ####
CentresWithMin2Cases = 
  B117data[,.(N = .N), by = .(TestCentre)][N > 1,TestCentre]
B117data = 
  B117data[TestCentre %in% CentresWithMin2Cases]

B117model.re.adjusted =
  bf(log10Load ~ B117 + Group + Sex + PCR + s(Age) + (1 | TestCentre),
     sigma ~ B117 + PAMS + (1 | TestCentre))

scode <- make_stancode(B117model.re.adjusted, data = B117data)
spath <- write_stan_file(scode)
cat(scode)

sdata <- make_standata(B117model.re.adjusted, data = B117data)

mod <- cmdstan_model(spath)
fit <- mod$sample(data = sdata,
                  chains = 4,
                  parallel_chains = 4,
                  iter_sampling = 2500,
                  iter_warmup = 1000,
                  adapt_delta = 0.99,
                  refresh = 100)

# Bfit <- brm(B117model.re.adjusted, data = B117data, empty = TRUE)
# Bfit$fit <- fit
# Bfit <- rename_pars(Bfit)

# B117_model_stats = get_B117_stats(Bfit,"RE, adjusted, (paired)",1,"Yes")
# data.table(rhat = rhat(Bfit)) %>% 
#   ggplot(aes(x = rhat)) + 
#   geom_histogram(bins = 100) + 
#   geom_vline(xintercept = 1.1, color = "red")+ 
#   geom_vline(xintercept = 1.01, color = "red", lty = 3)

# B117fit = Bfit
# saveRDS(B117fit, file.path(dirname, "B117fit.RData"))