source("./utils.R")

library(brms)

dirname <- "B117fits"
if (!dir.exists(dirname)) {
  dir.create(dirname)
}

# get data
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

B117model.unadjusted =
  bf(log10Load ~ B117,
     sigma ~ B117 + (1 | TestCentre))

B117model.re =
  bf(log10Load ~ B117 + (1 | TestCentre),
     sigma ~ B117 + PAMS + (1 | TestCentre))

B117model.re.adjusted =
  bf(log10Load ~ B117 + Group + Sex + PCR + s(Age) + (1 | TestCentre),
     sigma ~ B117 + PAMS + (1 | TestCentre))

######### all, unadjusted #########
print("all, unadjusted")
B117data = 
  bdata.B117 
print(dim(B117data))
Bfit = fit_B117_model(B117model.unadjusted,B117data,"B117model_unadjusted_all",adapt_delta = .8)
B117_model_stats = get_B117_stats(Bfit,"unadjusted",Inf)
Bfit = fit_B117_model(B117model.re,B117data,"B117model_re_all",adapt_delta = .8)
B117_model_stats = rbind(B117_model_stats,get_B117_stats(Bfit,"RE, unadjusted",Inf))
B117_model_stats = rbind(B117_model_stats,get_B117_stats(Bfit,"RE, adjusted",Inf))

########### +/- 5 days ########### 
print("+/- 5 days")
B117data = 
  bdata.B117 %>%
  .[B117CentreDay5 == 1]
print(dim(B117data))
### with adjustment ####
Bfit = fit_B117_model(B117model.unadjusted,B117data,"B117model_unadjusted_5",adapt_delta = .99)
B117_model_stats = rbind(B117_model_stats,get_B117_stats(Bfit,"unadjusted",5))
Bfit = fit_B117_model(B117model.re,B117data,"B117model_re_5",adapt_delta = .99)
B117_model_stats = rbind(B117_model_stats,get_B117_stats(Bfit,"RE, unadjusted",5))
Bfit = fit_B117_model(B117model.re.adjusted,B117data,"B117model_re_adjusted_5",adapt_delta = .99)
B117_model_stats = rbind(B117_model_stats,get_B117_stats(Bfit,"RE, adjusted",5))

########### +/- 1 day ########### 
print("+/- 1 day")
B117data = 
  bdata.B117 %>%
  .[B117CentreDay1 == 1]
print(dim(B117data))
####  no adjustment ####
Bfit = fit_B117_model(B117model.unadjusted,B117data,"B117model_unadjusted_1",adapt_delta = .99)
B117_model_stats = rbind(B117_model_stats,get_B117_stats(Bfit,"unadjusted",1))
Bfit = fit_B117_model(B117model.re,B117data,"B117model_re_1",adapt_delta = .99)
B117_model_stats = rbind(B117_model_stats,get_B117_stats(Bfit,"RE, unadjusted",1))
Bfit = fit_B117_model(B117model.re.adjusted,B117data,"B117model_re_adjusted_1",adapt_delta = .99)
B117_model_stats = rbind(B117_model_stats,get_B117_stats(Bfit,"RE, adjusted",1))
### with adjustment, non-B117 & B117 in centre ####
CentresWithMin2Cases = 
  B117data[,.(N = .N), by = .(TestCentre)][N > 1,TestCentre]
B117data = 
  B117data[TestCentre %in% CentresWithMin2Cases]
Bfit = fit_B117_model(B117model.re.adjusted,B117data,"B117model_re_adjusted_1wcc",adapt_delta = .99)
B117_model_stats = rbind(B117_model_stats,get_B117_stats(Bfit,"RE, adjusted, (paired)",1,"Yes"))
data.table(rhat = rhat(Bfit)) %>% 
  ggplot(aes(x = rhat)) + 
  geom_histogram(bins = 100) + 
  geom_vline(xintercept = 1.1, color = "red")+ 
  geom_vline(xintercept = 1.01, color = "red", lty = 3)

B117fit = Bfit
rm(Bfit)
saveRDS(B117fit, file.path(dirname, "B117fit.RData"))