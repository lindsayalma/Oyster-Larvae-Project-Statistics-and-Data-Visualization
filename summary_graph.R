library(tidyverse)
library(here)
library(janitor)
setwd("C:/Users/Lindsay Alma/Dropbox/Raccoon/larvae/oyster/larvae survival stats/GitHub/RaccoonOlyLarvalSurvival2/data/RaccoonLikelyhoodLarvaeInJar")

library(readxl)
all500cox_coeff_HR_CI_122721 <- read_excel("C:/Users/Lindsay Alma/Dropbox/Raccoon/larvae/oyster/larvae survival stats/GitHub/RaccoonOlyLarvalSurvival2/data/RaccoonLikelyhoodLarvaeInJar/all500cox_coeff_HR_CI_122721.xlsx")
View(all500cox_coeff_HR_CI_122721)

#d <- read_csv(here("data", "RaccoonLikelyhoodLarvaeInJar", "all500cox_coeff_HR_CI_122721.csv")) %>%
View(all500cox_coeff_HR_CI_122721)  
d<-all500cox_coeff_HR_CI_122721 %>%
    clean_names() %>%
  mutate(comparison = paste(site_1, treatment_2, "x", site_4, treatment_5, sep = "_")) %>%
  mutate(exp_mean = exp(mean_expected_hr),
         exp_lower = exp(x0_025quantile), 
         exp_upper = exp(x0_975quantile)) %>%
  {.}






head(d)
View(d)

write.csv(d, "d.csv")

#needs to be an Excel file to read in the degrees
all500cox_coeff_HR_CI_exp_060922 <- read_excel("all500cox_coeff_HR_CI_exp_060922.xlsx")
View(all500cox_coeff_HR_CI_exp_060922)
d<-all500cox_coeff_HR_CI_exp_060922


color<-d$color

p <- d %>%
  ggplot(aes(reorder(comparison, exp.lower), exp.mean)) +
    geom_point(colour = color) +
    geom_errorbar(aes(ymin = exp.lower, ymax = exp.upper), color=color) +
    geom_hline(yintercept = 1, linetype = "dashed")

p

p + coord_flip()+
  xlab("Comparison") + ylab("Hazard Ratio")+ 
  theme_classic()







sggsave(filename = here("figs", "hr_interactions.png"), plot = p)


