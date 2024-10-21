#kaplan meier survival

#load libraries
library(survival)
library(survminer)
library(ggplot2)
library(rms)
library(readxl)
library(survELtest)
library(readxl)
OlyLarvaeKMforR <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/larvae/oyster/larvae survival stats/GitHub/RacoonOlyLarvalSurvival/data/fraility_RacoonOlyLarvaeSurvival/OlyLarvaeKMforR.xlsx")
View(OlyLarvaeKMforR)

# use gsub if you want to change 14+ to 14 in R

#make surv object
KMsurv = Surv(time = OlyLarvaeKMforR$day, OlyLarvaeKMforR$dead, type = "right")

# fit KM model and plot without random effect of tank
sf <- survfit(KMsurv ~ Treatment+Location, data = OlyLarvaeKMforR)
summary(coxph(KMsurv ~ Treatment*Location, data = OlyLarvaeKMforR))

# Graph the KMsurvival distribution
#you can add pval = TRUE, but we know our p>0.0001
ggsurvplot(sf, conf.int = 0.05)


# another type of graph
plot(sf, xlab="Larval Age", 
     ylab="% Surviving", yscale=100, col=c("springgreen4", "royalblue3", "red","orange2","springgreen2","dodgerblue","indianred","gold1"),
     main="% Larval Survival") 

#Plot with breaktime by = # of days, palette=number of treatments listed colors
fontsize<-20
pCox <- ggsurvplot(sf, data=OlyLarvaeKMforR, risk.table = FALSE, pval = FALSE, conf.int = TRUE,
                   font.main = fontSize, font.x =  fontSize, font.y = fontSize, 
                   font.tickslab = fontSize, font.legend = fontSize,
                   palette = c("springgreen4", "royalblue3", "red","orange2","springgreen2","dodgerblue","indianred","gold1"), legend = "none"
) +  xlab("Time (d)")
pCox$plot

#add a legend, dont need. probably make one not on R
legend("topright" , c("CI20-14C","CI5-14C","DB-14C","PW-14C", "CI20-20C", "CI5-20C", "DB-20C", "PW-20C"),
       fill=c("springgreen4", "royalblue3", "red","orange2","springgreen2","dodgerblue","indianred","gold1"))

#another type of graph- will prob use this one
ggsurvplot(sf, data=OlyLarvaeKMforR, conf.int=T,  risk.table=F, pval=F,legend=c("right"),
           legend.labs=c("CI20-14C","CI5-14C","DB-14C","PW-14C", "CI20-20C", "CI5-20C", "DB-20C", "PW-20C"),legend.title="Treatment", 
           palette =  c('darkgreen', 'blue4', 'darkred', 'darkgoldenrod', '#33CC66','steelblue','red','darkgoldenrod1'),   
           risk.table.height=.25,xlab="Time (days)", size=0.7, break.time.by = 3, break.y.by=.2, ggtheme = theme_bw() +  theme(
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank(),  
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank()
           ))


#Test non parametric (logrank test) p<0.0001 chisq=4053
#report this value in paper to compare data from multuple pairwise groups
#log-rank test does not make assumptions about survival distribution. Analyzes each cohort separetely. 
" When
survival curves cross, the log-rank test should not be
used because the probability of a type II error will be
increased, and the test has low power."
survdiff(formula=Surv(day,dead)~Treatment +Location, data=OlyLarvaeKMforR)

#emperica likelyhood-optimize power

#p-value <0.0001
surv_pvalue(sf) 

#cox model sample code without interaction
cox1<-coxph(KMsurv~Treatment+Location, data=OlyLarvaeKMforR)
summary(cox1)

#cox model sample code with interaction, this is the model for the main analysis
cox<-coxph(KMsurv~Treatment*Location, data=OlyLarvaeKMforR)
cox##<here results!!!
summary(cox)
#38400 larvae, 25506 death, 12894 censored 
#Looking at exp(coef)-- for example the Hazard Ratio of 20C is 0.586, this means that each additional
#day is associated with a 0.586 fold increse in the hazard death when compared to 14C, and its 95% confidence
#interval is (0.56,0.61). I think..Since this confidence interval is <1
#it indicates a decrease in the hazard survival compared to 14C, and there is a significant association
#between days and temperature (p<0.0001)
anova(cox)
cox.zph(cox)
plot(cox.zph(cox)) 
#A hazard ratio of 1 indicates no effect
#A hazard ratio > 1 indicates an increase in the hazard as the covariate rises- It gets to death faster than the control
#A hazard ratio < 1 indicates a decrease in the hazard as the covariate rises- it is slower go get to the event (death) than the cotnrol
ggforest(cox, data=OlyLarvaeKMforR)

#time-dependent covariate to fix proportional hazard assumption
#A Cox model with time-dependent covariate would compare the risk of an event between transplant and nontransplant at each event time, but would re-evaluate
#which risk group each person belonged in based on whether
#they'd had a transplant by that time

#model with temp only
coxTemp<-coxph(KMsurv~Treatment, data=OlyLarvaeKMforR)
ggforest(coxTemp, data=OlyLarvaeKMforR)

#model with locaiton only
coxLocation<-coxph(KMsurv~Location, data=OlyLarvaeKMforR)
ggforest(coxLocation, data=OlyLarvaeKMforR)

#model with non categorial veraibles
cox.number<-coxph(KMsurv~Temp+LocationNumber+Temp*LocationNumber, data=OlyLarvaeKMforR)
cox.number
summary(cox.number)
ggforest(cox.number)

############time varying coefficients

#show time points in time at which an individual died
cut.points <- unique(OlyLarvaeKMforR$day[OlyLarvaeKMforR$dead == 1])

#duplicate 4 line per individual, times 0-1, 1-7, 7-10, 10-13 =time0 and time
SURV2 <- survSplit(data = OlyLarvaeKMforR, cut = cut.points, end = "day", start = "day0", event = "dead")
View(SURV2)
cut.points

#run the cox model on orginal data- estimates one treatment male-21yrs
model.1 <- coxph(Surv(day, dead) ~ Treatment+Location+Treatment:Location, data = OlyLarvaeKMforR)
model.1
summary(model.1)
#Schoenfeld's global test for the violation of proportional assumption-- Grambsch PM, Therneau TM. Proportional hazards tests
# diagnostics based on weighted residuals. Biometrika 1994;81:515-26
zph<-cox.zph(model.1)
zph
#results of cox.zph show there is significant deviation from the proportional hazards assumption for the variable
covs <- data.frame(Location = "CI20", Treatment = "14C")
covs
summary(survfit(model.1, newdata = covs, type = "aalen"))
#output gives us probability of survival for CI20 14C for each day, i.e. a larvae has a .708 chance of survivind to day 4, but 0.163 chance of surviving to day 15
cox.zph(model.1)
#The result of zph shows that there is significant deviation from the proportional hazards assumption for all veriables
ggforest(model.1, SURV2)
plot(cox.zph(model.1)) 


####Percent mortality
CI2020C<-subset(OlyLarvaeKMforR, TreatmentLocation=="20CCI20")
CI2020Cend<-sum(CI2020C$Status)
CI2020Cend
CI2020Ctot<-length(CI2020C$Status)
CI2020Ctot
CI2020Cend/CI2020Ctot

CI2014C<-subset(OlyLarvaeKMforR, TreatmentLocation=="14CCI20")
CI2014Cend<-sum(CI2014C$Status)
CI2014Cend
CI2014Ctot<-length(CI2014C$Status)
CI2014Ctot
CI2014Cend/CI2014Ctot

CI520C<-subset(OlyLarvaeKMforR, TreatmentLocation=="20CCI5")
CI520Cend<-sum(CI520C$Status)
CI520Cend
CI520Ctot<-length(CI520C$Status)
CI520Ctot
CI520Cend/CI520C

CI514C<-subset(OlyLarvaeKMforR, TreatmentLocation=="14CCI5")
CI514Cend<-sum(CI514C$Status)
CI514Cend
CI514Ctot<-length(CI514C$Status)
CI514Ctot
CI514Cend/CI514Ctot

DB520C<-subset(OlyLarvaeKMforR, TreatmentLocation=="20CDB")
DB520Cend<-sum(DB520C$Status)
DB520Cend
DB520Ctot<-length(DB520C$Status)
DB520Ctot
DB520Cend/DB520Ctot

DB514C<-subset(OlyLarvaeKMforR, TreatmentLocation=="14CDB")
DB514Cend<-sum(DB514C$Status)
DB514Cend
DB514Ctot<-length(DB514C$Status)
DB514Ctot
DB514Cend/DB514Ctot

PW520C<-subset(OlyLarvaeKMforR, TreatmentLocation=="20CPW")
PW520Cend<-sum(PW520C$Status)
PW520Cend
PW520Ctot<-length(PW520C$Status)
PW520Ctot
PW520Cend/PW520Ctot

PW514C<-subset(OlyLarvaeKMforR, TreatmentLocation=="14CPW")
PW514Cend<-sum(PW514C$Status)
PW514Cend
PW514Ctot<-length(PW514C$Status)
PW514Ctot
PW514Cend/PW514Ctot