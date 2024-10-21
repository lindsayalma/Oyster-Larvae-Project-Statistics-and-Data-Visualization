#KM survival curves

library(survival)
library(survminer)
library(ggplot2)
library(rms)
library(readxl)
setwd("C:/Users/Lindsay/Dropbox/Raccoon/larvae/oyster/larvae survival stats/GitHub/RacoonOlyLarvalSurvival/KM curves -single treatment-locatation")
OlyLarvaeKMforR <- read_excel("data/fraility_RacoonOlyLarvaeSurvival/OlyLarvaeKMforR.xlsx")

##CI20############
CI20<-subset(OlyLarvaeKMforR, Location=="CI20")

#make surv object
survCI20 <-Surv(time = CI20$day, CI20$dead, type = "right")

# fit model and plot without random effect
sfCI20 <- survfit(survCI20 ~ Treatment, data = CI20)
coxA<-(coxph(survCI20 ~ Treatment, data = CI20))
coxA
summary(coxph(survCI20 ~ Treatment, data = CI20))
ggsurvplot(sf, conf.int = TRUE)

#plot Km curves
#darker color=14c, lighter color=20c
ggsurvplot(sfCI20, data=CI20, conf.int=T,  risk.table=F, pval=F,legend=c("right"),
           legend.labs=c("14C","20C"),legend.title="Treatment", 
           palette =  c('#006600', '#66ff66'),   
           risk.table.height=.25,xlab="Time (days)", size=0.7, break.time.by = 3, break.y.by=.2, ggtheme = theme_bw() +  theme(
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),  
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank()
           ))


#COX
#cox model sample code with interaction, this is the model for the main analysis
coxCI20<-coxph(survCI20~Treatment, data=CI20)
coxCI20##<here results!!!, p<0.0001
summary(coxCI20)
ggforest(coxCI20, data=CI20)
#Concordance= 0.575  (se = 0.003 )
#Likelihood ratio test= 696.8  on 1 df,   p=<2e-16
#Wald test            = 687.9  on 1 df,   p=<2e-16
#Score (logrank) test = 708  on 1 df,   p=<2e-16
#Test no parametric (logrank test) p<0.0001 chisq=4053
survdiff(formula=Surv(day,dead)~Treatment, data=CI20)
#Chisq= 675  on 1 degrees of freedom, p= <2e-16 
surv_pvalue(sfCI20) 
anova(coxCI20)
#p<0.0001
cox.zph(coxCI20)
plot(cox.zph(coxCI20)) 
ggforest(coxCI20, data=CI20)
#sig dfference between temps at CI20 p<0.001 HR=0.54



#CI5########################

CI5<-subset(OlyLarvaeKMforR, Location=="CI5")

#make surv object
survCI5 <-Surv(time = CI5$day, CI5$dead, type = "right")

# fit model and plot without random effect
sfCI5 <- survfit(survCI5 ~ Treatment, data = CI5)
summary(coxph(survCI5 ~ Treatment, data = CI5))
ggsurvplot(sfCI5, conf.int = TRUE)

#plot Km curves
#darker color=14c, lighter color=20c
ggsurvplot(sfCI5, data=CI5, conf.int=T,  risk.table=F, pval=F,legend=c("right"),
           legend.labs=c("14C","20C"),legend.title="Treatment", 
           palette =  c('#33CCFF', '#000066'),   
           risk.table.height=.25,xlab="Time (days)", size=0.7, break.time.by = 3, break.y.by=.2, ggtheme = theme_bw() +  theme(
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),  
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank()
           ))


#COX
#cox model sample code with interaction, this is the model for the main analysis
coxCI5<-coxph(survCI5~Treatment, data=CI5)
coxCI5##<here results!!!, p<0.0001
summary(coxCI5)
ggforest(coxCI5, data=CI5)
#Concordance= 0.575  (se = 0.003 )
#Likelihood ratio test= p=<2e-16
#Wald test            =   p=<2e-16
#Score (logrank) test = p=<2e-16
survdiff(formula=Surv(day,dead)~Treatment, data=CI5)
#Test non parametric (logrank test) p<0.0001 
# Chisq= 406  on 1 degrees of freedom, p= <2e-16 
surv_pvalue(sfCI5) 
anova(coxCI5)
#p<0.0001
cox.zph(coxCI5)
plot(cox.zph(coxCI5)) 
ggforest(coxCI5, data=CI5)
#sig dfference between temps at CI5 p<0.001 HR=0.62






#DB#############
##DB############
DB<-subset(OlyLarvaeKMforR, Location=="DB")

#make surv object
survDB <-Surv(time = DB$day, DB$dead, type = "right")

# fit model and plot without random effect
sfDB <- survfit(survDB ~ Treatment, data = DB)
summary(coxph(survDB ~ Treatment, data = DB))
ggsurvplot(sfDB, conf.int = TRUE)

#plot Km curves
#darker color=14c, lighter color=20c
ggsurvplot(sfDB, data=DB, conf.int=T,  risk.table=F, pval=F,legend=c("right"),
           legend.labs=c("14C","20C"),legend.title="Treatment", 
           palette =  c('#660000', '#FF6666'),   
           risk.table.height=.25,xlab="Time (days)", size=0.7, break.time.by = 3, break.y.by=.2, ggtheme = theme_bw() +  theme(
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),  
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank()
           ))


#COX
#cox model sample code with interaction, this is the model for the main analysis
coxDB<-coxph(survDB~Treatment, data=DB)
coxDB##<here results!!!, p<0.0001
summarycoxDB<-summary(coxDB)
#Concordance= 0.575  (se = 0.003 )
#Likelihood ratio test=   p=<2e-16
#Wald test            =  p=<2e-16
#Score (logrank) test =   p=<2e-16
#Test no parametric (logrank test) p<0.0001 chisq=4053
survdiff(formula=Surv(day,dead)~Treatment, data=DB)
#Chisq= 331  on 1 degrees of freedom, p= <2e-16 
surv_pvalue(sfDB) 
anova(coxDB)
#p<0.0001
cox.zph(coxDB)
plot(cox.zph(coxDB)) 
ggforest(coxDB, data=DB)
#sig dfference between temps at DB p<0.001 HR=0.59

summarycoxDB$conf.int




##PW############
PW<-subset(OlyLarvaeKMforR, Location=="PW")

#make surv object
survPW <-Surv(time = PW$day, PW$dead, type = "right")

# fit model and plot without random effect
sfPW <- survfit(survPW ~ Treatment, data = PW)
summary(coxph(survPW ~ Treatment, data = PW))
ggsurvplot(sfPW, conf.int = TRUE)

#plot Km curves
#darker color=14c, lighter color=20c
ggsurvplot(sfPW, data=PW, conf.int=T,  risk.table=F, pval=F,legend=c("right"),
           legend.labs=c("14C","20C"),legend.title="Treatment", 
           palette =  c('#996600', '#FFCC33'),   
           risk.table.height=.25,xlab="Time (days)", size=0.7, break.time.by = 3, break.y.by=.2, ggtheme = theme_bw() +  theme(
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),  
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank()
           ))


#COX
#cox model sample code with interaction, this is the model for the main analysis
coxPW<-coxph(survPW~Treatment, data=PW)
coxPW##<here results!!!, p<0.0001
summary(coxPW)
#Concordance= 0.575  (se = 0.003 )
#Likelihood ratio test= 696.8  on 1 df,   p=<2e-16
#Wald test            = 687.9  on 1 df,   p=<2e-16
#Score (logrank) test = 708  on 1 df,   p=<2e-16
#Test no parametric (logrank test) p<0.0001 chisq=4053
survdiff(formula=Surv(day,dead)~Treatment, data=PW)
#Chisq= 675  on 1 degrees of freedom, p= <2e-16 
surv_pvalue(sfPW) 
anova(coxPW)
#p<0.0001
cox.zph(coxPW)
plot(cox.zph(coxPW)) 
ggforest(coxPW, data=PW)
#sig dfference between temps at PW p<0.001 HR=0.62





##14C############
C14<-subset(OlyLarvaeKMforR, Treatment=="14C")

#make surv object
survC14 <-Surv(time = C14$day, C14$dead, type = "right")

# fit model and plot without random effect
sfC14 <- survfit(survC14 ~ Location, data = C14)
summary(coxph(survC14 ~ Location, data = C14))
ggsurvplot(sfC14, conf.int = TRUE)

#plot Km curves
#darker color=C14, lighter color=20c
ggsurvplot(sfC14, data=C14, conf.int=T,  risk.table=F, pval=F,legend=c("right"),
           legend.labs=c("CI20","CI5", "DB","PW"),legend.title="Location", 
           palette =  c('darkgreen', 'blue4', 'darkred', 'darkgoldenrod'),   
           risk.table.height=.25,xlab="Time (days)", size=0.7, break.time.by = 3, break.y.by=.2, ggtheme = theme_bw() +  theme(
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),  
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank()
           ))


#COX
#cox model sample code with interaction, this is the model for the main analysis
coxC14<-coxph(survC14~Location, data=C14)
coxC14##<here results!!!, p<0.0001
summary(coxC14)
#Concordance= 0.575  (se = 0.003 )
#Likelihood ratio test= 696.8  on 1 df,   p=<2e-16
#Wald test            = 687.9  on 1 df,   p=<2e-16
#Score (logrank) test = 708  on 1 df,   p=<2e-16
#Test no parametric (logrank test) p<0.0001 chisq=4053
survdiff(formula=Surv(day,dead)~Location, data=C14)
#Chisq= 675  on 1 degrees of freedom, p= <2e-16 
surv_pvalue(sfC14) 
anova(coxC14)
#p<0.0001
cox.zph(coxC14)
plot(cox.zph(coxC14)) 
ggforest(coxC14, data=C14)
#sig dfference between locations except CI5 and CI20
#CI20 reference
#CI5, p=0.169, HR=0.97
#DB=0.001, HR=0.46
#PW=0.001, HR=0.59




##C20############< means 20C
C20<-subset(OlyLarvaeKMforR, Treatment=="20C")

#make surv object
survC20 <-Surv(time = C20$day, C20$dead, type = "right")

# fit model and plot without random effect
sfC20 <- survfit(survC20 ~ Location, data = C20)
summary(coxph(survC20 ~ Location, data = C20))
ggsurvplot(sfC20, conf.int = TRUE)

#plot Km curves
#darker color=C20, lighter color=20c
ggsurvplot(sfC20, data=C20, conf.int=T,  risk.table=F, pval=F,legend=c("right"),
           legend.labs=c("CI20","CI5", "DB","PW"),legend.title="Location", 
           palette =  c('#33CC66','steelblue','red','darkgoldenrod1'),   
           risk.table.height=.25,xlab="Time (days)", size=0.7, break.time.by = 3, break.y.by=.2, ggtheme = theme_bw() +  theme(
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),  
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank()
           ))


#COX
#cox model sample code with interaction, this is the model for the main analysis
coxC20<-coxph(survC20~Location, data=C20)
coxC20##<here results!!!, p<0.0001
summary(coxC20)
#Concordance= 0.575  (se = 0.003 )
#Likelihood ratio test= 696.8  on 1 df,   p=<2e-16
#Wald test            = 687.9  on 1 df,   p=<2e-16
#Score (logrank) test = 708  on 1 df,   p=<2e-16
#Test no parametric (logrank test) p<0.0001 chisq=4053
survdiff(formula=Surv(day,dead)~Location, data=C20)
#Chisq= 675  on 1 degrees of freedom, p= <2e-16 
surv_pvalue(sfC20) 
anova(coxC20)
#p<0.0001
cox.zph(coxC20)
plot(cox.zph(coxC20)) 
ggforest(coxC20, data=C20)
#sig dfference between temps at C20 p<0.001 HR=0.54

#################Changing refs********************

CI5ref<-subset(OlyLarvaeKMforR, Location==c("CI5", "PW", "DB"))
View(CI5ref)
survCI5ref <-Surv(time = CI5ref$day, CI5ref$dead, type = "right")
sfCI5ref <- survfit(survCI5ref ~ Treatment +Location, data = CI5ref)
ggsurvplot(sfCI5ref, conf.int = TRUE)
coxCI5ref<-coxph(survCI5ref ~ Treatment * Location, data = CI5ref)
coxCI5ref
summary(coxCI5ref)
ggforest(coxCI5ref, data=CI5ref)


DB5ref<-subset(OlyLarvaeKMforR, Location==c("CI20", "CI5"))
View(DB5ref)
survDB5ref <-Surv(time = DB5ref$day, DB5ref$dead, type = "right")
sfDB5ref <- survfit(survDB5ref ~ Treatment +Location, data = DB5ref)
ggsurvplot(sfDB5ref, conf.int = TRUE)
coxDB5ref<-coxph(survDB5ref ~ Treatment * Location, data = DB5ref)
coxDB5ref
summary(coxDB5ref)
ggforest(coxDB5ref, data=DB5ref)


PW5ref<-subset(OlyLarvaeKMforR, TreatmentLocation==c("20CPW", "20CDB"))
View(PW5ref)
survPW5ref <-Surv(time = PW5ref$day, PW5ref$dead, type = "right")
sfPW5ref <- survfit(survPW5ref ~ TreatmentLocation, data = PW5ref)
ggsurvplot(sfPW5ref, conf.int = TRUE)
coxPW5ref<-coxph(survPW5ref ~ TreatmentLocation, data = PW5ref)
coxPW5ref
summary(coxPW5ref)
ggforest(coxPW5ref, data=PW5ref)


LocationOnlyref<-subset(OlyLarvaeKMforR)
View(LocationOnlyref)
survLocationOnlyref <-Surv(time = LocationOnlyref$day, LocationOnlyref$dead, type = "right")
sfLocationOnlyref <- survfit(survLocationOnlyref ~ Location, data = LocationOnlyref)
ggsurvplot(sfLocationOnlyref, conf.int = TRUE)
coxLocationOnlyref<-coxph(survLocationOnlyref ~ Location, data = LocationOnlyref)
coxLocationOnlyref
summary(coxLocationOnlyref)
ggforest(coxLocationOnlyref, data=LocationOnlyref)


CI5LocationOnlyref<-subset(OlyLarvaeKMforR, Location==c("CI5","PW", "DB"))
View(CI5LocationOnlyref)
survCI5LocationOnlyref <-Surv(time = CI5LocationOnlyref$day, CI5LocationOnlyref$dead, type = "right")
sfCI5LocationOnlyref <- survfit(survCI5LocationOnlyref ~ Location, data = CI5LocationOnlyref)
ggsurvplot(sfCI5LocationOnlyref, conf.int = TRUE)
coxCI5LocationOnlyref<-coxph(survCI5LocationOnlyref ~ Location, data = CI5LocationOnlyref)
coxCI5LocationOnlyref
summary(coxCI5LocationOnlyref)
ggforest(coxCI5LocationOnlyref, data=CI5LocationOnlyref)


DB5LocationOnlyref<-subset(OlyLarvaeKMforR, Location==c("PW", "DB"))
View(DB5LocationOnlyref)
survDB5LocationOnlyref <-Surv(time = DB5LocationOnlyref$day, DB5LocationOnlyref$dead, type = "right")
sfDB5LocationOnlyref <- survfit(survDB5LocationOnlyref ~ Location, data = DB5LocationOnlyref)
ggsurvplot(sfDB5LocationOnlyref, conf.int = TRUE)
coxDB5LocationOnlyref<-coxph(survDB5LocationOnlyref ~ Location, data = DB5LocationOnlyref)
coxDB5LocationOnlyref
summary(coxDB5LocationOnlyref)
ggforest(coxDB5LocationOnlyref, data=DB5LocationOnlyref)

#original no subset
survOlyLarvaeKMforR  <-Surv(time = OlyLarvaeKMforR $day, OlyLarvaeKMforR $dead, type = "right")
sfOlyLarvaeKMforR  <- survfit(survOlyLarvaeKMforR  ~ Treatment +Location, data = OlyLarvaeKMforR )
ggsurvplot(sfOlyLarvaeKMforR , conf.int = TRUE)
coxOlyLarvaeKMforR <-coxph(survOlyLarvaeKMforR  ~ Treatment * Location, data = OlyLarvaeKMforR )
coxOlyLarvaeKMforR 
summary(coxOlyLarvaeKMforR )
ggforest(coxOlyLarvaeKMforR , data=OlyLarvaeKMforR )



DB520ref<-subset(OlyLarvaeKMforR, TreatmentLocation==c("20CPW", "20CCI5", "20CDB"))
View(DB520ref)
survDB520ref <-Surv(time = DB520ref$day, DB520ref$dead, type = "right")
sfDB520ref <- survfit(survDB520ref ~ TreatmentLocation, data = DB520ref)
ggsurvplot(sfDB520ref, conf.int = TRUE)
coxDB520ref<-coxph(survDB520ref ~ TreatmentLocation, data = DB520ref)
coxDB520ref
summary(coxDB520ref)
ggforest(coxDB520ref, data=DB520ref)


DB520ref<-subset(OlyLarvaeKMforR, TreatmentLocation==c("20CPW", "20CCI5"))
survDB520ref <-Surv(time = DB520ref$day, DB520ref$dead, type = "right")
sfDB520ref <- survfit(survDB520ref ~ Treatment + Location, data = DB520ref)
ggsurvplot(sfDB520ref, conf.int = TRUE)
coxDB520ref<-coxph(survDB520ref ~ TreatmentLocation, data = DB520ref)
coxDB520ref
summary(coxDB520ref)
ggforest(coxDB520ref, data=DB520ref)

DB520ref<-subset(OlyLarvaeKMforR, TreatmentLocation==c("20CDB", "14CPW"))
survDB520ref <-Surv(time = DB520ref$day, DB520ref$dead, type = "right")
sfDB520ref <- survfit(survDB520ref ~ Treatment + Location, data = DB520ref)
ggsurvplot(sfDB520ref, conf.int = TRUE)
coxDB520ref<-coxph(survDB520ref ~ TreatmentLocation, data = DB520ref)
coxDB520ref
summary(coxDB520ref)
ggforest(coxDB520ref, data=DB520ref)

DB520ref<-subset(OlyLarvaeKMforR, TreatmentLocation==c("20CDB", "20CCI5"))
survDB520ref <-Surv(time = DB520ref$day, DB520ref$dead, type = "right")
sfDB520ref <- survfit(survDB520ref ~ Treatment + Location, data = DB520ref)
ggsurvplot(sfDB520ref, conf.int = TRUE)
coxDB520ref<-coxph(survDB520ref ~ TreatmentLocation, data = DB520ref)
coxDB520ref
summary(coxDB520ref)
ggforest(coxDB520ref, data=DB520ref)


DB520ref<-subset(OlyLarvaeKMforR, TreatmentLocation==c("20CPW", "20CCI5"))
survDB520ref <-Surv(time = DB520ref$day, DB520ref$dead, type = "right")
sfDB520ref <- survfit(survDB520ref ~ Treatment + Location, data = DB520ref)
ggsurvplot(sfDB520ref, conf.int = TRUE)
coxDB520ref<-coxph(survDB520ref ~ TreatmentLocation, data = DB520ref)
coxDB520ref
summary(coxDB520ref)
ggforest(coxDB520ref, data=DB520ref)

DB520ref<-subset(OlyLarvaeKMforR, TreatmentLocation==c("14CPW", "20CCI5"))
survDB520ref <-Surv(time = DB520ref$day, DB520ref$dead, type = "right")
sfDB520ref <- survfit(survDB520ref ~ Treatment + Location, data = DB520ref)
ggsurvplot(sfDB520ref, conf.int = TRUE)
coxDB520ref<-coxph(survDB520ref ~ TreatmentLocation, data = DB520ref)
coxDB520ref
summary(coxDB520ref)
ggforest(coxDB520ref, data=DB520ref)

DB520ref<-subset(OlyLarvaeKMforR, Location==c("DB","CI5","PW"))
survDB520ref <-Surv(time = DB520ref$day, DB520ref$dead, type = "right")
sfDB520ref <- survfit(survDB520ref ~ Location, data = DB520ref)
ggsurvplot(sfDB520ref, conf.int = TRUE)
coxDB520ref<-coxph(survDB520ref ~ Location, data = DB520ref)
coxDB520ref
summary(coxDB520ref)
ggforest(coxDB520ref, data=DB520ref)
