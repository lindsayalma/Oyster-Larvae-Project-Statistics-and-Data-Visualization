# Alma oyster larvae experiment
# likelihood of larvae in jar

#library
library(tidyverse)
library(here)
library(readxl)
library(dplyr)
library(broom)
library(survival)
library(survminer)
library(ggplot2)
library(rms)
library(survELtest)

.libPaths()

#set working directory
setwd("C:/Users/Lindsay/Dropbox/Raccoon/larvae/oyster/larvae survival stats/GitHub/RaccoonOlyLarvalSurvival2")

#jar and sample volumes (ml)
vol_jar <- 100 
vol_sample <- 6 

#fraction of water sampled
# this is the probably of successfully get a larva from the jar 
# in a binomial process
p <- vol_sample / vol_jar
#p=0.06

# vector of the potential actual number of larvae in a jar
# this is the number of "trials" or chances of sampling a larvae 
# in a binomial process

# create at data frame of sample probabilities
# the like_of_sample is the probably of obtaining the sample count 
# given the actual number of larvae.
# Thus, the probability is the likelihood of the sample given the jar total

# maximum number if larvae ever counted in a 6ml sample (can change this to the actual value)
max_count <- 71
# total number of larvae you think there could possibly be in a jar 
# (since target initial abundance is 800, 2000 should be plenty 
#no penalty for guessing big except a waste of computer time
max_possible_larvae <- 2000
#data frame of the 
# prob_count_given_jar is the likelihood of getting a particular count value given some actual number of larva in jar
#prob_jar_given_count is the probably of how many larve are in the jar given a count value (bayes therom assuming uniform prior)
#cummulative_prob is useful for getting random samples
d_jar_dist <- data.frame(sample_count = rep(0:max_count, each = max_possible_larvae + 1)) %>%
  mutate(jar_actual_larvae = rep(0:max_possible_larvae, times = max_count + 1)) %>%
  mutate(prob_count_given_jar = pbinom(sample_count, jar_actual_larvae, p) - 
           pbinom(sample_count-1, jar_actual_larvae, p)) %>%
  group_by(sample_count) %>%
  mutate(prob_jar_given_count = prob_count_given_jar / sum(prob_count_given_jar)) %>%
  mutate(cummulative_prob = cumsum(prob_jar_given_count)) %>%
  ungroup() %>%
  {.}
#View(d_jar_dist)

#plot of the prob distribuion
d_jar_dist %>%
  #filter(sample_count == 0) %>%
  ggplot(aes(jar_actual_larvae, prob_jar_given_count)) +
  geom_step(aes(colour = as.factor(sample_count))) +
  labs(colour = "Sample count") +
  xlab("Larvae in jar") +
  ylab("Probability of jar given count") +
  #xlim(0,100) +
  theme_bw()

#plot of the cumulative prob
d_jar_dist %>%
  #filter(sample_count == 0) %>%
  ggplot(aes(jar_actual_larvae, cummulative_prob)) +
  geom_step(aes(colour = as.factor(sample_count))) +
  labs(colour = "Sample count") +
  xlab("Larvae in jar") +
  ylab("Cummulative probability") +
  #xlim(0,100) +
  theme_bw()

#function that returns a vector (length = n) of random number of larvae in a jar given 6ml subsample count
r_jar <- function(n, count, dist){
  rj <- NULL
  for(i in 1:n){
    r = runif(1)
    rj_temp <- dist %>%
      filter(sample_count == count &
               cummulative_prob >= r &
               lag(cummulative_prob) < r ) %>%
      pull(jar_actual_larvae) %>%
      {.}
    
    if(is_empty(rj_temp)){
      rj_temp <- 0
    }
    rj <- c(rj, rj_temp)
  }
  
  return(rj)
}


#quick look at output of the r_jar function
# sampled distributions if 6ml sample contained 0, 10 or 30 larvae
data.frame(r0 = r_jar(100, 0, d_jar_dist), 
           r10 = r_jar(100, 10, d_jar_dist),
           r30 = r_jar(100, 30, d_jar_dist)) %>%
  pivot_longer(cols = starts_with("r"),names_to = "sample_count", values_to = "jar_count") %>%
  ggplot(aes(jar_count)) +
  geom_histogram() +
  facet_wrap(vars(sample_count), scales = "free")


#read in the real data
# count data for 6ml samples 
#d_count <- read_excel("data/real data.xlsx") %>%
d_count <- read_excel(here("data","real data.xlsx")) %>%
  arrange(jar_id, day) %>%
  group_by(jar_id) %>%
  mutate(delta_count = count - lag(count)) %>%
  ungroup()
#vector of unique jar IDs = 48
all_jars <- unique(d_count$jar_id)


#find problem jars with large increase in count from one time step to the next
problem_jars <- d_count %>%
  filter(delta_count >= 10 ) %>%
  pull(jar_id) %>%
  unique() %>%
  {.}

good_jars <- all_jars[!(all_jars %in% problem_jars)]


#problem jars=28 31 32 33 34 35 45 48
#Jar 28- real variation. Had some samples with 9, 9, 9, 11, 14 larvae total per sample with a high number of live in each
#Jar 31- had a single 1 ml sample where 18 were alive on day 10 which brought the count up
#Jar 32- typo, had one jar with 22 instead of 2
#Jar 33- On day 3 there were a particurlay large number of larvae per ml: 18,15,15,9. Maybe interns didnt mix correctly?
#Jar 34- Not anything to explain here, thats just how the counts came out
#Jar 35- On day 12 there were a particurlay large number of larvae per ml. Maybe interns didnt mix correctly?
#Jar 45- On day 1 got 2 1 ml samples with only 1 or 2 larvae, brought count down.
#Jar 48- Not anything to explain here, thats just how the counts came out





#Create simulated jar sample time series
# n_rep = number of replicate series to create
# This should be >=500 but 5 is engouhg to show how it works
n_rep <- 5
# empty frame to hold result
d_sim <- NULL
#pick which jars to run
#run_jars <- jar
run_jars <- problem_jars
#run_jars <- good_jars
#loop through all the jars picked
#time the loop
start_time <- Sys.time()
for(i in 1:length(run_jars)){
  #print jar to track progress
  print(paste("jar",run_jars[i]))
  #use data from one jar
  d_jar <- d_count %>%
    filter(jar_id == run_jars[i])
  #vector of raw jar sample count
  count <- d_jar$count
  n_count <- length(count)
  # vector the same lenght as count to hold random draw data 
  #(this gets over-written)
  r_count <- count
  # initialize rep counter to track number of simulated series
  rep_counter <- 1
  # while to so keep repeating until you get enough decreasing time series
  while(rep_counter <= n_rep) {
    #create a simulated jar counter time series
    for(j in 1:n_count){
      r_count[j] <- r_jar(1, count[j], d_jar_dist)
    }
    #test whether the series is monotonically decreasing
    is_valid <-  all(if_else(r_count <= lag(r_count), TRUE, FALSE), na.rm = TRUE)
    # if decreasing, add the result to the data frame and increment rep counter
    if(is_valid){
      d_temp <- data.frame(day = d_jar$day,
                           raw_count = count, 
                           sim_count = r_count) %>%
        mutate(jar_id = run_jars[i],
               rep_id = rep_counter,
               treatment = d_jar$treatment[1],
               site = d_jar$treatment[1])
      d_sim <- rbind(d_sim, d_temp)
      #print rep counter to track progress
      print(paste("rep_counter", rep_counter))
      rep_counter = rep_counter + 1
    }
  }
}
end_time <- Sys.time()
end_time - start_time

# plot the first 5 simulated series for all jars
d_sim %>%
  filter(rep_id <= 5) %>%
  ggplot(aes(day, sim_count)) +
  geom_line(aes(colour = as.factor(rep_id))) +
  facet_wrap(vars(jar_id))

#write simulated series to file
d_sim %>%
  arrange(jar_id, re, day) %>%
  write_csv(here("output", "d_sim.csv"))


# plot the first 5 simulated series for all jars
d_sim %>%
  filter(rep_id <= 5) %>%
  ggplot(aes(day, sim_count)) +
  geom_line(aes(colour = as.factor(rep_id))) +
  facet_wrap(vars(jar_id))

#write simulated series to file
d_sim %>%
  arrange(jar_id, rep_id, day) %>%
<<<<<<< HEAD
  write_csv(here("output", "d_sim_pj_200.csv"))
=======
  write_csv(here("output", "d_sim_pj_300.csv"))

  #number of days counted
  #CI20-5
  #CI5- 6
  #DB-7
  #PW-6
  


  


#Number of days we counted per site/treatment
#CI20- (5 days) 1,4,7,10,14
#CI5- (6 days) 1,3,6,9,11,14
#DB- (7 days) 1,3,5,7,10,12,14
#PW- (6 days) 1,4,7,10,12,14
#=24 days*48 jars*500=576,000
  
#number of days counted
#CI20-5
#CI5- 6
#DB-7
#PW-6

  
##### Paul example code to format for cox
# expansion of jar total counts to individual for survival analysis

# read in simulated data
#example is just on one jar - need to loop through all reps of all the jars

d <- read_csv(here("output", "d_sim_pj_300.csv")) %>%
    filter(jar_id == 31, rep_id == 1) %>%
    arrange(jar_id, rep_id, day)
View(d)

#create a data frame for the expanded data
#populate day and status with default assumptoin that they are alive at the last sample day
d_expand <- data.frame(jar_id =rep(d$jar_id[1], d$sim_count[1])) %>%
    mutate(rep_id = d$rep_id[1], 
           treatment = d$treatment[1], 
           site = d$site[1], 
           day = d$day[nrow(d)],
           status = 0)
  
View(d_expand)

#loop through all the days, assigning status = 1 and day to the right number of individuals
# dead_index and new_dead help make sure you change the correct row for status and day
# any thing that didn't die remains right censored (status = 0 on last day)
dead_index <- 1
for(i in 2:nrow(d)){
    new_dead <- d$sim_count[i-1] - d$sim_count[i]
    d_expand$day[dead_index:(dead_index+new_dead)] <- d$day[i]
    d_expand$status[dead_index:(dead_index+new_dead)] <- 1
    dead_index <- dead_index + new_dead
  }
  
View(dead_index)
View(new_dead)
# use a frequency table to check that the example worked.
# this shows that the last count is off by one.
# I'll leave it to you to fix that...
lag(d$sim_count) - d$sim_count  
table(d_expand$day, d_expand$status)

write.csv(d_expand, file = "output/d_expand.csv")

#Notes
#282 0s in the dataset, #551 1s in the dataset =833
#the table shows that on day 14 we had 282 left, and 23 died, but its supposed to be 283 left and 22 died - so somehow, we need to change one 1 into a 0
#we also want to change the day of all those whos status == 0 from 14 to 15
#from there we want to run the code on the whole dataset - 48 jars and 500 reps each



##################################################################################
##Format for cox model/48 jars, 4 sites, 2 temperatures, day counts, 500 reps
# read in simulated data
#example is just on one jar - need to loop through all reps of all the jars

database <- read_excel(here("data","d_sim_all.xlsx"))
View(database)

#create a data frame for the expanded data
#populate day and status with default assumption that they are alive at the last sample day

numreps <- as.numeric(max(database$rep_id)) #number of replicates=500
numjars <- as.numeric(max(database$jar_id)) # number of jars=48

dead_index <- 1 #everyone is alive at the beginning. status=1

#create an empty list to store each category's response in
replist <- vector(mode = "list", length = length(numreps * numjars))

for (i in 1:numjars) { #for each of the 48 jars
  #i is basically jar number
  jar <- filter(database, jar_id == i) #filter out the ith jar
  
  for(j in 1:numreps) { #for each of the 500 reps in each jar
    #j is the replicate it's on
    count <- filter(jar, rep_id == j)
    d_expand <- data.frame(jar_id = rep(count$jar_id[1], times = count$sim_count[1]))  %>% #create a data frame of that jar
      #the data frame should be the length of the number of simulated larvae on day 1 aka first line of the database
      mutate(rep_id = count$rep_id[1],  #create the number of rows that corresponds to the 
             #number of simulated larvae on day 1
             treatment = count$treatment[1], 
             site = count$site[1], 
             day = count$day[nrow(count)],
             status = 0)
    
    for(z in 2:(nrow(count))) { #days
      new_dead <- count$sim_count[z-1] - count$sim_count[z] #get number that died on this new timepoint
      d_expand$day[dead_index : nrow(d_expand)] <- count$day[z] #repeat that timepoint the number 
      #of times of new_dead i.e. the number that died that day
      d_expand$status[dead_index : nrow(d_expand)] <- 1 #change those numbers to 1
      dead_index <- dead_index + new_dead #adding new number of dead larvae to the current tally 
      #from the previous day
    }
    d_expand$status[nrow(d_expand) : dead_index] <- 0 #put the correct number of 0's at the bottom
    dead_index <- 1 #reset this to 1 between replicates
    replist[[(((i - 1) * 500) + j)]] <- d_expand #put this d_expand dataframe into the i*jth spot on the list
  }
}


bigcox <- bind_rows(replist)
write.csv(bigcox, file = "bigcox.csv")
write.csv(here(bigcox,"output","bigcox.csv"))

#######################################################################################
#separate each of the 500 datasets into separate df and run cox model
split <- split(bigcox, f = bigcox$rep_id)  
split

KMsurv = Surv(time = bigcox$day, bigcox$status, type = "right")#Kalp1n meier
sf <- survfit(KMsurv ~ treatment+site, data = bigcox)
ggsurvplot(sf, conf.int = 0.05) #simple plot

ggsurvplot(sf, data=bigcox, conf.int=T,  risk.table=F, pval=F,legend=c("right"),
           legend.labs=c("CI20-14C","CI5-14C","DB-14C","PW-14C", "CI20-20C", "CI5-20C", "DB-20C", "PW-20C"),legend.title="Treatment", 
           palette =  c( "#04c390", "#0451a4","#ec844c","#8c64d4", "#336633","#002043", "darkorange4", "#660099"),   
           risk.table.height=.25,xlab="Time (days)", size=0.7, break.time.by = 3, break.y.by=.2, ggtheme = theme_bw() +  theme(
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),  
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank()
           ))
################## PM : Question about the plot. Shouldnt we have much fatter looking confience intervals given we have all the replicates?
head(bigcox)

#Just 1 rep cox output
R1<-subset(bigcox, rep_id=="1")
KMsurv = Surv(time = R1$day, R1$status, type = "right")
sf <- survfit(KMsurv ~ site + treatment, data = R1)
coxR1<-coxph(KMsurv~treatment*site, data=R1)
S1<-summary(coxR1)
S1
out1 = tidy(coxR1)
out1
write.csv(here("out1.csv"))

#writing the for loop model
database<-bigcox
head(database)
numreps <- as.numeric(max(database$rep_id)) #number of replicates=500 single number
k <- filter(database, database$rep_id == 1)#only 1 out of 500 reps

############################CI20

#make the data for graphing
sv = Surv(time = database$day, database$status, type = "right") #running Surv model on the replicate,
#taking into account day and status
sf <- survfit(sv ~ site + treatment + rep_id, data = database) #taking what we just made and 
#putting it into a different function
#graphing it
ggsurvplot(sf, group.by = c(treatment, site), conf.int = 0.05) 
#figure out a way to manually define confidence intervals based on the different replicates


#create an empty list to store all of the replicate cox outputs
cox <- vector(mode = "list", length = numreps)
coxcoef <- vector(mode = "list", length = numreps)

numreps <- 500

#run the cox model in a for loop
for(i in 1:numreps) { #for each of the 500 reps in each jar-- i is the replicate it's on
  rep <- subset(database, rep_id == i)#filter 1 rep at a time
  coxoutput <- coxph(Surv(day, status) ~ treatment + site + treatment:site, data = rep)
  cox[[i]] <- coxoutput #store the whole output of the model in a list
  blah <- data.frame( #create a data frame with the coefficients and the replicate
    coefficients = coxoutput$coefficients,
    replicate = i,
    pval = summary(coxoutput)$coefficients[,5])
  blah$treatment_site <- rownames(blah) #in that dataframe, create a new column that has the treatment and site
  #info in it
  coxcoef[[i]] <- blah #assign the dataframe to the correct spot in the list
}

#combine all coefficients and p-value into a single dataframe
coefficients_ci20 <- bind_rows(coxcoef)#CI20 as the control
#head(coefficients_ci20)
write.csv(coefficients_ci20, file = "coefficients_ci20.csv")

#run model with other site as refs



#Just 1 rep cox output
survPW5ref <-Surv(time = PW5ref$day, PW5ref$status, type = "right")
sfPW5ref <- survfit(survPW5ref ~ treatment +site, data = PW5ref)
ggsurvplot(sfPW5ref, conf.int = TRUE)
coxPW5ref<-coxph(survPW5ref ~ treatment * site, data = PW5ref)
coxPW5ref
ggforest(coxPW5ref, data=PW5ref)

##################PW only

#make the data for graphing
sv = Surv(time = database$day, database$status, type = "right") #running Surv model on the replicate,
#taking into account day and status
sf <- survfit(sv ~ site + treatment + rep_id, data = database) #taking what we just made and 
#putting it into a different function
#graphing it
ggsurvplot(sf, group.by = c(treatment, site), conf.int = 0.05) 

#run the cox model in a for loop
for(i in 1:numreps) { #for each of the 500 reps in each jar-- i is the replicate it's on
  rep <- subset(PW5ref, rep_id == i)#filter 1 rep at a time
  coxoutput <- coxph(Surv(day, status) ~ treatment * site, data = rep)
  cox[[i]] <- coxoutput #store the whole output of the model in a list
  blah <- data.frame( #create a data frame with the coefficients and the replicate
    coefficients = coxoutput$coefficients,
    replicate = i,
    pval = summary(coxoutput)$coefficients[,5])
  blah$treatment_site <- rownames(blah) #in that dataframe, create a new column that has the treatment and site
  #info in it
  coxcoef[[i]] <- blah #assign the dataframe to the correct spot in the list
}
#combine all coefficients and p-value into a single dataframe
coefficients_pw5 <- bind_rows(coxcoef)#pw as the control
head(coefficients_pw5)

write.csv(coefficients_pw5, file = "coefficients_pw5.csv")


#################DB only
#DB ref, CI 20 and 5 deleted
DB5ref = filter(bigcox, !(site %in% c("CI5","CI20")))
nrow(DB5ref)

#DB only
#run the cox model in a for loop
for(i in 1:numreps) { #for each of the 500 reps in each jar-- i is the replicate it's on
  rep <- subset(DB5ref, rep_id == i)#filter 1 rep at a time
  coxoutput <- coxph(Surv(day, status) ~ treatment * site, data = rep)
  cox[[i]] <- coxoutput #store the whole output of the model in a list
  blah <- data.frame( #create a data frame with the coefficients and the replicate
    coefficients = coxoutput$coefficients,
    replicate = i,
    pval = summary(coxoutput)$coefficients[,5])
  blah$treatment_site <- rownames(blah) #in that dataframe, create a new column that has the treatment and site
  #info in it
  coxcoef[[i]] <- blah #assign the dataframe to the correct spot in the list
}
#combine all coefficients and p-value into a single dataframe
coefficients_DB5 <- bind_rows(coxcoef)#DB as the control
head(coefficients_DB5)

write.csv(coefficients_DB5, file = "coefficients_DB5.csv")





#########################PW14 ref, all other delted
PW5ref = filter(bigcox, !(site %in% c("CI5","DB","CI20")))
write.csv(PW5ref, file = "PW5ref.csv")
head(PW5ref)


#make the data for graphing
sv = Surv(time = PW5ref$day, PW5ref$status, type = "right") #running Surv model on the replicate,
#taking into account day and status
sf <- survfit(sv ~ site + treatment + rep_id, data = PW5ref) #taking what we just made and 
#putting it into a different function
#graphing it- takes al ong time
#ggsurvplot(sf, group.by = c(treatment, site), conf.int = 0.05) 
#figure out a way to manually define confidence intervals based on the different replicates


#create an empty list to store all of the replicate cox outputs
numreps <- 500
cox <- vector(mode = "list", length = numreps)
coxcoef <- vector(mode = "list", length = numreps)


#run the cox model in a for loop
for(i in 1:numreps) { #for each of the 500 reps in each jar-- i is the replicate it's on
  rep <- subset(PW5ref, rep_id == i)#filter 1 rep at a time
  coxoutput <- coxph(Surv(day, status) ~ treatment, data = rep)
  cox[[i]] <- coxoutput #store the whole output of the model in a list
  blah <- data.frame( #create a data frame with the coefficients and the replicate
    coefficients = coxoutput$coefficients,
    replicate = i,
    pval = summary(coxoutput)$coefficients[,5])
  blah$treatment_site <- rownames(blah) #in that dataframe, create a new column that has the treatment and site
  #info in it
  coxcoef[[i]] <- blah #assign the dataframe to the correct spot in the list
}

#combine all coefficients and p-value into a single dataframe
coefficients_PW5ref <- bind_rows(coxcoef)#DB as the control
#head(coefficients_PW5ref)
write.csv(coefficients_PW5ref, file = "coefficients_PW5ref.csv")


#############CI5 ref
#PW14 ref, all other delted
CI5ref = filter(bigcox, !(site %in% c("CI20")))
write.csv(CI5ref, file = "CI5ref.csv")
head(CI5ref)
gc()

#make the data for graphing
sv = Surv(time = DB5ref$day, DB5ref$status, type = "right") #running Surv model on the replicate,
#taking into account day and status
sf <- survfit(sv ~ site + treatment + rep_id, data = DB5ref) #taking what we just made and 
#putting it into a different function
#graphing it- takes al ong time
#ggsurvplot(sf, group.by = c(treatment, site), conf.int = 0.05) 
#figure out a way to manually define confidence intervals based on the different replicates


#create an empty list to store all of the replicate cox outputs
numreps <- 500
cox <- vector(mode = "list", length = numreps)
coxcoef <- vector(mode = "list", length = numreps)


#run the cox model in a for loop
for(i in 1:numreps) { #for each of the 500 reps in each jar-- i is the replicate it's on
  rep <- subset(CI5ref, rep_id == i)#filter 1 rep at a time
  coxoutput <- coxph(Surv(day, status) ~ treatment * site, data = rep)
  cox[[i]] <- coxoutput #store the whole output of the model in a list
  blah <- data.frame( #create a data frame with the coefficients and the replicate
    coefficients = coxoutput$coefficients,
    replicate = i,
    pval = summary(coxoutput)$coefficients[,5])
  blah$treatment_site <- rownames(blah) #in that dataframe, create a new column that has the treatment and site
  #info in it
  coxcoef[[i]] <- blah #assign the dataframe to the correct spot in the list
}

#combine all coefficients and p-value into a single dataframe
coefficients_CI5ref <- bind_rows(coxcoef)#DB as the control
#head(coefficients_CI5ref)
write.csv(coefficients_CI5ref, file = "coefficients_CI5ref.csv")







#Next steps on this.

# One issue is how to deal with the day 0 jar simumalion. Easiest (and maybe best?) solution is just start 
# your survival analysis on day 1 when you start the 6ml sampling.
#alternarive is to figure out the distibution of initial stocking density based on your stocking method.
#the target intial stocking was 800 per jar, but there is liekly a lot of noise around that based on sampling

# X simulate a 1000 valid (monotoncially decreasing) time series of counts for each jar
# X The above code makes valid series, but need to modify it so you get a 1000 for each jar of the real data
# X treat the jars like you have 1000 replicate experiments
# X expand the jar counts with 0/1 scoring per individual for running cox model like you did before
# x you and run the analysis based on live count or dead count (for live count, subtract dead based on day1 count)
# x run the cox model on each of the 1000 data sets 
# x store the output hazard ratio coefficients() as a vector with 1000 values) 
# x you can actually just save all 1000 model outputs (not just coefficients) in case we want to look at other values
# x sort the coefficients from smallest to largest - the 2.5% and 97.5% quantiles will give the 95% confidence intervals. 
# x the mean is  the expected value of the hazard ratio

#after getting this to work with the simple cox model, can look at coxme or frailty_em to deal with mixed effects


#the reason why the jar sampling is an issue has to do with whether larvae in the jar are distributed randomly or uniformly. 
#If the larvae were uniformly distributed, your 6ml sample would exactly reflect what is in the jar
#However, larvae are (at best) randomly distributed in the jar, so sampling probability matters
#The larger the fraction of the jar you actually count, the less sampling probably matters (uniform assumption gets close enough)

#The actuall confidence intervals will be a bit wider than estimated with this method because of basic uncertainy in estiminating cox model
#however, with n = ~ 800, this should not be a big contribution and the CI should be pretty close.


