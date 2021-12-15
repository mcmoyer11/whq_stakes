# Processing Stakes present and past tense

library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(bootstrap)
library(lme4)
library(tidyverse)

# theme_set(theme_grey())
View(dd_seen)
#Set the working directory to whereever you have your raw data and the "helpers.R" file
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
source("helpers.R")


##############################
# a bit of preprocessing, skip further down for graphs and stats

past <- read.csv("../data/past.csv")
# nrow(past) #1386
sub <- subset(past, past$mention_one_low_seen=="1")
# nrow(sub) #599

present <- read.csv("../data/present.csv")
#nrow(present) #1372
sub2 <- subset(present, present$mention_one_low_seen=="1")
#nrow(sub2) #559

#make a column with past/present as a factor
past$tense <- "past"
present$tense <- "present"
#bind the two data files together
total <- rbind(past, present)
write.csv(total, "../data/finite_total.csv")
# View(total)
nrow(total) #2758


########################################
# here we go
# try importing the non-finite data in to run stats over that 

# to run just the finite data, use the variable call below, and comment out the lines above
total <- read.csv("../data/finite_total.csv")
# View(total)
# length(unique(total$subject))
#197
# length(unique(total$native_lang))

native <- subset(total, total$native!="Albanian")
native1 <- subset(native, native$native!="Romanian")
native2 <- subset(native1, native1$native!="hindi")
native3 <- subset(native2, native2$native!="Italian")
# length(unique(native3$subject)) #193

#to ztart getting stats, scroll to the bottom

subb <- subset(native3, native3$mention_one_low_seen=="1")
# nrow(subb) #1130

#get AVG count of seen and chosen
dseen = native3 %>%
  filter(trial %in% c("test")) %>%
  select(subject,story,trial,exhaustive_seen,mention_one_high_seen,mention_one_low_seen,mention_some_high_seen,mention_some_low_seen,false_report_seen,tense) %>%
  gather(ResponseTypeSeen,Seen,-subject,-story,-trial,-tense) %>%
  mutate(ResponseType=gsub("_seen","",ResponseTypeSeen,fixed=T)) %>%
  select(-ResponseTypeSeen)
#head(dseen)

dchosen = native3 %>%
  filter(trial %in% c("test")) %>%
  select(subject,story,trial,exhaustive_chosen,mention_one_high_chosen,mention_one_low_chosen,mention_some_high_chosen,mention_some_low_chosen,false_report_chosen,tense) %>%
  gather(ResponseTypeChosen,Chosen,-subject,-story,-trial,-trial,-tense) %>%
  mutate(ResponseType=gsub("_chosen","",ResponseTypeChosen,fixed=T)) %>%
  select(-ResponseTypeChosen)





######################################################################
#kruskal.test(as.factor(Chosen) ~ as.factor(ResponseType), data = dchosen)
# Kruskal-Wallis chi-squared = 189.8, df = 4, p-value < 2.2e-16 (on df total)
# Kruskal-Wallis chi-squared = 187.91, df = 4, p-value < 2.2e-16 (on df native3)
kruskal.test(as.factor(Chosen) ~ as.factor(ResponseType), data = dchosen)
# Kruskal-Wallis chi-squared = 563.21, df = 5, p-value < 2.2e-16

kruskal.test(as.factor(Chosen) ~ as.factor(story), data = dchosen)
# Kruskal-Wallis chi-squared = 30.593, df = 5, p-value = 1.127e-05

inter <- interaction(dchosen$ResponseType, dchosen$story)
kruskal.test(as.factor(Chosen)~inter, data = dchosen)
# Kruskal-Wallis chi-squared = 624.8, df = 35, p-value < 2.2e-16
######################################################################

shoes = dchosen %>%
  filter(story %in% c("test_shoes")) %>%
  filter(ResponseType %in% c("exhaustive","mention_one_high","mention_some_high"))

kruskal.test(Chosen ~ ResponseType, data = shoes)
# Kruskal-Wallis chi-squared = 22.239, df = 2, p-value = 1.482e-05

shoesmo = dchosen %>%
  filter(story %in% c("test_shoes")) %>%
  filter(ResponseType %in% c("exhaustive","mention_one_high"))
kruskal.test(Chosen ~ ResponseType, data = shoesmo)
# Kruskal-Wallis chi-squared = 21.679, df = 1, p-value = 3.223e-06
testttt = dseen %>%
  filter(story %in% c("test_shoes")) %>%
  filter(Seen %in% c("1")) %>%
  filter(ResponseType %in% c("mention_some_high"))
length(unique(testttt$subject)) 

# 74 chose moh, 33 chose exh, 62 chose msh
# 120 saw moh, 62 saw exh, 116 saw msh
# moh: 62%; exh: 53%; msh; 53%

shoesms = dchosen %>%
  filter(story %in% c("test_shoes")) %>%
  filter(ResponseType %in% c("exhaustive","mention_some_high"))
kruskal.test(Chosen ~ ResponseType, data = shoesms)
# Kruskal-Wallis chi-squared = 11.712, df = 1, p-value = 0.0006209
length(unique(shoesmo$subject)) #193


#differences per story
ggplot(dchosen %>% filter(Chosen == 1), aes(ResponseType)) +
  geom_histogram(stat="count",position="dodge") +
  coord_flip() +
  facet_wrap(~story)




View(dd)

# add them together
dd = dseen %>%
   left_join(dchosen,by=c("subject","story","trial","ResponseType","tense"))
ddnd = subset(dd, dd$story!="test_dentist")
# View(dd)
dd = subset(dd, dd$ResponseType!="false_report")
agr2 = dd %>%
  group_by(ResponseType,story) %>%
  summarize(propChosen=sum(Chosen)/sum(Seen)) %>%
  # group_by(story) %>%
  # summarize(MeanChosen=mean(propChosen),CILow=ci.low(propChosen),CIHigh=ci.high(propChosen)) %>%
  ungroup()
  # mutate(YMin=MeanChosen-CILow,YMax=MeanChosen+CIHigh)

View(dd_seen)
dd_seen = ddnd %>%
  filter(Seen %in% c("1")) %>%
  separate(ResponseType, c("ResponseType","fill", "Informativity")) %>%
  unite(ResponseType, ResponseType, fill, sep="_")

write.csv(dd_seen, "../data/stakes_finite.csv")
  # revalue(., c("exhaustive_NA"="MA", "mention_some"="MS", "mention_one" = "MO"))
nrow(dd_seen) #2554

kruskal.test(Chosen ~ subject, data = dd_seen)
kruskal.test(Chosen ~ tense, data = dd_seen)
# with dentist
# Kruskal-Wallis chi-squared = 9.4162, df = 1, p-value = 0.002151
# wihtout dentist
# Kruskal-Wallis chi-squared = 6.6174, df = 1, p-value = 0.0101

kruskal.test(Chosen ~ ResponseType, data = dd_seen)
# with dentist
# Kruskal-Wallis chi-squared = 172.34, df = 4, p-value < 2.2e-16
# without dentist
# Kruskal-Wallis chi-squared = 6.5063, df = 2, p-value = 0.03865


pairwise.wilcox.test(dd_seen$Chosen, dd_seen$ResponseType, p.adjust.method = "BH")
#             exhaustive_NA mention_one
#   mention_one  0.730         -          
#   mention_some 0.099         0.067      
# 
# P value adjustment method: BH 


mamsss = dd_seen %>%
  filter(ResponseType %in% c("mention_some", "mention_one"))
wilcox.test(mamsss$Chosen ~ mamsss$ResponseType)
# W = 661842, p-value = 0.02246
kruskal.test(Chosen ~ ResponseType, data = mamsss)
# Kruskal-Wallis chi-squared = 5.2099, df = 1, p-value = 0.02246
t.test(mamsss$Chosen ~ mamsss$ResponseType)
# t = 2.2849, df = 2243.6, p-value = 0.02241


# BUT WHY DOES prop.test GIVE DIFFERENT RESULTS? I'M GUESSING
# IT HAS SOMETHING TO DO WITH THE VARIANCE
View(mamsss)

m <- glmer(Chosen ~ ResponseType*Informativity + (1|subject), family ="binomial", data = dd_seen)
summary(m)




# Very very helpful discussion below: 
# https://stats.stackexchange.com/questions/86351/interpretation-of-rs-output-for-binomial-regression
# The estimate is the amount by which the log odds of the Dependent Varible 
# would increase if the predictor (covariate) variable were one unit higher. 

# The log odds of Chosen when predictor is 0 is the intercept.

wilcox.test(dd_seen$Chosen ~ dd_seen$Informativity)
# W = 798498, p-value < 2.2e-16
kruskal.test(Chosen ~ Informativity, data = dd_seen)
# Kruskal-Wallis chi-squared = 157.43, df = 1, p-value < 2.2e-16 (mamsss data)
# Kruskal-Wallis chi-squared = 157.43, df = 1, p-value < 2.2e-16 (dd_seen data)

interRI = interaction(dd_seen$ResponseType, dd_seen$Informativity)
# there is an interaction between ResponseType and Informativity
# It doesn't matter which data set you use
kruskal.test(Chosen ~ interRI, data = dd_seen)
# Kruskal-Wallis chi-squared = 170.44, df = 3, p-value < 2.2e-16 (mamsss data set)
# Kruskal-Wallis chi-squared = 170.44, df = 3, p-value < 2.2e-16 (dd_seen data set)



pairwise.wilcox.test(dd_seen$Chosen, dd_seen$ResponseType, p.adjust.method = "BH")
# (Without dentist)
  #             exhaustive_NA mention_one
  # mention_one  0.730         -          
  # mention_some 0.099         0.067 

kruskal.test(Chosen ~ story, data = dd_seen)
# with dentist
# Kruskal-Wallis chi-squared = 39.613, df = 5, p-value = 1.788e-07
# without dentist
# Kruskal-Wallis chi-squared = 5.971, df = 4, p-value = 0.2013
pairwise.wilcox.test(dd_seen$Chosen, dd_seen$story, p.adjust.method = "BH")

# THIS IS THE BEST GRAPH!
ggplot(agr2, aes(ResponseType,y=propChosen)) +
  geom_bar(position=dodge,stat="identity") +
  # geom_boxplot() +
  facet_wrap(~story) +
  ylim(0,1)

ggplot(agr2, aes(ResponseType,y=propChosen)) +
  geom_bar(position=dodge,stat="identity") +
  # facet_wrap(~story) +
  ylim(0,1)

######################################################################
agrr = dd %>%
  group_by(story,ResponseType,Chosen) %>%
  summarize(SumChosen=sum(Chosen)) %>%
  group_by(ResponseType)

seenagr = dd %>%
  drop_na() %>%
  group_by(story,ResponseType,Seen) %>%
  summarize(SumSeen=sum(Seen)) %>%
  group_by(ResponseType)

new <- agrr %>% 
  left_join(seenagr) %>%
  drop_na()

nowr <- subset(new, new$ResponseType!="none_of_above")
now <- subset(nowr, nowr$ResponseType!="false_report")

now_agr = now %>%
  filter(., Chosen!=0, Seen!=0) %>%
  mutate(., proportion = SumChosen / SumSeen)

# View(now_agr)
res = now_agr %>%
  group_by(ResponseType) %>%
  summarise(SumChosen=sum(SumChosen),SumSeen=sum(SumSeen))

# Tests of proportion across entire sample

# MA vs MO-max with dentist
prop.test(c(176,425),c(362,677))
# X-squared = 18.814, df = 1, p-value = 1.441e-05 ***
# without dewntist
prop.test(c(156,377),c(306,570))
# X-squared = 18.577, df = 1, p-value = 1.632e-05 ***

# MA vs MS-max with dentist
prop.test(c(176,334),c(362,647))
# X-squared = 0.72216, df = 1, p-value = 0.3954
# without dewntist
prop.test(c(156,297),c(306,536))
# X-squared = 1.365, df = 1, p-value = 0.2427

# MO-max vs MS-max with dentist
prop.test(c(425,334),c(677,647))
# X-squared = 16.372, df = 1, p-value = 5.204e-05 ***
# without dewntist
prop.test(c(377,297),c(570,536))
# X-squared = 12.914, df = 1, p-value = 0.0003261 ***

# MA vs MO-min with dentist
prop.test(c(176,225),c(362,680))
# X-squared = 23.417, df = 1, p-value = 1.304e-06 ***

# MA vs MS-min with dentist
prop.test(c(176,235),c(362,693))
# X-squared = 21.018, df = 1, p-value = 4.551e-06 ***

View(now_agr)
######################################################################
# story effects

books = now_agr %>%
  filter(story %in% c("test_books"))
View(books)
# MO-max vs MS-max
prop.test(c(74,61),c(109,106))
# X-squared = 2.0377, df = 1, p-value = 0.1534
# MO-max vs MA
prop.test(c(74,28),c(109,62))
# X-squared = 7.5642, df = 1, p-value = 0.005954****

coffee = now_agr %>%
  filter(story %in% c("test_coffee"))
View(coffee)
# MO-max vs MS-max
prop.test(c(86,70),c(119,106))
# X-squared = 0.75168, df = 1, p-value = 0.3859
# MO-max vs MA
prop.test(c(86,29),c(119,54))
# X-squared = 4.9418, df = 1, p-value = 0.02622

dentist = now_agr %>%
  filter(story %in% c("test_dentist"))
View(dentist)
# MO-max vs MS-max
prop.test(c(48,37),c(107,111))
# X-squared = 2.5776, df = 1, p-value = 0.1084
# MO-max vs MA
prop.test(c(48,20),c(107,56))
# X-squared = 0.9164, df = 1, p-value = 0.3384

hair = now_agr %>%
  filter(story %in% c("test_hair"))
View(hair)
# MO-max vs MA
prop.test(c(77,34),c(120,67))
# X-squared = 2.6777, df = 1, p-value = 0.1018
# MO-max vs MS-max
prop.test(c(77,53),c(120,101))
# X-squared = 2.631, df = 1, p-value = 0.1048

shoes = now_agr %>%
  filter(story %in% c("test_shoes"))
View(shoes)
# MO-max vs MA
prop.test(c(74,33),c(120,62))
# X-squared = 0.87903, df = 1, p-value = 0.3485
# MO-max vs MS-max
prop.test(c(74,62),c(120,116))
# X-squared = 1.3123, df = 1, p-value = 0.252

yarn = now_agr %>%
  filter(story %in% c("test_yarn"))
View(yarn)
# MO-max vs MS-max
prop.test(c(66,51),c(102,107))
# X-squared = 5.4826, df = 1, p-value = 0.01921 *
# MO-max vs MA
prop.test(c(66,32),c(102,61))
# X-squared = 1.9045, df = 1, p-value = 0.1676

######################################################################
# Removing the errant story
kruskal.test(as.factor(Chosen) ~ as.factor(ResponseType), data = ddnd)
# Kruskal-Wallis chi-squared = 513.44, df = 5, p-value < 2.2e-16

kruskal.test(as.factor(Chosen) ~ as.factor(story), data = ddnd)
# Kruskal-Wallis chi-squared = 5.9394, df = 4, p-value = 0.2037

inter <- interaction(ddnd$ResponseType, ddnd$story)
kruskal.test(as.factor(Chosen)~inter, data = ddnd)
# Kruskal-Wallis chi-squared = 534.82, df = 29, p-value < 2.2e-16



######################################################################

agr = dd %>%
  drop_na() %>%
  group_by(story,ResponseType,Chosen,tense) %>%
  summarize(SumChosen=sum(Chosen)) %>%
  group_by(ResponseType)
  # summarize(MeanChosen=mean(SumChosen),CILow=ci.low(SumChosen),CIHigh=ci.high(SumChosen)) %>%
  # ungroup() %>%
  # mutate(YMin=MeanChosen-CILow,YMax=MeanChosen+CIHigh)
#View(agr)  

seenagr = dd %>%
  drop_na() %>%
  group_by(story,ResponseType,Seen,tense) %>%
  summarize(SumSeen=sum(Seen)) %>%
  group_by(ResponseType)
# View(seenagr)

# overall sum chosen
ggplot(agr, aes(ResponseType,y=SumChosen)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_fill_grey() +
  ggtitle(label = "Sum Chosen") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #ylim(0,1)



# View(agr)


### Getting a box and whisker plot with Chris
#newstuff

# View(new)


summary(test)

#get the proportion of the chosen / seen
new <- agr %>% left_join(seenagr)
#View(new)
# this works, but is it showing what we want it to show??


# bar graph, not working
# new %>%
#   filter(., Chosen!=0, Seen!=0) %>%
#   mutate(., proportion = SumChosen / SumSeen) %>%
#   ggplot(., aes(ResponseType, y=proportion)) +
#   # geom_histogram(stat="identity") +
#   # geom_bar(position=dodge,stat="identity") +
#   # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
#   ylim(0,1)

# theme_set(theme_grey())
# View(new)
# box plot
new %>%
  filter(., Chosen!=0, Seen!=0) %>%
  mutate(., proportion = SumChosen / SumSeen) %>%
  ggplot(., aes(ResponseType, y=proportion)) +
  geom_boxplot() +
  # geom_bar(position=dodge,stat="identity") +
  # scale_fill_grey() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_wrap(~story) +
  ylab("Proportion chosen") +
  xlab("AnswerType") +
   # theme(axis.text=element_text(size=14),
   #       axis.title=element_text(size=18),
   #       legend.text=element_text(size=14)) +
  ylim(0,1)




#this isn't working, but is it really what we want to show?
# newr <- agr %>% left_join(seenagr)
# View(newr)
# newr %>%
#   filter(., Seen!=0) %>%
#   group_by(story,ResponseType,SumChosen,SumSeen) %>%
#   rowwise() %>%
#   mutate(., proportion = SumChosen / SumSeen) %>%
#   summarise(proportion = SumChosen / SumSeen, CILow = ci.low(proportion), CIHigh = ci.high(proportion)) %>%
#   mutate(YMin = proportion - CILow, YMax = proportion + CIHigh) %>%
# #dodge = position_dodge(.9)
#     ggplot(., aes(ResponseType, y=proportion,fill=tense)) +
#     #stat_boxplot() +
#   geom_bar(position=dodge,stat="identity") +
#   geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
#     scale_fill_grey() +
#     # ylab("Proportion chosen") +
#     # xlab("Answer") +
#     # theme(axis.text=element_text(size=14),
#     #       axis.title=element_text(size=18),
#     #       legend.text=element_text(size=14)) +
#     ylim(0,1)

View(new)
#trying to look at the breakdown perstory, unfinished

new %>%
  filter(., Chosen !=0, Seen!=0) %>%
  mutate(., proportion = SumChosen / SumSeen) %>%
  # mutate(., story = case_when(
  #   story == "test_florist" ~ "yarn",
  #   story == "test_books" ~ "books",
  #   story == "test_coffee" ~ "coffee",
  #   story == "test_dentist" ~ "dentist",
  #   story == "test_hair" ~m "hair",
  #   story == "test_shoes" ~ "shoes",
  #   story == "test_yarn" ~ "yarn",
  # ) ) %>% 
  ggplot(., aes(story, port,fill=tense))+
  geom_boxplot() +
  ylim(0,1) +
  facet_wrap(~ResponseType)

ggplot(new2, aes(ResponseType, y=port, fill=)) +
  geom_boxplot() +
  facet_wrap

########################################
##### try to look at the alternatives
# August 26, 2018
########################################
# Questions to answer:
# When MO/MS is seen, what else are people choosing?
# When they choose MO/MSlow, are they also choosing high?
# Are there people who are only choosing MA?

# 
dseen = total %>%
  filter(trial %in% c("test")) %>%
  select(subject,story,trial,exhaustive_seen,mention_one_high_seen,mention_one_low_seen,mention_some_high_seen,mention_some_low_seen,false_report_seen,none_of_above_seen,tense) %>%
  gather(ResponseTypeSeen,Seen,-subject,-story,-trial,-tense) %>%
  mutate(ResponseType=gsub("_seen","",ResponseTypeSeen,fixed=T)) %>%
  select(-ResponseTypeSeen)
View(dseen)


ggplot(dseen %>% filter(Seen == 1), aes(ResponseType)) +
  geom_histogram(stat="count") +
  coord_flip()


#look at per story
#distribution is pretty even
ggplot(dseen %>% filter(Seen == 1), aes(ResponseType,fill=tense)) +
  geom_histogram(stat="count") +
  facet_wrap(~story) +
  coord_flip()


# do the same for chosen
total = subset(total, total$story!="test_dentist")
dchosen = total %>%
  filter(trial %in% c("test")) %>%
  select(subject,story,trial,exhaustive_chosen,mention_one_high_chosen,mention_one_low_chosen,mention_some_high_chosen,mention_some_low_chosen,false_report_chosen,none_of_above_chosen,tense) %>%
  gather(ResponseTypeChosen,Chosen,-subject,-story,-trial,-tense) %>%
  mutate(ResponseType=gsub("_chosen","",ResponseTypeChosen,fixed=T)) %>%
  select(-ResponseTypeChosen)

View(dchosen)

ggplot(dchosen %>% filter(Chosen == 1), aes(ResponseType,fill=tense)) +
  geom_histogram(stat="count",position="dodge") +
  coord_flip()

# count of answer chosen by story
ggplot(dchosen %>% filter(Chosen == 1), aes(ResponseType,fill=tense)) +
  geom_histogram(stat="count",position="dodge") +
  coord_flip() +
  facet_wrap(~story)


#
# dc = total %>%
#   filter(trial %in% c("test")) %>%
#   select(subject,story,trial,exhaustive_seen,mention_one_low_chosen,mention_some_low_chosen,false_report_chosen,none_of_above_chosen,tense) %>%
#   gather(ResponseTypeChosen,Chosen,-subject,-story,-trial,-tense) %>%
#   mutate(ResponseType=gsub("_chosen","",ResponseTypeChosen,fixed=T)) %>%
#   select(-ResponseTypeChosen)

#looking at per subject breakdown
p = ggplot(dchosen %>% filter(Chosen == 1), aes(ResponseTypeChosen,fill=tense)) +
  geom_histogram(stat="count",position="dodge") +
  coord_flip() +
  facet_wrap(~subject)

dd = dseen %>%
  left_join(dchosen,by=c("subject","story","trial","tense","ResponseType"))
dd = subset(dd, dd$story!="test_dentist")
View(dd)
nrow(dd)
nrow(dseen)
nrow(dchosen)

agr = dd %>%
  group_by(subject,story,tense) %>%
  summarize(SumChosen=sum(Chosen)) %>%
  group_by(subject) %>%
  summarize(MeanChosen=mean(SumChosen),CILow=ci.low(SumChosen),CIHigh=ci.high(SumChosen)) %>%
  ungroup() %>%
  mutate(YMin=MeanChosen-CILow,YMax=MeanChosen+CIHigh)
View(agr)

ggplot(agr, aes(x=reorder(subject,MeanChosen),y=MeanChosen)) +
  geom_bar(stat="identity",color="black",fill="gray80") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25)


agr = dd %>%
  filter(Chosen==1) %>%
  group_by(subject) %>%
  mutate(count = n())

ggplot(agr, aes(x=reorder(subject,count),fill=ResponseType)) +
  geom_histogram(stat="count")


#################################################
# Now start trying to look at the alternatives
#################################################
# Reminder, Questions to answer:
# When MO/MS is seen, what else are people choosing?
# When they choose MO/MSlow, are they also choosing high?
# Are there people who are only choosing MA?

#exhaustive seen
total = subset(total, total$story!="test_dentist")
agr = total %>%
  filter(exhaustive_seen == 1 & trial %in% c("test")) %>%
  select(subject,story,trial,exhaustive_seen,exhaustive_chosen,mention_one_high_chosen,mention_one_low_chosen,mention_some_high_chosen,mention_some_low_chosen,tense) %>%
  gather(ResponseTypeChosen,Chosen,-subject,-story,-trial,-tense,-exhaustive_seen,) %>%
  mutate(ResponseType=gsub("_chosen","",ResponseTypeChosen,fixed=T)) %>%
  select(-ResponseTypeChosen) %>%
  arrange(story,subject) %>%
  group_by(trial,ResponseType,tense) %>%
  summarize(Mean=mean(Chosen),CILow=ci.low(Chosen),CIHigh=ci.high(Chosen)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
dodge=position_dodge(.9)
View(agr)

ggplot(agr, aes(x=ResponseType,y=Mean,fill=tense)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  ylim(0,1) +
  coord_flip() +
  ggtitle("exhaustive seen")
ggsave("../graphs/exhaustive_seen.pdf",width=6)

#MO-h seen and resonses chosen
agr = total %>%
  filter(mention_one_high_seen == 1 & trial %in% c("test")) %>%
  select(subject,story,trial,exhaustive_seen,exhaustive_chosen,mention_one_high_chosen,mention_one_low_chosen,mention_some_high_chosen,mention_some_low_chosen,tense) %>%
  gather(ResponseTypeChosen,Chosen,-subject,-story,-trial,-tense,-exhaustive_seen,) %>%
  mutate(ResponseType=gsub("_chosen","",ResponseTypeChosen,fixed=T)) %>%
  select(-ResponseTypeChosen) %>%
  arrange(story,subject) %>%
  group_by(trial,ResponseType,tense) %>%
  summarize(Mean=mean(Chosen),CILow=ci.low(Chosen),CIHigh=ci.high(Chosen)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
dodge=position_dodge(.9)

ggplot(agr, aes(x=ResponseType,y=Mean,fill=tense)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  ylim(0,1) +
  coord_flip() +
  ggtitle("mention-one-max seen")
ggsave("../graphs/mo-max_seen.pdf",width=6)


#MS-h seen and resonses chosen
agr = total %>%
  filter(mention_some_high_seen == 1 & trial %in% c("test")) %>%
  select(subject,story,trial,exhaustive_seen,exhaustive_chosen,mention_one_high_chosen,mention_one_low_chosen,mention_some_high_chosen,mention_some_low_chosen,tense) %>%
  gather(ResponseTypeChosen,Chosen,-subject,-story,-trial,-tense,-exhaustive_seen,) %>%
  mutate(ResponseType=gsub("_chosen","",ResponseTypeChosen,fixed=T)) %>%
  select(-ResponseTypeChosen) %>%
  arrange(story,subject) %>%
  group_by(trial,ResponseType,tense) %>%
  summarize(Mean=mean(Chosen),CILow=ci.low(Chosen),CIHigh=ci.high(Chosen)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
dodge=position_dodge(.9)

ggplot(agr, aes(x=ResponseType,y=Mean,fill=tense)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  ylim(0,1) +
  coord_flip() +
  ggtitle("mention-some-max seen")
ggsave("../graphs/ms-max_seen.pdf",width=6)

#MO-l seen and resonses chosen
agr = total %>%
  filter(mention_one_low_seen == 1 & trial %in% c("test")) %>%
  select(subject,story,trial,exhaustive_seen,exhaustive_chosen,mention_one_high_chosen,mention_one_low_chosen,mention_some_high_chosen,mention_some_low_chosen,tense) %>%
  gather(ResponseTypeChosen,Chosen,-subject,-story,-trial,-tense,-exhaustive_seen,) %>%
  mutate(ResponseType=gsub("_chosen","",ResponseTypeChosen,fixed=T)) %>%
  select(-ResponseTypeChosen) %>%
  arrange(story,subject) %>%
  group_by(trial,ResponseType,tense) %>%
  summarize(Mean=mean(Chosen),CILow=ci.low(Chosen),CIHigh=ci.high(Chosen)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
dodge=position_dodge(.9)

ggplot(agr, aes(x=ResponseType,y=Mean,fill=tense)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  ylim(0,1) +
  coord_flip() +
  ggtitle("mention-one-min seen")
ggsave("../graphs/mo-min_seen.pdf",width=6)


#MS-l seen and resonses chosen
agr = total %>%
  filter(mention_some_low_seen == 1 & trial %in% c("test")) %>%
  select(subject,story,trial,exhaustive_seen,exhaustive_chosen,mention_one_high_chosen,mention_one_low_chosen,mention_some_high_chosen,mention_some_low_chosen,tense) %>%
  gather(ResponseTypeChosen,Chosen,-subject,-story,-trial,-tense,-exhaustive_seen,) %>%
  mutate(ResponseType=gsub("_chosen","",ResponseTypeChosen,fixed=T)) %>%
  select(-ResponseTypeChosen) %>%
  arrange(story,subject) %>%
  group_by(trial,ResponseType,tense) %>%
  summarize(Mean=mean(Chosen),CILow=ci.low(Chosen),CIHigh=ci.high(Chosen)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
dodge=position_dodge(.9)

ggplot(agr, aes(x=ResponseType,y=Mean,fill=tense)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  ylim(0,1) +
  coord_flip() +
  ggtitle("mention-some-min seen")
ggsave("../graphs/ms-min_seen.pdf",width=6)


##########################
# this part isn't working quite right.
# Error: Error in mutate_impl(.data, dots) : 
# Column `alternatives` must be length 1936 (the number of rows) or one, not 3480
total$alternatives = paste(total$tag_a,d$tag_b,d$tag_c)
tmp = total %>%
  # rowwise() %>%
  mutate(alternatives=paste(sort(c(tag_a,tag_b,tag_c))))

dchosen = total %>%
  filter(trial %in% c("test")) %>%
  select(alternatives,subject,story,trial,exhaustive_chosen,mention_one_high_chosen,mention_one_low_chosen,mention_some_high_chosen,mention_some_low_chosen,tense) %>%
  gather(ResponseTypeChosen,Chosen,-subject,-story,-trial,-alternatives,-tense) %>%
  mutate(ResponseType=gsub("_chosen","",ResponseTypeChosen,fixed=T)) %>%
  select(-ResponseTypeChosen)

View(dchosen)

agr = dchosen %>%
  group_by(trial,ResponseType,alternatives) %>%
  summarize(Mean=mean(Chosen),CILow=ci.low(Chosen),CIHigh=ci.high(Chosen)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
dodge=position_dodge(.9)

View(agr)

p=ggplot(agr, aes(x=ResponseType,y=Mean,fill=trial)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  ylim(0,1) +
  coord_flip() +
  facet_wrap(~alternatives) +
  ggtitle("../graphs/mention_one_high seen")


####### doing some stats
# Questions
# When MO/MS is seen, what else are people choosing?
# When they choose MO/MSlow, are they also choosing high?
# Are there people who are only choosing MA?
head(total)

#when MO-high is seen
mohseen <- subset(total, total$mention_one_high_seen=="1")


agrmo <- dd %>%
  #filter(ResponseType %in% c("mention_one_high"))
  filter(., Seen!=0) #%>%
  #mutate(., proportion = SumChosen / SumSeen)

View(agrmo)
kruskal.test(as.factor(Chosen) ~ as.factor(ResponseType), data = agrmo)


###############
test <- subset(total, total$trial=="test")
summary(test)
tru <- subset(test, test$native_lang!="Albanian")

#testing tense in exh 
exh <- subset(tru, tru$exhaustive_seen=="1")
View(exh)
head(exh)
kruskal.test(tense ~ exhaustive_chosen, data = exh)
#Kruskal-Wallis chi-squared = 3.4276, df = 1, p-value = 0.06412

#MOH vs EXH
moh <- subset(tru, tru$mention_one_high_seen=="1")


a <- subset(dd, dd$ResponseType=="exhaustive" | dd$ResponseType=="mention_one_high")
b <- subset(a, a$Seen=="1")
View(b)
kruskal.test(ResponseType ~ Chosen, data = b)
#Kruskal-Wallis chi-squared = 17.536, df = 1, p-value = 2.82e-05

c <- subset(dd, dd$ResponseType=="mention_one_high" | dd$ResponseType=="mention_some_high"| dd$ResponseType=="mention_some_low"| dd$ResponseType=="mention_one_low")
d <- subset(c, c$Seen=="1")
View(d)
kruskal.test(ResponseType ~ Chosen, data = d)
# Kruskal-Wallis chi-squared = 55.983, df = 1, p-value = 7.309e-14



View(subb)
shoess = subb %>%
  filter(story %in% c("test_shoes"))
View(shoess)


