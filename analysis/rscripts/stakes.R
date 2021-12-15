#Stakes
#last edit: 4/25/18

library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(bootstrap)
library(lme4)
library(tidyverse)

theme_set(theme_bw())

this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
source("helpers.R")

# to edit the processed_results.csv file, go to the file named "stakes_processing.R".


#import the filtered CSV into R
a <- read.csv("../data/stakes_easy_nonfin.csv")
#View(a)
#unique(a$subject) #116 subjects
#97 from online, 3 removed for browser incompatibility

#unique(a$native_lang) 

#remove non-native
b <- subset(a, a$native_lang!="Chinese") 
d <- subset(b, b$native_lang!="English%2C Japanese")

#View(d)
#unique(d$subject)
#114, so 2 removed from non-native speaker status
#da <- filter(trial)

#take a look at the average count of answers seen
dseen = d %>%
  filter(trial %in% c("high","low")) %>%
  drop_na() %>%
  select(subject,story,trial,exhaustive_seen,mention_one_high_seen,mention_one_low_seen,mention_some_high_seen,mention_some_low_seen,false_report_seen,none_of_above_seen) %>%
  gather(ResponseTypeSeen,Seen,-subject,-story,-trial) %>%
  mutate(ResponseType=gsub("_seen","",ResponseTypeSeen,fixed=T)) %>%
  select(-ResponseTypeSeen)
#View(dseen)
  
# plot the distribution of the answers seen, out of 1200
ggplot(dseen %>% filter(Seen == 1), aes(ResponseType)) +
  geom_histogram(stat="count") +
  coord_flip()

#take a look at the distribution by trial
ggplot(dseen %>% filter(Seen == 1), aes(ResponseType)) +
  geom_histogram(stat="count") +
  facet_wrap(~trial) +
  coord_flip()



# Do the same for chosen
dchosen = d %>%
  filter(trial %in% c("high","low")) %>%
  drop_na() %>%
  select(subject,story,trial,exhaustive_chosen,mention_one_high_chosen,mention_one_low_chosen,mention_some_high_chosen,mention_some_low_chosen,false_report_chosen,none_of_above_chosen) %>%
  gather(ResponseTypeChosen,Chosen,-subject,-story,-trial,-trial) %>%
  mutate(ResponseType=gsub("_chosen","",ResponseTypeChosen,fixed=T)) %>%
  select(-ResponseTypeChosen)

# View(dchosen)
############################################################
kruskal.test(as.factor(Chosen) ~ as.factor(ResponseType), data = dchosen)
# Kruskal-Wallis chi-squared = 1130.3, df = 6, p-value < 2.2e-16 ******

kruskal.test(as.factor(Chosen) ~ as.factor(trial), data = dchosen)
# Kruskal-Wallis chi-squared = 0.2825, df = 1, p-value = 0.5951

kruskal.test(as.factor(Chosen) ~ as.factor(story), data = dchosen)
# Kruskal-Wallis chi-squared = 4.2375, df = 9, p-value = 0.8951

inter <- interaction(dchosen$ResponseType, dchosen$story)
kruskal.test(as.factor(Chosen)~inter, data = dchosen)
# Kruskal-Wallis chi-squared = 1228.2, df = 69, p-value < 2.2e-16
############################################################
# Effect of Stakes disappears in the MS Answer Types
ms = dchosen %>%
  filter(ResponseType %in% c("mention_some_high","mention_some_low"))

View(ms)
kruskal.test(as.factor(Chosen) ~ as.factor(trial), data = ms)
# Kruskal-Wallis chi-squared = 0.048971, df = 1, p-value = 0.8249

############################################################
# Do we see story effects within Stakes?
high = dchosen %>%
  filter(trial %in% c("high"))

kruskal.test(as.factor(Chosen) ~ as.factor(story), data = high)
# Kruskal-Wallis chi-squared = 1.4944, df = 4, p-value = 0.8276


low = dchosen %>%
  filter(trial %in% c("low"))

kruskal.test(as.factor(Chosen) ~ as.factor(story), data = low)
# Kruskal-Wallis chi-squared = 2.453, df = 4, p-value = 0.6531

############################################################
dmohexh = dchosen %>%
  filter(ResponseType %in% c("mention_one_high", "exhaustive"))

kruskal.test(as.factor(Chosen) ~ as.factor(trial), data = dmohexh)
# Kruskal-Wallis chi-squared = 2.8601, df = 1, p-value = 0.0908


dmoh = dchosen %>%
  filter(ResponseType %in% c("mention_one_high"))
kruskal.test(as.factor(Chosen) ~ as.factor(trial), data = dmoh)
# Kruskal-Wallis chi-squared = 19.996, df = 1, p-value = 7.761e-06

############################################################
# in the FBI and TERRORISM stories, diference between MA and MO-MAX or MS-MAX?
View(dchosen)
fbimoh = dchosen %>%
  filter(story %in% c("high_fbi")) %>%
  filter(ResponseType %in% c("exhaustive", "mention_one_high"))
length(unique(fbimoh$subject)) #114 subjects

kruskal.test(Chosen ~ ResponseType, data = fbimoh)
# Kruskal-Wallis chi-squared = 2.1231, df = 1, p-value = 0.1451
wilcox.test(Chosen ~ ResponseType, data=fbimoh)
# W = 5928, p-value = 0.1454

fbimsh = dchosen %>%
  filter(story %in% c("high_fbi")) %>%
  filter(ResponseType %in% c("exhaustive", "mention_some_high"))
kruskal.test(Chosen ~ ResponseType, data = fbimsh)
# Kruskal-Wallis chi-squared = 12.818, df = 1, p-value = 0.0003434 ***
wilcox.test(Chosen ~ ResponseType, data=fbimsh)
# W = 5016, p-value = 0.000345
length(unique(fbimsh$subject))

terrmoh = dchosen %>%
  filter(story %in% c("high_terrorism")) %>%
  filter(ResponseType %in% c("exhaustive", "mention_one_high"))
kruskal.test(Chosen ~ ResponseType, data = terrmoh)
# Kruskal-Wallis chi-squared = 6.8091, df = 1, p-value = 0.00907
wilcox.test(Chosen ~ ResponseType, data=terrmoh)
# W = 5415, p-value = 0.009102

terrmsh = dchosen %>%
  filter(story %in% c("high_terrorism")) %>%
  filter(ResponseType %in% c("exhaustive", "mention_some_high"))
kruskal.test(Chosen ~ ResponseType, data = terrmsh)
# Kruskal-Wallis chi-squared = 18.343, df = 1, p-value = 1.845e-05
wilcox.test(Chosen ~ ResponseType, data=terrmsh)
# W = 4674, p-value = 1.854e-05

togeth = dchosen %>%
  filter(story %in% c("high_terrorism", "high_fbi")) %>%
  filter(ResponseType %in% c("exhaustive", "mention_one_high", "mention_some_high"))
kruskal.test(Chosen ~ ResponseType, data = togeth)




############################################################

ggplot(dchosen %>% filter(Chosen == 1), aes(ResponseType,fill=trial)) +
  geom_histogram(stat="count",position="dodge") +
  coord_flip()

#take a look at the differences per story
ggplot(dchosen %>% filter(Chosen == 1), aes(ResponseType,fill=trial)) +
  geom_histogram(stat="count",position="dodge") +
  coord_flip() +
  facet_wrap(~story)


p = ggplot(dchosen %>% filter(Chosen == 1), aes(ResponseTypeChosen,fill=trial)) +
  geom_histogram(stat="count",position="dodge") +
  coord_flip() +
  facet_wrap(~subject)
p

#put the two together so that we can take a look at proportions
dd = dseen %>%
  left_join(dchosen,by=c("subject","story","trial","ResponseType"))

View(dd)

agr2 = dd %>%
  group_by(ResponseType,story) %>%
  summarize(propChosen=sum(Chosen)/sum(Seen)) %>%
  # group_by(story) %>%
  # summarize(MeanChosen=mean(propChosen),CILow=ci.low(propChosen),CIHigh=ci.high(propChosen)) %>%
  ungroup()
# mutate(YMin=MeanChosen-CILow,YMax=MeanChosen+CIHigh)
agr2 = subset(agr2, agr2$ResponseType!="false_report")
agr2 = subset(agr2, agr2$ResponseType!="none_of_above")
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

View(dd)

dd_high = dd %>%
  filter(trial %in% c("high")) 




#gather the individuals to look at that
agr = dd %>%
  group_by(subject,story) %>%
  summarize(SumChosen=sum(Chosen)) %>%
  group_by(subject) %>%
  summarize(MeanChosen=mean(SumChosen),CILow=ci.low(SumChosen),CIHigh=ci.high(SumChosen)) %>%
  ungroup() %>%
  mutate(YMin=MeanChosen-CILow,YMax=MeanChosen+CIHigh)

#plot all the individuals mean chosen
ggplot(agr, aes(x=reorder(subject,MeanChosen),y=MeanChosen)) +
  geom_bar(stat="identity",color="black",fill="gray80") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25)

agr = dd %>%
  filter(Chosen==1) %>%
  group_by(subject) %>%
  mutate(count = n())

#look at all the subjects and their choices
ggplot(agr, aes(x=reorder(subject,count),fill=ResponseType)) +
  geom_histogram(stat="count") 
  
see = subset(dd, dd$Seen=="1")
nrow(see)
dd_seen = dd %>%
  filter(Seen %in% c("1")) %>%
  separate(story, c("Informativity1","story")) %>%
  separate(ResponseType, c("ResponseType","fill", "Informativity")) %>%
  unite(ResponseType, ResponseType, fill, sep="_") %>%
  select(-c(Informativity1))
nrow(dd_seen) #4560
nrow()

write.csv(dd_seen, "../graphs/stakes_nonfinite.csv")

sub = dd_seen %>%
  filter(ResponseType %in% c("exhaustive_NA", "mention_some", "mention_one"))

View(dd_seen)
kruskal.test(Chosen ~ ResponseType, data = sub)
# Kruskal-Wallis chi-squared = 909.64, df = 4, p-value < 2.2e-16 (everything)
# Kruskal-Wallis chi-squared = 16.675, df = 2, p-value = 0.0002393 (just MA, MS, MO)

pairwise.wilcox.test(sub$Chosen, sub$ResponseType, p.adjust.method = "BH")
# Everything
#               exhaustive_NA false_report mention_one mention_some
#   false_report < 2e-16       -            -           -           
#   mention_one  6.2e-05       < 2e-16      -           -           
#   mention_some 0.00058       < 2e-16      0.34540     -           
#   none_of      < 2e-16       2.7e-06      < 2e-16     < 2e-16 

#               exhaustive_NA mention_one
#   mention_one  0.00015       -          
#   mention_some 0.00078       0.34540 

kruskal.test(Chosen ~ Informativity, data = dd_seen)
# Kruskal-Wallis chi-squared = 1302.5, df = 2, p-value < 2.2e-16

kruskal.test(Chosen ~ trial, data = sub)



##################
# trying to plot everything
View(agrr)

agrr = dd_seen %>%
  group_by(story,trial,ResponseType,Chosen) %>%
  summarize(SumChosen=sum(Chosen)) %>%
  group_by(ResponseType)

seenagr = dd_seen %>%
  drop_na() %>%
  group_by(story,trial,ResponseType,Seen) %>%
  summarize(SumSeen=sum(Seen)) %>%
  group_by(ResponseType)
View(seenagr)

new <- agrr %>% 
  left_join(seenagr) %>%
  drop_na()

nowr <- subset(new, new$ResponseType!="none_of")
now <- subset(nowr, nowr$ResponseType!="false")

now_agr = now %>%
  # filter(., Chosen!=0, Seen!=0) %>%
  mutate(., proportion = SumChosen / SumSeen)

View(now_agr)
res = now_agr %>%
  group_by(trial, ResponseType, Informativity) %>%
  summarise(SumChosen=sum(SumChosen),SumSeen=sum(SumSeen)) %>%
  filter(trial %in% c("high","low"))

View(res)

# Across the entire (high/low) samples:
# prop test MA vs. MO-max in HIGH Stakes
prop.test(x = c(144, 214), n = c(188, 325))
# X-squared = 6.0274, df = 1, p-value = 0.01409 *
binom.test(c(144, 214),c(188,325), p=0.5)????
  
# prop test MA vs. MS-max in High Stakes
prop.test(x = c(144, 276), n = c(188, 374))
# X-squared = 0.38142, df = 1, p-value = 0.5368 NOT SIGNIFICANT!

# prop test MA vs. MO-max in LOW Stakes
prop.test(x = c(107, 289), n = c(201, 354))
# X-squared = 49.223, df = 1, p-value = 2.285e-12 ***

# prop test MA vs. MS-max in LOW Stakes
prop.test(x = c(289, 276), n = c(201, 343))
# X-squared = 43.81, df = 1, p-value = 3.619e-11 ***

# prop test MO-max vs. MS-max in LOW Stakes
prop.test(x = c(289, 276), n = c(354, 343))
# X-squared = 0.088864, df = 1, p-value = 0.7656

View(res_low)

now_high = now_agr %>%
  filter(trial %in% c("high"))

now_low = now_agr %>%
  filter(trial %in% c("low"))

View(now_low)
  
ggplot(now_low, aes(ResponseType, y=proportion)) +
  # geom_boxplot() +
  geom_bar(position=dodge,stat="identity") +
  facet_wrap(~story) +
  # scale_fill_grey() +
  facet_wrap(~Informativity) +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  ylab("Proportion chosen") +
  xlab("AnswerType") +
  # theme(axis.text=element_text(size=14),
  #       axis.title=element_text(size=18),
  #       legend.text=element_text(size=14)) +
  ylim(0,1)

new_fbi = now_agr %>%
  filter(story %in% c("high_fbi"))

new_terr = now_agr %>%
  filter(story %in% c("high_terrorism"))

View(now_high)
###################################
# Test for Equal or Given Propostions

# Though many of the graphs appear to show significant differences between MA and MO-max/MS-max, 
# none of the differences are in fact significant.

# Inspector
new_ins = now_agr %>%
  filter(story %in% c("high_inspector"))
View(new_ins)
# MA vs. MO-max - NO
prop.test(x = c(26, 42), n = c(29, 59))
# X-squared = 2.7979, df = 1, p-value = 0.09439
# MA vs. MS-max - NO
prop.test(x = c(26, 52), n = c(29, 72))
# X-squared = 2.65, df = 1, p-value = 0.1036

# MO-max vs MS-max
prop.test(x = c(42, 52), n = c(59, 72))
# X-squared = 9.8038e-31, df = 1, p-value = 1

# Firefighter
new_fire = now_agr %>%
  filter(story %in% c("high_firefighter"))
View(new_fire)
# MA vs. MO-max NO
prop.test(x = c(28, 41), n = c(33, 65))
# X-squared = 3.9894, df = 1, p-value = 0.04579 * 
# MA vs, MS-max
prop.test(x = c(28, 52), n = c(33, 72))
# X-squared = 1.3535, df = 1, p-value = 0.2447

# Operatove
new_op = now_agr %>%
  filter(story %in% c("high_operative"))
View(new_op)
# MA vs. MO-max NO
prop.test(x = c(30, 42), n = c(40, 73))
# X-squared = 2.6961, df = 1, p-value = 0.1006


# FBI Story
# High Stakes, MO-max vs MA
prop.test(x = c(28, 38), n = c(42, 60))
# X-squared = 0.018552, df = 1, p-value = 0.8917

# High Stakes, MA vs. MS-max
prop.test(x = c(28, 54), n = c(42, 74))
# X-squared = 0.25494, df = 1, p-value = 0.6136

# High, MO-max vs MS-max
prop.test(x = c(54, 38), n = c(74, 60))
# X-squared = 1.0179, df = 1, p-value = 0.313

#################################################
# Now start trying to look at the alternatives
#################################################

agr = d %>%
  filter(exhaustive_seen == 1 & trial %in% c("high","low")) %>%
  select(subject,story,trial,exhaustive_seen,exhaustive_chosen,mention_one_high_chosen,mention_one_low_chosen,mention_some_high_chosen,mention_some_low_chosen,false_report_chosen,none_of_above_chosen) %>%
  gather(ResponseTypeChosen,Chosen,-subject,-story,-trial,-trial,-exhaustive_seen,) %>%
  mutate(ResponseType=gsub("_chosen","",ResponseTypeChosen,fixed=T)) %>%
  select(-ResponseTypeChosen) %>%
  arrange(story,subject) %>%
  group_by(trial,ResponseType) %>%
  summarize(Mean=mean(Chosen),CILow=ci.low(Chosen),CIHigh=ci.high(Chosen)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
dodge=position_dodge(.9)

ggplot(agr, aes(x=ResponseType,y=Mean,fill=trial)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  ylim(0,1) +
  coord_flip() +
  ggtitle("exhaustive seen")
ggsave("../graphs/exhaustive_seen.pdf",width=6)

agr = d %>%
  filter(mention_one_high_seen == 1 & trial %in% c("high","low")) %>%
  select(subject,story,trial,mention_one_high_seen,exhaustive_chosen,mention_one_high_chosen,mention_one_low_chosen,mention_some_high_chosen,mention_some_low_chosen,false_report_chosen,none_of_above_chosen) %>%
  gather(ResponseTypeChosen,Chosen,-subject,-story,-trial,-trial,-mention_one_high_seen) %>%
  mutate(ResponseType=gsub("_chosen","",ResponseTypeChosen,fixed=T)) %>%
  select(-ResponseTypeChosen) %>%
  arrange(story,subject) %>%
  group_by(trial,ResponseType) %>%
  summarize(Mean=mean(Chosen),CILow=ci.low(Chosen),CIHigh=ci.high(Chosen)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
dodge=position_dodge(.9)

ggplot(agr, aes(x=ResponseType,y=Mean,fill=trial)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  ylim(0,1) +
  coord_flip() +
  ggtitle("mention_one_high seen")
ggsave("../graphs/mention_one_high_seen.pdf",width=6)
  

#this part isn't working right
names(d)

d$alternatives = paste(d$tag_a,d$tag_b,d$tag_c)
tmp = d %>%
  #rowwise() %>%
  mutate(alternatives=paste(sort(c(tag_a,tag_b,tag_c))))
  
dchosen = d %>%
  filter(trial %in% c("high","low")) %>%
  select(alternatives,subject,story,trial,exhaustive_chosen,mention_one_high_chosen,mention_one_low_chosen,mention_some_high_chosen,mention_some_low_chosen,false_report_chosen,none_of_above_chosen) %>%
  gather(ResponseTypeChosen,Chosen,-subject,-story,-trial,-alternatives) %>%
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
  ggtitle("mention_one_high seen")

ggsave(p,"../graphs/results_by_alternatives.pdf",width=20,height=15)



test <- subset(d,d$trial=="high" | d$trial=="low")
View(test)
summary(test)



m = glmer(mention_one_high ~ trial + (1|subject),family="binomial",data=text)


####################################
#Look at t-test for responses_chosen
####################################
#effect of TRIAL on RESPONSE_CHOSEN
kruskal.test(trial ~ exhaustive_chosen, data = test) #YES
#Kruskal-Wallis chi-squared = 6.988, df = 1, p-value = 0.008206
kruskal.test(trial ~ mention_one_high_chosen, data = test) #YES
#Kruskal-Wallis chi-squared = 19.996, df = 1, p-value = 7.761e-06
kruskal.test(trial ~ mention_some_high_chosen, data = test) #no
#Kruskal-Wallis chi-squared = 0, df = 1, p-value = 1
kruskal.test(trial ~ mention_one_low_chosen, data = test) #no
#Kruskal-Wallis chi-squared = 1.0801, df = 1, p-value = 0.2987
kruskal.test(trial ~ mention_some_low_chosen, data = test) #no
#Kruskal-Wallis chi-squared = 0.14118, df = 1, p-value = 0.7071

kruskal.test(trial ~ false_report_chosen, data = test)
#Kruskal-Wallis chi-squared = 7.6856, df = 1, p-value = 0.005566
kruskal.test(trial ~ none_of_above_chosen, data = test)
#Kruskal-Wallis chi-squared = 4.8993, df = 1, p-value = 0.02687


# effect of MS type
ms_h <- subset(test, test$mention_some_high_seen)
View(ms_h)

mo_h <- subset(test, test$mention_one_high_seen=="1")
View(mo_h)

ms_l <- subset(test, test$mention_some_low_seen=="1")
View(ms_l)

mo_l <- subset(test, test$mention_one_low_seen=="1")
View(mo_l)

mso <- subset(test, test$mention_one_high_seen=="1" | test$mention_some_high_seen=="1" | test$mention_some_low_seen=="1" | test$mention_one_low_seen=="1")
View(mso)

write.csv(mso, file = "../data/stakes_mso.csv")

#summary(test)

mso_high <- subset(mso, mso$trial=="high")

msagr <- read.csv("../data/stakes_msagr.csv")
head(msagr)
kruskal.test(trial ~ response, data=msagr) #significant


agr = msagr %>%
  group_by(trial,response) %>%
  summarise(Proportion_yes = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Proportion_yes - CILow, YMax = Proportion_yes + CIHigh)
dodge = position_dodge(.9)

#plot the overall data
ggplot(agr, aes(tense,y=Proportion_yes,fill=tense)) +
  geom_bar(position=dodge,stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~emb_verb) +
  scale_fill_grey() +
  ggtitle(label = "Mention-All T+F predict") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)




####################################

prop_exh <- d$exhaustive/sum(d$exhaustive)

###########STATS

a <- subset(dd, dd$ResponseType=="exhaustive" | dd$ResponseType=="mention_one_high")
b <- subset(a, a$Seen=="1")
kruskal.test(ResponseType ~ Chosen, data = b)
# Kruskal-Wallis chi-squared = 10.868, df = 1, p-value = 0.0009783

#testing interaction between stakes and MO-h/exh answer
interAB<-interaction(b$ResponseType, b$trial)
kruskal.test(Chosen ~ interAB, data = b)
#Kruskal-Wallis chi-squared = 56.726, df = 3, p-value = 2.94e-12

c <- subset(dd, dd$ResponseType=="mention_one_high" | dd$ResponseType=="mention_some_high"| dd$ResponseType=="mention_some_low"| dd$ResponseType=="mention_one_low")
d <- subset(c, c$Seen=="1")
kruskal.test(ResponseType ~ Chosen, data = d)
#Kruskal-Wallis chi-squared = 92.986, df = 1, p-value < 2.2e-16


#MO vs. MS - High
m <- subset(dd, dd$ResponseType=="mention_one_high" | dd$ResponseType=="mention_some_high")
n <- subset(m,m$Seen=="1")
kruskal.test(ResponseType ~ Chosen, data = n)
#chi-squared = 1.5963, df = 1, p-value = 0.2064

#Mo vs MS - Low
o <- subset(dd, dd$ResponseType=="mention_one_low" | dd$ResponseType=="mention_some_low")
p <- subset(o,m$Seen=="1")
kruskal.test(ResponseType ~ Chosen, data = p)
#chi-squared = 2.4002, df = 1, p-value = 0.1213

#MS-h vs MA
q <- subset(dd, dd$ResponseType=="mention_some_high" | dd$ResponseType=="exhaustive")
r <- subset(q,m$Seen=="1")
kruskal.test(ResponseType ~ Chosen, data = r)
#MS-H:chi-squared = 508.76, df = 1, p-value < 2.2e-16
  
#MS-l vs MA
s <- subset(dd, dd$ResponseType=="mention_some_low" | dd$ResponseType=="exhaustive")
t <- subset(s,m$Seen=="1")
kruskal.test(ResponseType ~ Chosen, data = t)
#chi-squared = 0.092751, df = 1, p-value = 0.7607

#MS l vs. h
u <- subset(dd, dd$ResponseType=="mention_some_low" | dd$ResponseType=="mention_some_high")
v <- subset(u,m$Seen=="1")
kruskal.test(ResponseType ~ Chosen, data = v)
# chi-squared = 110.17, df = 1, p-value < 2.2e-16