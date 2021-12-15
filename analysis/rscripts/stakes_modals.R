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

#Set the working directory to whereever you have your raw data and the "helpers.R" file
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
source("helpers.R")

d <- read.csv("../data/should2.csv", header=FALSE)
# if you need to make the subjects characters a sting
#as.character(d$subject)
str(d)
View(d)
head(d)
unique(d$V1)

#for the moment, remove the native speaker tests
good <- subset(d, d$V5!='nst')
View(good)

gooder <- subset(good, good$V5!='train_end')
#write that to a CSV to try and use the filter python script
write.csv(gooder, file = "../data/should_stakes_easy.csv")

#Remove subjects whose browsers are not reporting answers for nonfinite condition
#filt <- subset(gooder, good$V1!='A219VCQZADQ45W')
#filtered <- subset(filt, good$V1!='A13J3RCDUBXQ7E' | good$V1!='A3ODVV1SM7DCXN')

#for removing subjects from the 'should' data set
filt <- subset(gooder, good$V1!='A1GPO13F5EY7FJ' | good$V1!='AHVB5DEQEUMEO')


#make a CSV that can be filtered properly using Tim's python script
write.csv(filt, file = "../data/should_stakes_easy.csv")

#View(filtered)

#import the filtered CSV back into R
d <- read.csv("../data/stakes_easy_nonfin.csv")
View(d)

names(d)

dseen = d %>%
  filter(trial %in% c("high","low")) %>%
  select(subject,story,trial,exhaustive_seen,mention_one_high_seen,mention_one_low_seen,mention_some_high_seen,mention_some_low_seen,false_report_seen,none_of_above_seen) %>%
  gather(ResponseTypeSeen,Seen,-subject,-story,-trial,-trial) %>%
  mutate(ResponseType=gsub("_seen","",ResponseTypeSeen,fixed=T)) %>%
  select(-ResponseTypeSeen)

ggplot(dseen %>% filter(Seen == 1), aes(ResponseType)) +
  geom_histogram(stat="count") +
  coord_flip()

dchosen = d %>%
  filter(trial %in% c("high","low")) %>%
  select(subject,story,trial,exhaustive_chosen,mention_one_high_chosen,mention_one_low_chosen,mention_some_high_chosen,mention_some_low_chosen,false_report_chosen,none_of_above_chosen) %>%
  gather(ResponseTypeChosen,Chosen,-subject,-story,-trial,-trial) %>%
  mutate(ResponseType=gsub("_chosen","",ResponseTypeChosen,fixed=T)) %>%
  select(-ResponseTypeChosen)

tail(dchosen)


ggplot(dchosen %>% filter(Chosen == 1), aes(ResponseType,fill=trial)) +
  geom_histogram(stat="count",position="dodge") +
  coord_flip()

ggplot(dchosen %>% filter(Chosen == 1), aes(ResponseType,fill=trial)) +
  geom_histogram(stat="count",position="dodge") +
  coord_flip() +
  facet_wrap(~story)

p = ggplot(dchosen %>% filter(Chosen == 1), aes(ResponseTypeChosen,fill=trial)) +
  geom_histogram(stat="count",position="dodge") +
  coord_flip() +
  facet_wrap(~subject)

dd = dseen %>%
  left_join(dchosen,by=c("subject","story","trial","ResponseType"))
nrow(dd)
head(dd)

agr = dd %>%
  group_by(subject,story) %>%
  summarize(SumChosen=sum(Chosen)) %>%
  group_by(subject) %>%
  summarize(MeanChosen=mean(SumChosen),CILow=ci.low(SumChosen),CIHigh=ci.high(SumChosen)) %>%
  ungroup() %>%
  mutate(YMin=MeanChosen-CILow,YMax=MeanChosen+CIHigh)

ggplot(agr, aes(x=reorder(subject,MeanChosen),y=MeanChosen)) +
  geom_bar(stat="identity",color="black",fill="gray80") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25)

agr = dd %>%
  filter(Chosen==1) %>%
  group_by(subject) %>%
  mutate(count = n())

ggplot(agr, aes(x=reorder(subject,count),fill=ResponseType)) +
  geom_histogram(stat="count") #+


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

names(d)

d$alternatives = paste(d$tag_a,d$tag_b,d$tag_c)
tmp = d %>%
  # rowwise() %>%
  mutate(alternatives=paste(sort(c(tag_a,tag_b,tag_c))))


dchosen = d %>%
  filter(trial %in% c("high","low")) %>%
  select(alternatives,subject,story,trial,exhaustive_chosen,mention_one_high_chosen,mention_one_low_chosen,mention_some_high_chosen,mention_some_low_chosen,false_report_chosen,none_of_above_chosen) %>%
  gather(ResponseTypeChosen,Chosen,-subject,-story,-trial,-alternatives) %>%
  mutate(ResponseType=gsub("_chosen","",ResponseTypeChosen,fixed=T)) %>%
  select(-ResponseTypeChosen)

agr = dchosen %>%
  group_by(trial,ResponseType,alternatives) %>%
  summarize(Mean=mean(Chosen),CILow=ci.low(Chosen),CIHigh=ci.high(Chosen)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
dodge=position_dodge(.9)

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
#Kruskal-Wallis chi-squared = 8.0583, df = 1, p-value = 0.00453
kruskal.test(trial ~ mention_one_high_chosen, data = test) #YES
#Kruskal-Wallis chi-squared = 21.271, df = 1, p-value = 3.987e-06
kruskal.test(trial ~ mention_some_high_chosen, data = test) #no
#Kruskal-Wallis chi-squared = 0.0034483, df = 1, p-value = 0.9532
kruskal.test(trial ~ mention_one_low_chosen, data = test) #no
#Kruskal-Wallis chi-squared = 1.0765, df = 1, p-value = 0.2995
kruskal.test(trial ~ mention_some_low_chosen, data = test) #no
#Kruskal-Wallis chi-squared = 0.1406, df = 1, p-value = 0.7077

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


