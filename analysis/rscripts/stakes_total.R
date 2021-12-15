library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(bootstrap)
library(lme4)
library(tidyverse)

setwd("/Users/morganmoyer/Dropbox/morgan_scripts/Stakes/processing/")
source("helpers.R")
f = read.csv("stakes_finite.csv")
nf = read.csv("stakes_nonfinite.csv")

f = f %>%
  mutate(nonfinite = "0")

nf = nf %>%
  mutate(nonfinite = "1") %>%
  filter(ResponseType %in% c("MA", "MS", "MO"))
View(nf)

df = bind_rows(f, nf)

df = df %>%
  mutate(ResponseType = as.factor(ResponseType)) %>%
  mutate(trial = as.factor(trial)) %>%
  mutate(story = as.factor(story)) %>%
  mutate(Informativity = as.factor(Informativity)) %>%
  mutate(nonfinite = as.factor(nonfinite)) %>%
  mutate(subject = as.factor(subject)) %>%
  select(-c("X"))
  # filter(Informativity %in% c("high", "low", "NA"))

#########################################################
############# OVERALL MAIN EFFECTS
#########################################################
str(df)

kruskal.test(Chosen ~ ResponseType, data = df)
# Kruskal-Wallis chi-squared = 14.626, df = 2, p-value = 0.0006667

pairwise.wilcox.test(df$Chosen, df$ResponseType, p.adjust.method = "BH")

#               exhaustive_NA mention_one
#   mention_one  0.00142       -          
#   mention_some 0.00046       0.47599


kruskal.test(Chosen ~ Informativity, data = df)
# Kruskal-Wallis chi-squared = 664.8, df = 1, p-value < 2.2e-16

kruskal.test(Chosen ~ nonfinite, data = df)
# Kruskal-Wallis chi-squared = 19.547, df = 1, p-value = 9.815e-06



############ OVERALL INTERACTIONS

##### ANSWER AND FINITENESS
interRF = interaction(df$ResponseType, df$nonfinite)
kruskal.test(Chosen ~ interRF, data = df)
# Kruskal-Wallis chi-squared = 52.362, df = 5, p-value = 4.548e-10

pairwise.wilcox.test(df$Chosen,interRF, p.adjust.method = "BH")
  #                 exhaustive_NA.0 mention_one.0 mention_some.0 exhaustive_NA.1 mention_one.1
  # mention_one.0   2.3e-11         -             -              -               -            
  # mention_some.0  2.3e-07         0.023         -              -               -            
  # exhaustive_NA.1 0.143           < 2e-16       1.3e-13        -               -            
  # mention_one.1   6.0e-05         9.5e-06       0.043          5.9e-10         -            
  # mention_some.1  < 2e-16         3.0e-07       1.7e-12        < 2e-16         < 2e-16  

##### ANSWER AND INFORMATIVITY
interRI = interaction(df$ResponseType, df$Informativity)
kruskal.test(Chosen ~ interRI, data = df)
# Kruskal-Wallis chi-squared = 667.2, df = 3, p-value < 2.2e-16
pairwise.wilcox.test(df$Chosen,interRI, p.adjust.method = "BH")
#                     mention_one.high mention_some.high mention_one.low
#   mention_some.high 0.17             -                 -              
#   mention_one.low   <2e-16           <2e-16            -              
#   mention_some.low  <2e-16           <2e-16            0.42  


##### ANSWER, INFORMATIVITY, FINITENESS
interRAI = interaction(df$ResponseType, df$Informativity, df$nonfinite)
kruskal.test(Chosen ~ interRAI, data = df)
# Kruskal-Wallis chi-squared = 735.62, df = 7, p-value < 2.2e-16




m1 = glmer(Chosen ~ nonfinite + ResponseType + Informativity + (1|subject), family="binomial", data = df)
summary(m1)

m2 = glmer(Chosen ~ ResponseType*Informativity + (1|subject), family = "binomial", data = df)
summary(m2)

#########################################################
############ More fine-grained comparisons In SUB GROUPS
#########################################################

############ Finite
fin = subset(df, df$nonfinite=="0")

kruskal.test(Chosen ~ ResponseType, data = fin)
# Kruskal-Wallis chi-squared = 5.2099, df = 1, p-value = 0.02246

kruskal.test(Chosen ~ Informativity, data = fin)
# Kruskal-Wallis chi-squared = 157.43, df = 1, p-value < 2.2e-16

interFRI = interaction(fin$ResponseType, fin$Informativity)
kruskal.test(Chosen ~ interFRI, data = fin)
# Kruskal-Wallis chi-squared = 170.44, df = 3, p-value < 2.2e-16

kruskal.test(Chosen ~ story, data = fin)
# Kruskal-Wallis chi-squared = 6.3232, df = 4, p-value = 0.1763

############ Non-Finite
nonfin = subset(df, df$nonfinite=="1")

kruskal.test(Chosen ~ ResponseType, data = nonfin)
# Kruskal-Wallis chi-squared = 16.675, df = 2, p-value = 0.0002393

kruskal.test(Chosen ~ Informativity, data = nonfin)
# Kruskal-Wallis chi-squared = 547.26, df = 1, p-value < 2.2e-16

kruskal.test(Chosen ~ trial, data = nonfin)
# Kruskal-Wallis chi-squared = 0.84772, df = 1, p-value = 0.3572

interNTR = interaction(nonfin$ResponseType, nonfin$trial)
kruskal.test(Chosen ~ interNTR, data = nonfin)
# Kruskal-Wallis chi-squared = 49.003, df = 5, p-value = 2.217e-09

pairwise.wilcox.test(nonfin$Chosen,interNTR, p.adjust.method = "BH")
  #         MA.high MO.high MS.high MA.low MO.low
  # MO.high 1.2e-10 -       -       -      -     
  # MS.high 1.4e-07 0.0785  -       -      -     
  # MA.low  5.0e-06 0.3445  0.8694  -      -     
  # MO.low  5.0e-06 0.0031  0.3357  0.4193 -     
  # MS.low  8.5e-07 0.0196  0.6096  0.6096 0.6096


interNTRI = interaction(nonfin$ResponseType, nonfin$trial, nonfin$Informativity)
kruskal.test(Chosen ~ interNTRI, data = nonfin)
# Kruskal-Wallis chi-squared = 569.47, df = 7, p-value < 2.2e-16

pairwise.wilcox.test(nonfin$Chosen,interNTRI, p.adjust.method = "BH")
  #             MO.high.high MS.high.high MO.low.high MS.low.high MO.high.low MS.high.low MO.low.low
  # MS.high.high 0.031        -            -           -           -           -           -         
  # MO.low.high  4.6e-06      0.017        -           -           -           -           -         
  # MS.low.high  3.1e-05      0.046        0.719       -           -           -           -         
  # MO.high.low  < 2e-16      < 2e-16      < 2e-16     < 2e-16     -           -           -         
  # MS.high.low  < 2e-16      < 2e-16      < 2e-16     < 2e-16     0.719       -           -         
  # MO.low.low   < 2e-16      < 2e-16      < 2e-16     < 2e-16     0.671       0.417       -         
  # MS.low.low   < 2e-16      < 2e-16      < 2e-16     < 2e-16     0.994       0.719       0.671 

# The interaction between MS/MO and informativity removes the MA trials.
# Now we want to bring MA in to comapare with informativity of MO/MS
nonfin_new = nonfin %>%
  unite(new, ResponseType:Informativity, sep="_")

kruskal.test(Chosen ~ new, data = nonfin_new)
# Kruskal-Wallis chi-squared = 567.32, df = 4, p-value < 2.2e-16

pairwise.wilcox.test(nonfin_new$Chosen, nonfin_new$new, p.adjust.method = "BH")
  #         MA_NA   MO_high MO_low  MS_high
  # MO_high 0.0012  -       -       -      
  # MO_low  < 2e-16 < 2e-16 -       -      
  # MS_high 1.3e-05 0.2294  < 2e-16 -      
  # MS_low  < 2e-16 < 2e-16 0.4970  < 2e-16

interNT = interaction(nonfin_new$new, nonfin_new$trial)
kruskal.test(Chosen ~ interNT, data = nonfin_new)
# Kruskal-Wallis chi-squared = 609.48, df = 9, p-value < 2.2e-16
pairwise.wilcox.test(nonfin_new$Chosen,interNT, p.adjust.method = "BH")
  #             MA_NA.high MO_high.high MO_low.high MS_high.high MS_low.high MA_NA.low MO_high.low MO_low.low
  # MO_high.high 0.0151     -            -           -            -           -         -           -         
  # MO_low.high  < 2e-16    < 2e-16      -           -            -           -         -           -         
  # MS_high.high 0.5445     0.0293       < 2e-16     -            -           -         -           -         
  # MS_low.high  < 2e-16    < 2e-16      0.7072      < 2e-16      -           -         -           -         
  # MA_NA.low    2.6e-06    0.0058       6.3e-07     1.1e-06      2.6e-06     -         -           -         
  # MO_high.low  0.2048     4.3e-06      < 2e-16     0.0153       < 2e-16     2.5e-12   -           -         
  # MO_low.low   < 2e-16    < 2e-16      0.6309      < 2e-16      0.3876      7.2e-08   < 2e-16     -         
  # MS_high.low  0.3586     3.0e-05      < 2e-16     0.0440       < 2e-16     4.0e-11   0.7092      < 2e-16   
  # MS_low.low   < 2e-16    < 2e-16      0.9938      < 2e-16      0.7072      4.3e-07   < 2e-16     0.6309    
 
 #              MS_high.low
  # MO_high.high -          
  # MO_low.high  -          
  # MS_high.high -          
  # MS_low.high  -          
  # MA_NA.low    -          
  # MO_high.low  -          
  # MO_low.low   -          
  # MS_high.low  -          
  # MS_low.low   < 2e-16    





agrr = nonfin %>%
  group_by(story,trial,ResponseType,Chosen) %>%
  summarize(SumChosen=sum(Chosen)) %>%
  group_by(ResponseType) %>%
  drop_na()

seenagr = nonfin %>%
  drop_na() %>%
  group_by(story,trial,ResponseType,Seen) %>%
  summarize(SumSeen=sum(Seen)) %>%
  group_by(ResponseType)

new <- agrr %>% 
  left_join(seenagr) %>%
  drop_na() %>%
  summarise(SumChosen=sum(SumChosen),SumSeen=sum(SumSeen)) %>%
  mutate(proportion = SumChosen / SumSeen)

ggplot(new, aes(ResponseType, y=proportion)) +
  geom_bar(position=dodge,stat="identity") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  ylab("Proportion chosen") +
  xlab("AnswerType") +

  ylim(0,1)

############ Low Stakes
low = df %>%
  filter(trial %in% c("low","test"))

kruskal.test(Chosen ~ story, data = low)
# Kruskal-Wallis chi-squared = 38.807, df = 9, p-value = 1.248e-05

kruskal.test(Chosen ~ nonfinite, data = low)
# Kruskal-Wallis chi-squared = 25.004, df = 1, p-value = 5.721e-07

kruskal.test(Chosen ~ ResponseType, data = low)
# Kruskal-Wallis chi-squared = 4.6005, df = 2, p-value = 0.1002

interLNR = interaction(low$ResponseType, low$nonfinite)
kruskal.test(Chosen ~ interLNR, data = low)
# Kruskal-Wallis chi-squared = 32.64, df = 5, p-value = 4.437e-06

low_new = low %>%
  unite(new, ResponseType:Informativity, sep="_")

interNLNR = interaction(low_new$new, low_new$nonfinite)
kruskal.test(Chosen ~ interNLNR, data=low_new)
# Kruskal-Wallis chi-squared = 549.5, df = 9, p-value < 2.2e-16
pairwise.wilcox.test(low_new$Chosen, interNLNR, p.adjust.method = "BH")
  #           MA_NA.0 MO_high.0 MO_low.0 MS_high.0 MS_low.0 MA_NA.1 MO_high.1 MO_low.1 MS_high.1
  # MO_high.0 1.6e-05 -         -        -         -        -       -         -        -        
  # MO_low.0  1.0e-06 < 2e-16   -        -         -        -       -         -        -        
  # MS_high.0 0.24836 0.00034   9.3e-13  -         -        -       -         -        -        
  # MS_low.0  9.7e-06 < 2e-16   0.62514  4.0e-11   -        -       -         -        -        
  # MA_NA.1   0.63406 0.00147   1.7e-06  0.62514   1.1e-05  -       -         -        -        
  # MO_high.1 < 2e-16 5.6e-07   < 2e-16  2.2e-15   < 2e-16  2.7e-12 -         -        -        
  # MO_low.1  4.8e-08 < 2e-16   0.16697  3.2e-13   0.06725  6.9e-08 < 2e-16   -        -        
  # MS_high.1 6.0e-15 5.2e-06   < 2e-16  8.2e-14   < 2e-16  4.0e-11 0.69341   < 2e-16  -        
  # MS_low.1  3.0e-07 < 2e-16   0.40806  2.0e-12   0.19933  4.0e-07 < 2e-16   0.62514  < 2e-16 

low_fin = subset(low, low$nonfinite=="0")
kruskal.test(Chosen ~ story, data = low_fin)
# Kruskal-Wallis chi-squared = 5.971, df = 4, p-value = 0.2013

low_nonfin = subset(low, low$nonfinite=="1")
kruskal.test(Chosen ~ story, data = low_nonfin)
# Kruskal-Wallis chi-squared = 7.9465, df = 4, p-value = 0.09356


############ High STakes
high = subset(df, df$trial=="high")

# bring the columns to together to compare MA with MO-max and MS-max
high_new = high %>%
  unite(new, ResponseType:Informativity, sep="_")



View(high_new)

