# Processing Stakes from MTuk/IbexFarm

library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(bootstrap)
library(lme4)
library(tidyverse)

theme_set(theme_bw())

#Set the working directory to whereever you have your raw data and the "helpers.R" file
setwd("/Users/morganmoyer/Dropbox/morgan_scripts/Stakes/processing/")


#############
# have to remove the NST and Ginz files
#Import the processed data file in
p <- read.csv("processed_results.csv", header=FALSE)
View(p)

#remove the nst and ginzburg trials
s <- subset(p, V5 != "nst")
ps <- subset(s, V5 != "ginz")
View(ps)

ginz <- subset(p, v5 = "ginz")
write.csv(ginz, file = "ginz.csv")

# for the past tense study, remove the following subjects because their browswer
# did not print the answer

t <- subset(ps, V1 != "A1RATFICCKLCQ")

View(t)

#Write to a file so can then run the filter.py script
write.csv(t, file = "past.csv")

#########################
######for the present tense
pr <- read.csv("processed_results.csv", header = FALSE)
View(pr)
#remove the nst and ginzburg trials
s <- subset(pr, V5 != "nst")
ps <- subset(s, V5 != "ginz")
#write the Ginz files to CSV
ginz <- subset(pr, v5 = "ginz")
write.csv(ginz, file = "ginz_present.csv")

#remove the subjects whose browsers aren't reporting the answers
t <- subset(ps, V1 != "A2TKHXY755FAM")
tt <- subset(t, V1 != "A2Z210D88A29PT")
ttt <- subset(tt, V1 != "A2VGEE0ZIZ8CBQ")


#Write to a file so can then run the filter.py script
write.csv(ttt, file = "present.csv")
