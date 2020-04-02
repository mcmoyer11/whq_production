#  Stats for Con_Prob
# Date: June 2, 2019
############################
# Stats for Likert Study
############################
library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(bootstrap)
library(ggpubr)
library(psych)
library(likert)
library(reshape2)
library(lme4)
library(languageR)
library(ordinal)
library(lmerTest)
library(FSA)
library(lattice)
library(boot)
library(rcompanion)
library(MASS)
library(Hmisc)
library(reshape2)
library(foreign)
# First, set the working directory
setwd("/Users/morganmoyer/Dropbox/Moyer_research/Embedded_Questions/Dissertation/Experiments/Production/")
source("helpers.R")


d <- read.csv("tagged_prod_test.csv", header = TRUE)
View(d)

nrow(d) #357
length(d$stakes=="low")
low = subset(d, d$stakes=="low")
nrow(low) #174
high = subset(d, d$stakes=="high")
nrow(high)

sc = ftable(table(d$clauseType, d$stakes))
sc
#             high low
# Finite       172 156
# Modal         11  17
# Non-Finite     0   1
# total high: 183
# total Low: 174

# High: finite vs. modal
prop.test(c(172,11), c(183,183), alternative = "two.sided", correct = FALSE)
# X-squared = 283.29, df = 1, p-value < 2.2e-16

# Low: finite vs. modal
prop.test(c(17,11), c(174,174), alternative = "two.sided", correct = FALSE)
# X-squared = 1.3982, df = 1, p-value = 0.237

# finite clauses sig dif across stakes?
prop.test(c(172,156), c(183,174), alternative = "two.sided", correct = FALSE)
# X-squared = 2.2447, df = 1, p-value = 0.1341

# modal sig dif. across stakes?
prop.test(c(11,17), c(183,174), alternative = "two.sided", correct = FALSE)
# X-squared = 2.5714, df = 1, p-value = 0.1088
# X-squared = 1.7438, df = 1, p-value = 0.1867

# wh
 
ftable(table(d$wh, d$stakes))
#         high low
# how      16   2
# what     31  47
# where    99 117
# which    35   8
# who       2   0

# how
prop.test(c(16,2), c(183,174), alternative = "two.sided", correct = FALSE)
# X-squared = 10.743, df = 1, p-value = 0.001047
# what
prop.test(c(31,47), c(183,174), alternative = "two.sided", correct = FALSE)
# X-squared = 5.2987, df = 1, p-value = 0.02134
# where
prop.test(c(99,117), c(183,174), alternative = "two.sided", correct = FALSE)
# X-squared = 6.4474, df = 1, p-value = 0.01111
# which
prop.test(c(35,8), c(183,174), alternative = "two.sided", correct = FALSE)
# X-squared = 17.77, df = 1, p-value = 2.493e-05
# who
prop.test(c(2,0), c(183,174))#, alternative = "two.sided", correct = FALSE)


# for the extra words, counts are gotten from the jupyter notebook
# can only do the X^2 test for ones where there's a high and low stakes count
# close
prop.test(c(3,11), c(183,174), alternative = "two.sided", correct = FALSE)
# X-squared = 5.1904, df = 1, p-value = 0.02271
# can
prop.test(c(3,16), c(183,174), alternative = "two.sided", correct = FALSE)
# X-squared = 10.106, df = 1, p-value = 0.001478

# local - can't compute
prop.test(c(1,6), c(183,174))#, alternative = "two.sided", correct = FALSE)

# near
prop.test(c(2,28), c(183,174), alternative = "two.sided", correct = FALSE)
# X-squared = 26.069, df = 1, p-value = 3.294e-07

# around
prop.test(c(2,30), c(183,174), alternative = "two.sided", correct = FALSE)
# X-squared = 28.504, df = 1, p-value = 9.353e-08


# d-linking

prop.test(c(2,30), c(183,174), alternative = "two.sided", correct = FALSE)


# Matrix verb and stakes
prop.test(c(6,13), c(19,19), alternative = "two.sided", correct = FALSE)
# X-squared = 5.1579, df = 1, p-value = 0.02314

mod_can = d %>%
  filter(clauseType %in% c("Modal")) %>%
  filter(emb_verb %in% c("can"))
View(mod_can)

length(mod_can)


hc = subset(mod_can, mod_can$stakes=="high")
lc = subset(mod_can, mod_can$stakes=="low")

nrow(hc) #3
nrow(lc) #15
nrow(mod_can) #

# 'can' differs high to low?
prop.test(c(3,15), c(18, 18), alternative = "two.sided", correct = FALSE)
# X-squared = 16, df = 1, p-value = 6.334e-05

# where props differ?
prop.test(c(99,117), c(216, 216), alternative = "two.sided", correct = FALSE)
# X-squared = 3, df = 1, p-value = 0.08326

# what
prop.test(c(31,47), c(78, 78), alternative = "two.sided", correct = FALSE)
# X-squared = 6.5641, df = 1, p-value = 0.01041

# which
prop.test(c(35,8), c(43, 43), alternative = "two.sided", correct = FALSE)
# X-squared = 33.907, df = 1, p-value = 5.781e-09

# how
prop.test(c(16,2), c(18, 18), alternative = "two.sided", correct = FALSE)
# X-squared = 21.778, df = 1, p-value = 3.061e-06




