library(readstata13)
setwd("~/Documents/Grad School/HKS/MPP/Course docs/Spring 2017 - API 202Z/Group project")
library(haven)
afghan <- read_dta("afghandata.dta")

names(afghan)

## Question 6: Local average treatment effect
## f07_both_norma_total  ~ f07_formal_school + f07_nearest_scl + (f07_nearest_scl*f07_formal_school)

afghan$enroll_dist <- afghan$f07_formal_school*afghan$f07_nearest_scl

## Full sample
summary(lm(afghan$f07_both_norma_total ~ afghan$f07_formal_school + afghan$f07_nearest_scl + afghan$enroll_dist), robust = T)

## Boys only
summary(lm(afghan$f07_both_norma_total ~ afghan$f07_formal_school + afghan$f07_nearest_scl + afghan$enroll_dist, subset=(afghan$f07_girl_cnt == 0)), robust = T)

## Girls only
summary(lm(afghan$f07_both_norma_total ~ afghan$f07_formal_school + afghan$f07_nearest_scl + afghan$enroll_dist, subset=(afghan$f07_girl_cnt == 1)), robust = T)
