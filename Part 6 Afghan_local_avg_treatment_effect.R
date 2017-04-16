library(readstata13)
setwd("~/Documents/Grad School/HKS/MPP/Course docs/Spring 2017 - API 202Z/Group project")
library(haven)
afghan <- read_dta("afghandata.dta")

## Question 6: Local average treatment effect
## f07_both_norma_total  ~ f07_formal_school + f07_nearest_scl + (f07_nearest_scl*f07_formal_school)

## Full sample
summary(lm(f07_both_norma_total ~ f07_formal_school * f07_nearest_scl, data = afghan), robust = T)

## Boys only
summary(lm(f07_both_norma_total ~ f07_formal_school * f07_nearest_scl, data = afghan, subset=(afghan$f07_girl_cnt == 0)), robust = T)

## Girls only
summary(lm(f07_both_norma_total ~ f07_formal_school * f07_nearest_scl, data = afghan, subset=(afghan$f07_girl_cnt == 1)), robust = T)
