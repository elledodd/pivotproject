#installs - run only if needed
#install.packages("plyr")
# install.packages("lfe")
# install.packages("stargazer")
# install.packages("plm")
# install.packages("sandwich")

#libraries
library(plyr)
library(sandwich)
library(lfe)
library(stargazer)
library(plm)
library(haven)
library(car)
library(knitr)
library(gtools)

# load necessary packages for importing the function
library(RCurl)

# import the function from repository
url_robust <- “https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R”
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)


#clean variable names
#only run this once
afghan <- afghandata
afghan <- rename(afghan, c("f07_hh_id"= "hh_id",
                           "f07_heads_child_cnt" = "heads_child",
                           "f07_girl_cnt" = "girl",
                           "f07_age_head_cnt" ="age_head",
                           "f07_yrs_ed_head_cnt" = "yrs_ed_head", 
                           "f07_jeribs_cnt" = "jeribs", 
                           "f07_num_sheep_cnt" = "num_sheep", 
                           "f07_duration_village_cnt" = "duration_village", 
                           "f07_farsi_cnt" = "farsi",
                           "f07_tajik_cnt" = "tajik", 
                           "f07_farmer_cnt"    = "farmer",
                           "f07_num_ppl_hh_cnt"   = "num_ppl_hh",
                           "f07_test_observed" = "test_observed",
                           "f07_formal_school" = "formal_school",
                           "f07_nearest_scl" = "nearest_scl",
                           
                           # non-matching new names#                 
                           "f07_age_cnt" = "age_child",
                           "f07_both_norma_total" = "test_score_normalized"
))

#log variables

#interaction terms
#language by farmer
#age by gender
afghan$age_girl <- afghan$age_child*afghan$girl
afghan$age_girl[afghan$age_girl  == 0] <- NA
afghan$age_boy <-  afghan$age_child* !afghan$girl
afghan$age_boy[afghan$age_boy  == 0] <- NA
#ratios
#sheep per household member (proxy for wealth?)
afghan$sheep_per_hh_member = afghan$num_sheep / afghan$num_ppl_hh

attach(afghan)


##############################################################################
#Q1 - Create Balance Table                                                   #
##############################################################################

#create dtaa frame of only the variables of interest
remove <- c("hh_id", "observation_id")
varlist <- colnames(afghan[,!names(afghan) %in% remove])
balance_variables <-afghan[,!colnames(afghan) %in% remove]

#generate counts
n_ctrl <- apply(balance_variables[balance_variables$treatment == 0,], 2, function(x) length(which(!is.na(x))))
n_trt <- apply(balance_variables[balance_variables$treatment == 1,], 2, function(x) length(which(!is.na(x))))

#generate table
balancetable <-cbind(n_ctrl,n_trt)
#drop treatment, test score, cluster rows
balancetable<-balancetable[!rownames(balancetable) == "treatment", ]
balancetable<-balancetable[!rownames(balancetable) == "test_score_normalized", ]
balancetable<-balancetable[!rownames(balancetable) == "clustercode", ]


#run t.tests, skipping treatment[14]
varlist
balance_tests <- lapply(varlist[c(1:13,16:18, 20:22)], function(x) {
  t.test(as.formula(paste(x,"treatment",sep="~")), data = balance_variables
         , alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
})

#extract and adjust p vals
balance_test_pvals <- t(sapply(balance_tests, function(x) {
  c("mean_crtl" = unname(x$estimate[1]),
    "mean_trt" = unname(x$estimate[2]),
    "diff_means" =unname(x$estimate[2])-unname(x$estimate[1]),
    "adj.p.value" = p.adjust(x$p.value, method = "bonferroni", n = length(x))
  )
}))

balance_test_pvals <- data.frame(balance_test_pvals, stringsAsFactors=FALSE)
balance_test_pvals[] <- lapply(balance_test_pvals[], function(x) as.numeric(as.character(x)))
balancetable<-cbind(balancetable,balance_test_pvals)
balancetable<-round(balancetable,3)
balancetable
  
##############################################################################
#Q2 - Create Attrition Table                                                 #
##############################################################################

#summary stats
sum_list <- list(afghan$test_observed,
                 afghan$test_observed[treatment == 1],
                 afghan$test_observed[treatment == 0]
)
n <- sapply(sum_list, function(x) length(which(!is.na(x))))
mean <- sapply(sum_list, mean, na.rm = T)
sd <- sapply(sum_list, sd, na.rm = T)
sum_table<-cbind(n,mean,sd)
sum_table<-round(sum_table,digits=3)
rownames(sum_table) <- c('% Test Taken - All','% Test Taken - Treatment', '% Test Taken - Control')
sum_table

#creating a dataset of only the attritted
afghanattrition <- afghan[!complete.cases(afghan),]

#########################################################
#comparisons of treatment and control for attritted only#
#########################################################
#omits test_observed[13], treatment[14], clustercode[15], and test_score[19]
varlist
attrition_by_treatment <-lapply(varlist[c(1:12,16:18,20:22)], function(x) {
  t.test(as.formula(paste(x,"treatment",sep="~")), data = afghanattrition
         , alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
})

#create table
attrition_table <- t(sapply(attrition_by_treatment, function(x) {
  c(x$data.name,
    "mean_crtl" = unname(x$estimate[1]),
    "mean_trt" = unname(x$estimate[2]),
    "diff_means" =unname(x$estimate[2])-unname(x$estimate[1]),
    "p.value" = p.adjust(x$p.value, method = "bonferroni", n = length(x))
  )
}))
rownames(attrition_table) <- attrition_table[,1]
attrition_table <- attrition_table[,-c(1)]
attrition_table <- data.frame(attrition_table, stringsAsFactors=FALSE)
attrition_table[] <- lapply(attrition_table, function(x) as.numeric(as.character(x)))
rownames(attrition_table) <- varlist[c(1:12,16:18,20:22)]
attrition_table

##############################################################################
#Q3 -  Baseline Enrollment                                                   #
##############################################################################




##############################################################################
#Q4 - Household fixed effects regression for school enrollment on test score #
##############################################################################
MAKE SURE SAME LIST OF VARIABLES

regschoolontest <- lm(test_score_normalized ~ 
                        formal_school + 
                        heads_child + 
                        girl + 
                        age_head + 
                        yrs_ed_head + 
                        jeribs + 
                        num_sheep + 
                        duration_village + 
                        farsi + 
                        tajik + 
                        farmer + 
                        num_ppl_hh + 
                        nearest_scl + 
                        age_child + 
                        tajik_farmer + 
                        age_child_girl +
                        heads_child_girl +
                        sheep_per_hh_member)

regschoolontestFE <- lm(test_score_normalized ~ 
                            formal_school + 
                            heads_child + 
                            girl + 
                            age_child + 
                            age_child_girl +
                            heads_child_girl +
                            as.factor(hh_id))

linearHypothesis(regschoolontestFE,c("heads_child_girl = 0","heads_child = 0"),test="F")

robust_se.sumregschoolontest <- summary(regschoolontest, robust = T)$coefficients[,2]
robust_se.sumregschoolontestFE <- summary(regschoolontestFE, robust = T)$coefficients[,2]
stargazer(regschoolontest,regschoolontestFE, title="Results", align=TRUE, type = "text",
          se = list(robust_se.sumregschoolontest, robust_se.sumregschoolontestFE),
          omit = "hh_id",
          omit.labels = "HH ID Fixed Effects?")



#########################################################################
#Part 5 - Treatment effect and differences in treatment effect by gender#
#########################################################################

#enroll~ treatment
treat_enrollment <- lm( formal_school ~ treatment, data = afghan)
treat_enrollment_girl <- lm( formal_school ~ treatment + treatment*girl, 
                             data = afghan)
treat_enrollment.adv <- lm( formal_school ~ treatment 
                            + nearest_scl + girl + age_child + age_head 
                            + num_sheep + jeribs + yrs_ed_head 
                            + heads_child+duration_village 
                            + num_ppl_hh + tajik + farsi , data = afghan)

treat_enrollment_girl.adv <- lm( formal_school ~ treatment + treatment*girl 
                             + nearest_scl + age_child + age_head 
                             + num_sheep + jeribs + yrs_ed_head 
                             + heads_child+duration_village 
                             + num_ppl_hh + tajik + farsi , data = afghan)
treat_enrollment.adv_clus <- lm( formal_school ~ treatment 
                            + nearest_scl + girl + age_child + age_head 
                            + num_sheep + jeribs + yrs_ed_head 
                            + heads_child+duration_village 
                            + num_ppl_hh + tajik + farsi + as.factor(clustercode)
                            , data = afghan)

treat_enrollment_girl.adv_clus <- lm( formal_school ~ treatment + treatment*girl 
                                 + nearest_scl + age_child + age_head 
                                 + num_sheep + jeribs + yrs_ed_head 
                                 + heads_child+duration_village 
                                 + num_ppl_hh + tajik + farsi + as.factor(clustercode)
                                 , data = afghan)

robust_se.treat_enrollment <- sqrt(diag(vcovHC(treat_enrollment, type = "HC1")))
robust_se.treat_enrollment_girl <- sqrt(diag(vcovHC(treat_enrollment_girl, type = "HC1")))
robust_se.treat_enrollment.adv <- sqrt(diag(vcovHC(treat_enrollment.adv, type = "HC1")))
robust_se.treat_enrollment_girl.adv <- sqrt(diag(vcovHC(treat_enrollment_girl.adv, type = "HC1")))
robust_se.treat_enrollment.adv_clus <- sqrt(diag(vcovHC(treat_enrollment.adv_clus, type = "HC1")))
robust_se.treat_enrollment_girl.adv_clus <- sqrt(diag(vcovHC(treat_enrollment_girl.adv_clus, type = "HC1")))

stargazer(treat_enrollment,treat_enrollment_girl,treat_enrollment.adv,treat_enrollment_girl.adv
          ,treat_enrollment.adv_clus,treat_enrollment_girl.adv_clus, title="Results"
          , align=TRUE, type = "text",omit = "clustercode", omit.labels = "Cluster Fixed Effects?",
          se = list(robust_se.treat_enrollment,robust_se.treat_enrollment_girl,robust_se.treat_enrollment.adv,robust_se.treat_enrollment_girl.adv
                    ,robust_se.treat_enrollment.adv_clus,robust_se.treat_enrollment_girl.adv_clus)
          )

#test score ~ treatment
treat_test <- (lm( test_score_normalized ~ treatment, data = afghan))
treat_test_girl <- (lm( test_score_normalized ~ treatment + treatment*girl
                        , data = afghan))
treat_test.adv <- (lm( test_score_normalized ~ treatment+ nearest_scl 
                       + girl + age_child + age_head 
                       + num_sheep + jeribs + yrs_ed_head 
                       + heads_child+duration_village 
                       + num_ppl_hh + tajik + farsi , data = afghan))
treat_test_girl.adv <- (lm( test_score_normalized ~ treatment + treatment*girl
                            + nearest_scl + age_child + age_head 
                            + num_sheep + jeribs + yrs_ed_head 
                            + heads_child+duration_village 
                            + num_ppl_hh + tajik + farsi, data = afghan))
treat_test.adv_clus <- (lm( test_score_normalized ~ treatment+ nearest_scl 
                       + girl + age_child + age_head 
                       + num_sheep + jeribs + yrs_ed_head 
                       + heads_child+duration_village 
                       + num_ppl_hh + tajik + farsi + as.factor(clustercode)
                       , data = afghan))
treat_test_girl.adv_clus <- (lm( test_score_normalized ~ treatment + treatment*girl
                            + nearest_scl + age_child + age_head 
                            + num_sheep + jeribs + yrs_ed_head 
                            + heads_child+duration_village 
                            + num_ppl_hh + tajik + farsi+ as.factor(clustercode)
                            , data = afghan))
robust_se.treat_test <- sqrt(diag(vcovHC(treat_test, type = "HC1")))
robust_se.treat_test_girl <- sqrt(diag(vcovHC(treat_test_girl, type = "HC1")))
robust_se.treat_test.adv <- sqrt(diag(vcovHC(treat_test.adv, type = "HC1")))
robust_se.treat_test_girl.adv <- sqrt(diag(vcovHC(treat_test_girl.adv, type = "HC1")))
robust_se.treat_test.adv_clus <- sqrt(diag(vcovHC(treat_test.adv_clus, type = "HC1")))
robust_se.treat_test_girl.adv_clus <- sqrt(diag(vcovHC(treat_test_girl.adv_clus, type = "HC1")))

stargazer(treat_test,treat_test_girl,treat_test.adv,treat_test_girl.adv,treat_test.adv_clus,treat_test_girl.adv_clus, title="Results", align=TRUE, type = "text",
se = list(robust_se.treat_test,robust_se.treat_test_girl,robust_se.treat_test.adv,robust_se.treat_test_girl.adv
          ,robust_se.treat_test.adv_clus,robust_se.treat_test_girl.adv_clus)
          ,omit = "clustercode",
          omit.labels = "Cluster Fixed Effects?")

#########################################################################
#Part 6 - Local average treatment effect                                #
#########################################################################
## Question 6: 
## f07_both_norma_total  ~ f07_formal_school + f07_nearest_scl 
##                        + (f07_nearest_scl*f07_formal_school)

## Full sample
q6.full <- lm(test_score_normalized ~ formal_school * treatment, 
           data = afghan)
q6.boys <- lm(test_score_normalized ~ formal_school * treatment, 
           data = afghan, subset=(afghan$girl == 0))
q6.girls <-lm(test_score_normalized ~ formal_school * treatment, 
           data = afghan, subset=(afghan$girl == 1))

q6.full_se<- summary(q6.full, robust = T)$coefficients[,2]
q6.boys_se <- summary(q6.boys, robust = T)$coefficients[,2]
q6.girls_se <- summary(q6.girls, robust = T)$coefficients[,2]

#```{r, echo=TRUE, message=FALSE, warning=FALSE, results='asis'}
stargazer(q6.full, q6.boys, q6.girls, 
          se = list(q6.full_se, q6.boys_se, q6.girls_se),
          align=TRUE,
          title = "Regression Table",
          type = "text")
