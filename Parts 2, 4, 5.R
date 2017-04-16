# install.packages("lfe")
# install.packages("stargazer")
# install.packages("plm")

library(sandwich)
library(lfe)
library(stargazer)
library(plm)
attach(afghan)


####################
#Part 2 - attrition#
####################

#summary stats
sum_list <- list(afghan$test_observed,
                 afghan$test_observed[treatment == 1],
                 afghan$test_observed[treatment == 0],
                 afghan$test_score_normalized,
                 afghan$test_score_normalized[treatment == 1],
                 afghan$test_score_normalized[treatment ==0]
                 )
n <- sapply(sum_list, function(x) length(which(!is.na(x))))
mean <- sapply(sum_list, mean, na.rm = T)
sd <- sapply(sum_list, sd, na.rm = T)
min <- sapply(sum_list, min, na.rm = T)
median <- sapply(sum_list, median, na.rm = T)
max <- sapply(sum_list, max, na.rm = T)
sum_table<-cbind(n,mean,sd,min,median,max)
sum_table<-round(sum_table,digits=3)
rownames(sum_table) <- c('Test Taken','Test Taken for treatment', 'Test Taken for Control',
                         'Normalized Test Score', 'Test Score for Treatment', 'Test Score for control')
sum_table

#creates list of variable for passing to loops, skips ids
varlist <- names(afghan)[c(2:20, 22:25)]
#creating a dataset of only the attritted
afghanattrition <- afghan[!complete.cases(afghan),]

#########################################################
#comparisons of treatment and control for attritted only#
#########################################################
#omits treatment[14] and test_score[19]
attrition_by_treatment <-lapply(varlist[c(1:13,15:18,20:23)], function(x) {
    t.test(as.formula(paste(x,"treatment",sep="~")), data = afghanattrition
           , alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
})
#create table
attrition_table <- t(sapply(attrition_by_treatment, function(x) {
            c(x$data.name,
              x$estimate,
              ci.lower = x$conf.int[1],
              ci.upper = x$conf.int[2],
              p.value = x$p.value,
              "adj.p.value" = p.adjust(x$p.value, method = "bonferroni", n = length(x)),
              x$parameter
            )
}))
rownames(attrition_table) <- attrition_table[,1]
attrition_table <- attrition_table[,-c(1)]
attrition_table <- data.frame(attrition_table, stringsAsFactors=FALSE)
attrition_table[] <- lapply(attrition_table, function(x) as.numeric(as.character(x)))
attrition_table<-format(round(attrition_table,3),digits =1)
attrition_table

############################################
#attritors vs compliants for treatment only#
############################################
#omits test_observed[13] and treatment[14] and test_score[19]
attrition_by_test_observed <- lapply(varlist[c(1:12,15:18,20:23)], function(x) {
  t.test(as.formula(paste(x,"test_observed",sep="~")), data =subset(afghan,treatment==1)
         ,  alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)})
#create table and adujst p values
attrition_by_treatment_table <- t(sapply(attrition_by_test_observed, function(x) {
  c(x$data.name,
    x$estimate,
    ci.lower = x$conf.int[1],
    ci.upper = x$conf.int[2],
    p.value = x$p.value,
    "adj.p.value" = p.adjust(x$p.value, method = "bonferroni", n = length(x)),
    x$parameter
  )
}))
rownames(attrition_by_treatment_table) <- attrition_by_treatment_table[,1]
attrition_by_treatment_table <- attrition_by_treatment_table[,-c(1)]
attrition_by_treatment_table <- data.frame(attrition_by_treatment_table, stringsAsFactors=FALSE)
attrition_by_treatment_table[] <- lapply(attrition_by_treatment_table, function(x) as.numeric(as.character(x)))
attrition_by_treatment_table<-format(round(attrition_by_treatment_table,3),digits =1)

attrition_by_treatment_table

########################################
#attritors vs compliants for girls only#
########################################
#omits girl[2], test_observed[13] and test_score[19]
attrition_for_girls <- lapply(varlist[c(1,3:12,15:18,20:23)], function(x) {
        t.test(as.formula(paste(x,"test_observed",sep="~")), data = subset(afghan,girl==1)
               , alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
     })

#create table
girl_attrition_table <- t(sapply(attrition_for_girls, function(x) {
  c(x$data.name,
    x$estimate,
    ci.lower = x$conf.int[1],
    ci.upper = x$conf.int[2],
    p.value = x$p.value,
    "adj.p.value" = p.adjust(x$p.value, method = "bonferroni", n = length(x)),
    x$parameter
  )
}))
rownames(girl_attrition_table) <- girl_attrition_table[,1]
girl_attrition_table <- girl_attrition_table[,-c(1)]
girl_attrition_table <- data.frame(girl_attrition_table, stringsAsFactors=FALSE)
girl_attrition_table[] <- lapply(girl_attrition_table, function(x) as.numeric(as.character(x)))
girl_attrition_table<-format(round(girl_attrition_table,3),digits =1)
girl_attrition_table



#################################################################################
#Part 4 - Household fixed effects regression for school enrollment on test score#
#################################################################################

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

regschoolontestFE <- felm(test_score_normalized ~ 
                            formal_school + 
                            heads_child + 
                            girl + 
                            age_child + 
                            age_child_girl +
                            heads_child_girl
                          | hh_id)


robust_se.sumregschoolontest <- summary(regschoolontest, robust = T)$coefficients[,2]
robust_se.sumregschoolontestFE <- summary(regschoolontestFE, robust = T)$coefficients[,2]
stargazer(regschoolontest,regschoolontestFE, title="Results", align=TRUE, type = "text",se = list(robust_se.sumregschoolontest, robust_se.sumregschoolontestFE))



#########################################################################
#Part 5 - Treatment effect and differences in treatment effect by gender#
#########################################################################

treat_enrollment <- lm( formal_school ~ treatment, data = afghan)
treat_enrollment_girl <- lm( formal_school ~ treatment + treatment*girl, data = afghan)
treat_test <- (lm( test_score_normalized ~ treatment, data = afghan))
treat_test_girl <- (lm( test_score_normalized ~ treatment + treatment*girl, data = afghan))
robust_se.treat_enrollment <- summary(treat_enrollment, robust = T)$coefficients[,2]
robust_se.treat_enrollment_girl<- summary(treat_enrollment_girl, robust = T)$coefficients[,2]
robust_se.treat_test <- summary(treat_test, robust = T)$coefficients[,2]
robust_se.treat_test_girl <- summary(treat_test_girl, robust = T)$coefficients[,2]
stargazer(treat_enrollment,treat_enrollment_girl,treat_test,treat_test_girl, title="Results", align=TRUE, type = "text",
          se = list(robust_se.treat_enrollment, robust_se.treat_enrollment_girl,robust_se.treat_test,robust_se.treat_test_girl))
