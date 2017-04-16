# balance table

#create dtaa frame of only the variables of interest
remove = c("hh_id", "observation_id")
varlist <- colnames(afghan[,!names(afghan) %in% remove])
balance_variables <-afghan[,!colnames(afghan) %in% remove]

#generate counts
n_treatment <- apply(balance_variables[balance_variables$treatment == 1,], 2, function(x) length(which(!is.na(x))))
n_control <- apply(balance_variables[balance_variables$treatment == 0,], 2, function(x) length(which(!is.na(x))))

#generate table
balancetable <-cbind(n_control,n_treatment)
#drop treatment row
balancetable<-balancetable[!rownames(balancetable) == "treatment", ]

#run t.tests, skipping treatment[14]
balance_tests <- lapply(varlist[c(1:13,15:23)], function(x) {
  t.test(as.formula(paste(x,"treatment",sep="~")), data = balance_variables
         , alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
})

#extract and adjust p vals
balance_test_pvals <- t(sapply(balance_tests, function(x) {
  c(x$estimate[],
    "diff in means" =unname(x$estimate[1])-unname(x$estimate[2]),
    ci.lower = x$conf.int[1],
    ci.upper = x$conf.int[2],
    p.value = x$p.value,
    "adj.p.value" = p.adjust(x$p.value, method = "bonferroni", n = length(x)),
    x$parameter
  )
}))

balance_test_pvals <- data.frame(balance_test_pvals, stringsAsFactors=FALSE)
balance_test_pvals[] <- lapply(balance_test_pvals, function(x) as.numeric(as.character(x)))
balancetable<-cbind(balancetable,balance_test_pvals)
balancetable<-round(balancetable,3)
balancetable

