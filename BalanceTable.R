# balance table

#create dtaa frame of only the variables of interest
afghan_names <- colnames(afghan)
remove = c("hh_id", "observation_id")
afghan_names <- colnames(afghan[,!colnames(afghan) %in% remove])
afghan_variables <-afghan[,!colnames(afghan) %in% remove]

#calculate means
means_treatment <- colMeans(afghan_variables[afghan_variables$treatment == 1,], na.rm = FALSE)
means_control <- colMeans(afghan_variables[afghan_variables$treatment == 0,], na.rm = FALSE)

#generate table
balancetable <-cbind(means_control, means_treatment)
balancetable
