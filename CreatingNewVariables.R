#hello, this is a file

#clean variable names
#only run this once
install.packages("plyr")
library(plyr)
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


#ratios
#sheep per household member (proxy for wealth?)
afghan$sheep_per_hh_member = afghan$num_sheep / afghan$num_ppl_hh
