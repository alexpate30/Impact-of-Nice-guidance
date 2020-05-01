### SET THE ROOT DIRECTORY

### SET THE ROOT DIRECTORY

library(dplyr)

### In this program we want to calculate a QRISK score using the online algorithm for each of the 20
### imputed datasets. That is, for every patient in our statin cohort, we calculate a QRISK3 score
### using the QRISK3 algorithm, these are the 'EHR derived risks' as referred to in the paper

### First load the imputed dataset
load("R_out_Vis2019/imputed_datasets_loaded.RData")

## First need to get data in right format

## Create a function convert, that will change the names of the variables into the names required 
## by the function defined in the previous program 'p2.1 qrisk3_calc_functions.R'
convert <- function(data.in){
  ## First want to rename all variables that we need
  data.in <- select(data.in,
                    patid = patid, age = age, gender = gender, b_AF = Atrialfib, b_atypicalantipsy = Atypical_antipsy_med,
                    b_corticosteroids = Corticosteroid_use, b_impotence2 = Erec_dysfunc, b_migraine = Migraine, b_ra = RA,
                    b_renal = CKD345, b_semi = Sev_men_ill_qrisk, b_sle = SLE, b_treatedhyp = Hypertension, b_type1 = T1dia,
                    b_type2 = T2dia, ethrisk = Ethnicity, fh_cvd = FamHis, rati = Cholesterol_HDL_Ratio, sbp = SBP, 
                    sbps5 = SBP_var, smoke_cat = Smoking, town = Townsend, bmi = BMI)
  
  ## Some categorical variables need re-leveling
  # Gender
  data.in$gender <- 2 - as.numeric(data.in$gender)

  # Ethnicity
  levels(data.in$ethrisk) <- c(5,4,7,6,8,2,9,3,1)
  data.in$ethrisk <- as.numeric(as.character(data.in$ethrisk))
  
  # Smoking
  data.in$smoke_cat <- as.numeric(data.in$smoke_cat) 
  
  # Townsend
  data.in$town <- as.numeric(as.character(data.in$town))
  
  # Assign the median value of townsend score associated with each quintile, as from the census 2011 data, this is because we do not
  # have raw townsend scores in CPRD
  # source: http://s3-eu-west-1.amazonaws.com/statistics.digitalresources.jisc.ac.uk/dkan/files/Townsend_Deprivation_Scores/UK%20Townsend%20Deprivation%20Scores%20from%202011%20census%20data.pdf
  data.in$town[data.in$town == 1] <- -3.724
  data.in$town[data.in$town == 2] <- -2.140
  data.in$town[data.in$town == 3] <- -0.557
  data.in$town[data.in$town == 4] <- 1.107
  data.in$town[data.in$town == 5] <- 4.217

  ## Now need to turn all categorical variablres into numeric
  data.in$b_atypicalantipsy <- as.numeric(data.in$b_atypicalantipsy) - 1
  data.in$b_AF <- as.numeric(data.in$b_AF) - 1
  data.in$b_corticosteroids <- as.numeric(data.in$b_corticosteroids) - 1
  data.in$b_migraine <- as.numeric(data.in$b_migraine) - 1
  data.in$b_ra <- as.numeric(data.in$b_ra) - 1
  data.in$b_renal <- as.numeric(data.in$b_renal) - 1
  data.in$b_semi <- as.numeric(data.in$b_semi) - 1
  data.in$b_sle <- as.numeric(data.in$b_sle) - 1
  data.in$b_treatedhyp <- as.numeric(data.in$b_treatedhyp) - 1
  data.in$b_type1 <- as.numeric(data.in$b_type1) - 1
  data.in$b_type2 <- as.numeric(data.in$b_type2) - 1
  data.in$fh_cvd <- as.numeric(data.in$fh_cvd) - 1

  ## Finally need to add the year at which we want to calculate risk for each patient (=10)
  data.in$surv <- 10
  
  return(data.in)
}

## Apply function to each imputed dataset (we have 20 from the multiple imputation procedure)
for (i in 1:20){long_data_parallel[[i]] <- convert(long_data_parallel[[i]])}


## Now each dataset should be ok to have risks generated using the functions defined in 'p2.1 qrisk3_calc_functions.R'
load("R_out_Vis2019/qrisk_calc_functions.RData")


## Lets do the women first
risks.out.female <-vector("list",20)
for (i in 1:20){risks.out.female[[i]] <- qrisk_calc_female(long_data_parallel[[i]])}

# Use Rubins rules to combine risk scores with log(-log()) transformation
risks.all.female <- cbind(risks.out.female[[1]]$score,risks.out.female[[2]]$score,risks.out.female[[3]]$score,risks.out.female[[4]]$score,
                                     risks.out.female[[5]]$score,risks.out.female[[6]]$score,risks.out.female[[7]]$score,risks.out.female[[8]]$score,
                                     risks.out.female[[9]]$score,risks.out.female[[10]]$score,risks.out.female[[11]]$score,risks.out.female[[12]]$score,
                                     risks.out.female[[13]]$score,risks.out.female[[14]]$score,risks.out.female[[15]]$score,risks.out.female[[16]]$score,
                                     risks.out.female[[17]]$score,risks.out.female[[18]]$score,risks.out.female[[19]]$score,risks.out.female[[20]]$score)

risks.all.female <- 1 - risks.all.female/100
risks.all.female <- log(-log(risks.all.female))
risks.all.female <- rowMeans(risks.all.female)
risks.all.female <- exp(-exp(risks.all.female))
risks.all.female <- data.frame("qrisk_data" = 100*(1 - risks.all.female))


# Add patid's, which are stored in each output object from the qrisk function
risks.all.female$patid <- risks.out.female[[1]]$patid
risks.all.female$gender <- 1


## Now do the male cohort
risks.out.male <-vector("list",20)
for (i in 1:20){risks.out.male[[i]] <- qrisk_calc_male(long_data_parallel[[i]])}

# Use Rubins rules to combine risk scores with log(-log()) transformation
risks.all.male <- cbind(risks.out.male[[1]]$score,risks.out.male[[2]]$score,risks.out.male[[3]]$score,risks.out.male[[4]]$score,
                          risks.out.male[[5]]$score,risks.out.male[[6]]$score,risks.out.male[[7]]$score,risks.out.male[[8]]$score,
                          risks.out.male[[9]]$score,risks.out.male[[10]]$score,risks.out.male[[11]]$score,risks.out.male[[12]]$score,
                          risks.out.male[[13]]$score,risks.out.male[[14]]$score,risks.out.male[[15]]$score,risks.out.male[[16]]$score,
                          risks.out.male[[17]]$score,risks.out.male[[18]]$score,risks.out.male[[19]]$score,risks.out.male[[20]]$score)

risks.all.male <- 1 - risks.all.male/100
risks.all.male <- log(-log(risks.all.male))
risks.all.male <- rowMeans(risks.all.male)
risks.all.male <- exp(-exp(risks.all.male))
risks.all.male <- data.frame("qrisk_data" = 100*(1 - risks.all.male))


risks.all.male$patid <- risks.out.male[[1]]$patid
risks.all.male$gender <- 2

str(risks.all.female)
str(risks.all.male)

## Combine two results
risks.all <- rbind(risks.all.female,risks.all.male)

rm(list=setdiff(ls(),list("risks.all")))
save.image("R_out_Vis2019/calculate_qrisk3_scores_data.RData")



