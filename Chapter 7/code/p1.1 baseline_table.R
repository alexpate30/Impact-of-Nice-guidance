### SET THE ROOT DIRECTORY

### SET THE ROOT DIRECTORY

library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)

### Next load the original dataset, which has statin initiation date, which we need
data <- read.table("data/CSV2019/statin_users_analysis_dataset.csv",header=TRUE, sep = ",")

#### ETHNICITY
## Change pakistani to pakistan
data$Ethnicity[data$Ethnicity == "pakistani"] <- "pakistan"
## Change oth_asian to asianother
data$Ethnicity[data$Ethnicity == "oth_asian"] <- "asianother"
## Change oth_asian to asianother
data$Ethnicity[data$Ethnicity == "mixed"] <- "other"

## According to QRISK, any missing is included in the category, white or not stated, so we will do this...
data$Ethnicity[data$Ethnicity == "missing"] <- "white"
data$Ethnicity[data$Ethnicity == "unclas"] <- "white"
data$Ethnicity <- droplevels(data$Ethnicity)

## Restrict both to gendered cohort
data.female <- data[data$gender == 1,]
data.male <- data[data$gender == 0,]

## Create vectors for continuous variables
female.mean <- c(mean(data.female$age),mean(data.female$SBP,na.rm = TRUE),mean(data.female$SBP_var,na.rm = TRUE),
                mean(data.female$BMI,na.rm = TRUE),mean(data.female$Cholesterol_HDL_Ratio,na.rm = TRUE))
female.sd <- c(sd(data.female$age),sd(data.female$SBP,na.rm = TRUE),sd(data.female$SBP_var,na.rm = TRUE),
              sd(data.female$BMI,na.rm = TRUE),sd(data.female$Cholesterol_HDL_Ratio,na.rm = TRUE))


male.mean <- c(mean(data.male$age),mean(data.male$SBP,na.rm = TRUE),mean(data.male$SBP_var,na.rm = TRUE),
                 mean(data.male$BMI,na.rm = TRUE),mean(data.male$Cholesterol_HDL_Ratio,na.rm = TRUE))
male.sd <- c(sd(data.male$age),sd(data.male$SBP,na.rm = TRUE),sd(data.male$SBP_var,na.rm = TRUE),
               sd(data.male$BMI,na.rm = TRUE),sd(data.male$Cholesterol_HDL_Ratio,na.rm = TRUE))



## Create discrete variables
Smoking.prop <- cbind(100*prop.table(table(data.female$Smoking)),100*prop.table(table(data.male$Smoking)))

Townsend.prop <- cbind(100*prop.table(table(data.female$Townsend)),100*prop.table(table(data.male$Townsend)))

Ethnicity.prop <- cbind(100*prop.table(table(data.female$Ethnicity)),100*prop.table(table(data.male$Ethnicity)))

Hypertension.prop <- c(100*prop.table(table(data.female$Hypertension))[2],100*prop.table(table(data.male$Hypertension))[2])

Famhis_lstrict.prop <- c(100*prop.table(table(data.female$Famhis_lstrict))[2],100*prop.table(table(data.male$Famhis_lstrict))[2])

T1dia.prop <- c(100*prop.table(table(data.female$T1dia))[2],100*prop.table(table(data.male$T1dia))[2])

T2dia.prop <- c(100*prop.table(table(data.female$T2dia))[2],100*prop.table(table(data.male$T2dia))[2])

Anixety.prop<- c(100*prop.table(table(data.female$Anxiety))[2],100*prop.table(table(data.male$Anxiety))[2])

Atrialfib.prop <- c(100*prop.table(table(data.female$Atrialfib))[2],100*prop.table(table(data.male$Atrialfib))[2])


Atypical_antipsy_med.prop <- c(100*prop.table(table(data.female$Atypical_antipsy_med))[2],100*prop.table(table(data.male$Atypical_antipsy_med))[2])


Corticosteroid_use.prop<- c(100*prop.table(table(data.female$Corticosteroid_use))[2],100*prop.table(table(data.male$Corticosteroid_use))[2])


CKD345.prop<- c(100*prop.table(table(data.female$CKD345))[2],100*prop.table(table(data.male$CKD345))[2])


CKD45.prop<- c(100*prop.table(table(data.female$CKD45))[2],100*prop.table(table(data.male$CKD45))[2])


LVH.prop<- c(100*prop.table(table(data.female$LVH))[2],100*prop.table(table(data.male$LVH))[2])


HIV.prop <- c(100*prop.table(table(data.female$HIV))[2],100*prop.table(table(data.male$HIV))[2])


FamHis.prop <- c(100*prop.table(table(data.female$FamHis))[2],100*prop.table(table(data.male$FamHis))[2])


Migraine.prop <- c(100*prop.table(table(data.female$Migraine))[2],100*prop.table(table(data.male$Migraine))[2])


RA.prop <- c(100*prop.table(table(data.female$RA))[2],100*prop.table(table(data.male$RA))[2])


T1dia.prop <- c(100*prop.table(table(data.female$T1dia))[2],100*prop.table(table(data.male$T1dia))[2])


SLE.prop <- c(100*prop.table(table(data.female$SLE))[2],100*prop.table(table(data.male$SLE))[2])


Sevmenill.prop <- c(100*prop.table(table(data.female$Sev_men_ill_qrisk))[2],100*prop.table(table(data.male$Sev_men_ill_qrisk))[2])



### Now to summarise missingness
### Now to summarise missingness

SBP.miss <- c(100*sum(is.na(data.female$SBP))/nrow(data.female),
              100*sum(is.na(data.male$SBP))/nrow(data.male))

SBP_var.miss <- c(100*sum(is.na(data.female$SBP_var))/nrow(data.female),
                  100*sum(is.na(data.male$SBP_var))/nrow(data.male))

BMI.miss <- c(100*sum(is.na(data.female$BMI))/nrow(data.female),
                  100*sum(is.na(data.male$BMI))/nrow(data.male))

Chol.miss <- c(100*sum(is.na(data.female$Cholesterol_HDL_Ratio))/nrow(data.female),
               100*sum(is.na(data.male$Cholesterol_HDL_Ratio))/nrow(data.male))


Smoking.miss <- c(100*sum(is.na(data.female$Smoking))/nrow(data.female),
                  100*sum(is.na(data.male$Smoking))/nrow(data.male))

Townsend.miss <- c(100*sum(is.na(data.female$townsend))/nrow(data.female),
                   100*sum(is.na(data.male$townsend))/nrow(data.male))

str(data.female)
## Create continuous table
cont.vars <- data.frame(cbind(paste(round(female.mean,2)," (",round(female.sd,2),")",sep=""),
                              paste(round(male.mean,2)," (",round(male.sd,2),")",sep="")))
colnames(cont.vars) <- c("Female","Male")
rownames(cont.vars) <- c("Age","SBP","SBP_var","BMI","Chol/HDL")



## Create categorical table
cat.vars <- rbind(Atrialfib.prop,
                  Atypical_antipsy_med.prop,
                  Corticosteroid_use.prop,
                  CKD345.prop,
                  T1dia.prop,
                  T2dia.prop,
                  Ethnicity.prop,
                  FamHis.prop,
                  HIV.prop,
                  Hypertension.prop ,
                  Migraine.prop,
                  RA.prop,
                  Smoking.prop ,
                  SLE.prop ,
                  Townsend.prop ,
                  Sevmenill.prop)
colnames(cat.vars) <- c("Female","Male")

# Create missingness dataset
miss.vars <- rbind(SBP.miss,SBP_var.miss,BMI.miss,Chol.miss,Smoking.miss,Townsend.miss)
colnames(miss.vars) <- c("Female","Male")
rownames(miss.vars) <- c("SBP","SBP_var","BMI","Chol/HDL","Smoking","Townsend")

male.n <- nrow(data.male)
female.n <- nrow(data.female)

rm(list=setdiff(ls(),list("male.n","female.n","cont.vars","cat.vars","miss.vars")))

cont.vars
cat.vars
miss.vars

save.image("R_out_Vis2019/baseline_table.RData")