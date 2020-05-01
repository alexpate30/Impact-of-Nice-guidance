### SET THE ROOT DIRECTORY

### SET THE ROOT DIRECTORY

library(mice)
library(foreach)
library(doParallel)
library(dplyr)

# -----------------------------------------------------------------------
### This program imputes the statin users cohort
# -----------------------------------------------------------------------

## First read in the dataset which contains all predictor variables
data <- read.table("data/CSV2019/statin_users_analysis_dataset.csv",header=TRUE, sep = ",")

# -----------------------------------------------------------------------
### First some variables need to be properly sorted out
# -----------------------------------------------------------------------

#### ERECTILE DYSFUNCTION
## Set erectile dysfunction to zero for all women
data$Erec_dysfunc[data$gender == 1] <- 0


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

## drop unused levels
data$Ethnicity <- droplevels(data$Ethnicity)

prop.table(table(data$Ethnicity))

#### SMOKING
### Want to change Smoking so that it becomes five categories, (0 = never, 1 = ex, 2 = light, 3 = moderate, 4 = heavy)
### Only remaining issue is that I have a group of patients whose smoking status = current, but there is no numcigs variable present
### For these patients we assume light smokers, rather than imputing them
prop.table(table(data$Smoking))

### Output ones where numcigs > 0 and Smoking = 1 or 0
test1 <- data[(data$Smoking < 2) & (!is.na(data$Smoking)) & (data$Numcigs_perday > 0) & (!is.na(data$Numcigs_perday)),]
head(test1,n=30)

## So there are some, likely because in the last three years they have had a "10 cigs per day" records, but since then have stopped
## This means they willl be down as ex smoker, and also have a numcigs per day thing
## In this case, smoking can stay as it is "ex smoker = 1"
rm(test1)

## Next want to changeanyone with numcigs 10 - 19, and Smoking = 2, to moderate smoker = 3
moderate.smokers <- (!is.na(data$Smoking)) & (!is.na(data$Numcigs_perday)) & (data$Smoking == 2) & (data$Numcigs_perday > 9) & (data$Numcigs_perday < 20)
head(data[moderate.smokers, ])

data$Smoking[moderate.smokers] <- 3

## Next want to changeanyone with numcigs 20+ to heavy smoker = 4
heavy.smokers <- (!is.na(data$Smoking)) & (!is.na(data$Numcigs_perday)) & (data$Smoking == 2) & (data$Numcigs_perday > 19)
head(data[heavy.smokers, ])

data$Smoking[heavy.smokers] <- 4

## Check distributuon of variable
prop.table(table(data$Smoking))


## Next get all variables in the correct format
data$Atrialfib <- as.factor(data$Atrialfib)
data$Atypical_antipsy_med <- as.factor(data$Atypical_antipsy_med)
data$CKD345 <- as.factor(data$CKD345)
data$Corticosteroid_use <- as.factor(data$Corticosteroid_use)
data$T1dia <- as.factor(data$T1dia)
data$T2dia <- as.factor(data$T2dia)
data$FamHis <- as.factor(data$FamHis)
data$HIV <- as.factor(data$HIV)
data$Hypertension <- as.factor(data$Hypertension)
data$Migraine <- as.factor(data$Migraine)
data$RA <- as.factor(data$RA)
data$Sev_men_ill_qrisk <- as.factor(data$Sev_men_ill_qrisk)
data$Smoking <- as.factor(data$Smoking)
data$SLE <- as.factor(data$SLE)
data$Townsend <- as.factor(data$townsend)

### Now remove variables not wanted for imputation, expect ones identifying the patient
str(data)
colnames(data)

data <- select(data, -c(dtvalid, dtcens, first_statin, first_cvd_all,townsend,Qrisk_score,
                        Numcigs_perday))
str(data)

print("dimensions data")
dim(data)

print('1) data has been read in')

## Add interaction terms, continuous variables
anal.dat2<-cbind(data,age.BMI = ((data$age-mean(data$age,na.rm=TRUE))*(data$BMI-mean(data$BMI,na.rm=TRUE))),
                            age.SBP = ((data$age-mean(data$age,na.rm=TRUE))*(data$SBP-mean(data$SBP,na.rm=TRUE))))


## Add interaction terms, binary variables, note deducting the mean will make no difference as I have already centered age
anal.dat2<-cbind(anal.dat2,age.Atrialfib = (anal.dat2$age-mean(anal.dat2$age))*(anal.dat2$Atrialfib==1), 
                          age.Corticosteroid_use = (anal.dat2$age-mean(anal.dat2$age))*(anal.dat2$Corticosteroid_use==1), 
                          age.CKD345 = (anal.dat2$age-mean(anal.dat2$age))*(anal.dat2$CKD345==1),
                          age.FamHis = (anal.dat2$age-mean(anal.dat2$age))*(anal.dat2$FamHis==1), 
                          age.Hypertension = (anal.dat2$age-mean(anal.dat2$age))*(anal.dat2$Hypertension==1), 
                          age.T1dia = (anal.dat2$age-mean(anal.dat2$age))*(anal.dat2$T1dia==1), 
                          age.T2dia = (anal.dat2$age-mean(anal.dat2$age))*(anal.dat2$T2dia==1),
                          age.Migraine = (anal.dat2$age-mean(anal.dat2$age))*(anal.dat2$Migraine==1)) 
                        

## Add interaction terms, categorical variables
anal.dat2<-cbind(anal.dat2,
                 age.Smoking.1 = ((anal.dat2$age-mean(anal.dat2$age))*(anal.dat2$Smoking==1)), 
                  age.Smoking.2 = ((anal.dat2$age-mean(anal.dat2$age))*(anal.dat2$Smoking==2)), 
                 age.Smoking.3 = ((anal.dat2$age-mean(anal.dat2$age))*(anal.dat2$Smoking==3)), 
                 age.Smoking.4 = ((anal.dat2$age-mean(anal.dat2$age))*(anal.dat2$Smoking==4)), 
                   age.Townsend.1 = ((anal.dat2$age-mean(anal.dat2$age))*(anal.dat2$Townsend==2)), 
                    age.Townsend.2 = ((anal.dat2$age-mean(anal.dat2$age))*(anal.dat2$Townsend==3)),
                      age.Townsend.3 = ((anal.dat2$age-mean(anal.dat2$age))*(anal.dat2$Townsend==4)),
                       age.Townsend.4 = ((anal.dat2$age-mean(anal.dat2$age))*(anal.dat2$Townsend==5)))


## Now re-arrage so that order is the order I want them to be imputed
anal.dat2<-select(anal.dat2, patid,
                 gender, age, BMI, age.BMI, Atrialfib, age.Atrialfib, Atypical_antipsy_med, Cholesterol, HDL, Cholesterol_HDL_Ratio, 
                 Corticosteroid_use, age.Corticosteroid_use, CKD345, age.CKD345, HIV, Erec_dysfunc, Ethnicity, 
                 FamHis, age.FamHis, Hypertension, age.Hypertension, Migraine, age.Migraine,
                 RA, SBP, age.SBP, SBP_var, Sev_men_ill_qrisk, SLE, 
                 Smoking, age.Smoking.1, age.Smoking.2, age.Smoking.3, age.Smoking.4, T1dia, age.T1dia, T2dia, age.T2dia, 
                 Townsend, age.Townsend.1, age.Townsend.2, age.Townsend.3, age.Townsend.4)

## Check variables are in the right order
head(anal.dat2)

print('3) variables have been got in correct order')

## Do initial imputation of zero imputations to extract methods and predictor matrix
imp0<-mice(anal.dat2,maxit=0,print=FALSE)

### Print matrix used for imputation to see what interaction terms are called for categorical variables (i.e. Townsend.1, Townsend.2, etc)
head(imp0$pad$data,1)

print('4) initial imputation done')

### First extract the list of imputation methods for each variables
meth<-imp0$meth
length(meth)
meth

## Specify how interactions, fractional polynomials and composite variables are imputed (passively, note BMI is not imputed passively from height and weight). 

## Continuous variables.
# Cholesterol/HDL ratio
meth["Cholesterol_HDL_Ratio"]<-"~I(Cholesterol/HDL)"

# BMI
meth["age.BMI"]<- "~I((age-mean(anal.dat2$age,na.rm=TRUE))*(BMI-mean(anal.dat2$BMI,na.rm=TRUE)))"

# SBP
meth["age.SBP"]<- "~I((age-mean(anal.dat2$age,na.rm=TRUE))*(SBP-mean(anal.dat2$SBP,na.rm=TRUE)))"

## Categorical variables
# Smoking
meth["age.Smoking.1"]<-"~I((age-mean(anal.dat2$age))*Smoking.1)"
meth["age.Smoking.2"]<-"~I((age-mean(anal.dat2$age))*Smoking.2)"
meth["age.Smoking.3"]<-"~I((age-mean(anal.dat2$age))*Smoking.3)"
meth["age.Smoking.4"]<-"~I((age-mean(anal.dat2$age))*Smoking.4)"
# Townsend
meth["age.Townsend.1"]<-"~I((age-mean(anal.dat2$age))*Townsend.1)"
meth["age.Townsend.2"]<-"~I((age-mean(anal.dat2$age))*Townsend.2)"
meth["age.Townsend.3"]<-"~I((age-mean(anal.dat2$age))*Townsend.3)"
meth["age.Townsend.4"]<-"~I((age-mean(anal.dat2$age))*Townsend.4)"

print("print methods")
meth

### Next extract the predictor matrix to assign which variables predict which
pred<-imp0$pred

print ("initial predictor matrix")
pred

### Assign which variables that shouldn't be used in the prediction
pred[,"patid"]<-0

### Assign which variables the interacions will help predict 

## Continuous variables
# BMI
# Don't want to predict BMI using age.BMI, or either of the BMI fractional polynomials, or age by age.BMI
pred[c("age","BMI"),"age.BMI"]<-0

# SBP
pred[c("age","SBP"),"age.SBP"]<-0

## Categorical variables
pred[c("age","Smoking"),c("age.Smoking.1","age.Smoking.2","age.Smoking.3","age.Smoking.4")]<-0
pred[c("age","Townsend"),c("age.Townsend.1","age.Townsend.2","age.Townsend.3","age.Townsend.4")]<-0

## Binary
# Note these shouldnt actually make a difference, as we don't impute the majority of the variables going into this imputation anyway
pred[c("age","Atrialfib"),"age.Atrialfib"]<-0
pred[c("age","Corticosteroid_use"),"age.Corticosteroid_use"]<-0
pred[c("age","CKD345"),"age.CKD345"]<-0
pred[c("age","FamHis"),"age.FamHis"]<-0
pred[c("age","Hypertension"),"age.Hypertension"]<-0
pred[c("age","T1dia"),"age.T1dia"]<-0
pred[c("age","T2dia"),"age.T2dia"]<-0
pred[c("age","Migraine"),"age.Migraine"]<-0

diag(pred)<-0

print ("final predictor matrix")
pred

print('5) methods and predictor matrix defined')

### Next want to check the order that the variabes are computed

# Rerun the imputation dry run with meth = meth and pred = pred, which includes imputation of extra stuff such as interaction.
imp00<-mice(anal.dat2,maxit=0,print=FALSE, meth=meth, pred=pred)

print('6) second preliminary imputation done')

head(imp00$pad$data,1)

# Check its in the right order (i.e. impute BMI before BMI's fractional and interactions etc)
vis<-imp00$vis
vis

rm(anal.dat)

## All seems legit, lets send it to the super computer!!!
print('7. visiting sequence done, send to the super computer')

# imptest<-mice(anal.dat2,m=1,meth=meth,pred=pred,vis=vis,seed=102,maxit=2,print=FALSE)

dim(anal.dat2)

cl <- makeCluster(11)
registerDoParallel(11)
start_time <- Sys.time()
mice1<-(foreach(seed.num=c(100,101,102,103,104,105,106,107,108,109), .combine=list, .multicombine=TRUE, 
                         .packages=c("dplyr","mice","tidyr"))
                 %dopar%{mice(anal.dat2,m=1,meth=meth,pred=pred,vis=vis,seed=seed.num,maxit=20,print=FALSE)
                 })
end_time <- Sys.time()
diff <- start_time - end_time
stopCluster(cl)


## Save image
rm(list=setdiff(ls(),list("data","mice1")))
save.image("R_out_Vis2019/imputation_h1.RData")

print("7) FINISHED")