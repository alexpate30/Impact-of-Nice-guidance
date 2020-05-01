## This file will calculate the outcomes for cohort A and B
## Hopefully it will do it in a fraction of the time!

setwd("/mnt/ja01-home01/mbrxsap3/phd_risk/R/p2_derive_variables")

# install.packages("parallel", repos="http://cran.rstudio.com/", lib="/mnt/ja01-home01/mbrxsap3/phd_risk/R/packages")
# install.packages("dplyr", repos="http://cran.rstudio.com/")


library(parallel)
library(dplyr)

# Read in GFR data
mydataA <- read.table("/mnt/ja01-home01/mbrxsap3/phd_risk/csv_data/p2_derive_variables/egfr_scores_A.txt", header=TRUE)

head(mydataA)

# Change relevant variable names
names(mydataA)[names(mydataA)=="eventdate"]<- "EntryDate"
names(mydataA)[names(mydataA)=="egfr"]<- "CodeValue"


# Create function to be used in lapply
# This will search each dataset for all CKD occurences

check_all_CKD_A<-function(dat){
  # Create output dataset
  output<-data.frame(patid=numeric(),pracid=numeric(),EntryDate=factor(),
                     CodeValue=numeric(),index_date_A=factor(),num=numeric(),stringsAsFactors=FALSE)
  # Loop through each patid using the 'num' variable
  # First check that what i'm looping through is correct
  # print(dat[1,"num"])
  # print(dat[dim(dat)[1],"num"])
  # Now loop through ID = ...
  for(ID in dat[1,"num"]:dat[dim(dat)[1],"num"]){
    d=dat[as.character(dat$num)==as.character(ID),]
    control=FALSE
    i=1
    index=0
    # date.CKD=NULL
    while((!control)&(i<dim(d)[1])){
      if((d[i,]$CodeValue<60)){
        j=1
        while((!control)&(j<=(dim(d)[1]-i))){
          if((d[i+j,]$CodeValue<60)){
            diff=(as.Date(d[i,]$EntryDate)-as.Date(d[i+j,]$EntryDate))
            if(diff>=90){
              control=TRUE
              index=i
              #date.CKD=as.character(d[index,]$EntryDate)
            }
          }else{
            j=dim(d)[1]-i
          }
          j=j+1
        }
      }
      i=i+1
    }
    # If index is > 0, add the entry to the output
    if (index > 0){
      output<-rbind(output,d[index,])
    }
  }
  # At the end of the for loop, output the dataset
  output
}

## Create datasets to go into lapply
maxnumA<-mydataA[dim(mydataA)[1],"num"]

n.dat<-20
for (i in 1:n.dat){
  assign(paste("mydataA",i,sep=""), filter(mydataA,(num<=maxnumA*i/n.dat)&(maxnumA*(i-1)/n.dat<num)))
}


# Now do the lapply stuff in the cluster
cl <- makeCluster(20)
clusterExport(cl, "check_all_CKD_A")

start_time_clusA20 <- Sys.time()
CKD_stage345_A.l<-parLapply(cl, list(mydataA1,mydataA2,mydataA3,mydataA4,mydataA5,mydataA6,mydataA7,mydataA8,mydataA9,mydataA10,
                   mydataA11,mydataA12,mydataA13,mydataA14,mydataA15,mydataA16,mydataA17,mydataA18,mydataA19,mydataA20), 
          function(x) check_all_CKD_A(x))
end_time_clusA20 <- Sys.time()
time_clusA20<-start_time_clusA20-end_time_clusA20
stopCluster(cl)
time_clusA20

str(CKD_stage345_A.l)

CKD_stage345_A<-bind_rows(CKD_stage345_A.l[[1]],CKD_stage345_A.l[[2]],CKD_stage345_A.l[[3]],CKD_stage345_A.l[[4]],
                          CKD_stage345_A.l[[5]],CKD_stage345_A.l[[6]],CKD_stage345_A.l[[7]],CKD_stage345_A.l[[8]],
                          CKD_stage345_A.l[[9]],CKD_stage345_A.l[[10]],CKD_stage345_A.l[[11]],CKD_stage345_A.l[[12]],
                          CKD_stage345_A.l[[13]],CKD_stage345_A.l[[14]],CKD_stage345_A.l[[15]],CKD_stage345_A.l[[16]],
                          CKD_stage345_A.l[[17]],CKD_stage345_A.l[[18]],CKD_stage345_A.l[[19]],CKD_stage345_A.l[[20]])

save.image("CKD_stage345_A_image.RData")
#load("CKD_stage345_A_image.RData")

write.csv(CKD_stage345_A,file="/mnt/ja01-home01/mbrxsap3/phd_risk/csv_data/p2_derive_variables/CKD_stage345_A_fromR.csv")
