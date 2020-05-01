### SET THE ROOT DIRECTORY

### SET THE ROOT DIRECTORY

library(mice)
library(foreach)
library(doParallel)
library(dplyr)

## Load h1 data
load("R_out_Vis2019/imputation_h1.RData")

## Load h2 data
load("R_out_Vis2019/imputation_h2.RData")

## Turn data into long format
h1_long <- vector("list",10)
for (i in 1:10){h1_long[[i]] <- mice::complete(mice1[[i]],action='long')}

h2_long <- vector("list",10)
for (i in 1:10){h2_long[[i]] <- mice::complete(mice2[[i]],action='long')}

## Create one list with each imputed dataset
long_data_parallel <- c(h1_long,h2_long)

rm(list=setdiff(ls(),list("long_data_parallel","data")))
save.image("R_out_Vis2019/imputed_datasets_loaded.RData")
