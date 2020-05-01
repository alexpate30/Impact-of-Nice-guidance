### SET THE ROOT DIRECTORY

### SET THE ROOT DIRECTORY

### This program will generate the incidence rate each year, and also produce a table for the number of patients initiated on statins each year

### Start by loading the original cohort of patients, for which we extracted statin users from
all.patients <- read.table("data/CSV2019/allpers.csv",header=TRUE, sep = ",")
all.patients.orig <- all.patients


## First convert date's into date format from factors, and then into numbers so we can use them properly
str(all.patients)

## do85 = date turned 85
## first_Statin = date initiated on statin
## dtcens = follow up ends in patient
## dtvalid = start of follow up for patient
## index_date_A = start of follow up for study, which required being aged 25, at least 1 year's valid follow up in CPRD or 1st Jan 1998 (no prior CVD events or statin prescriptions)
## These should be in the format 'dd/mm/yyyy')
all.patients$do85 <- as.Date(all.patients$do85, format = "%d/%m/%Y")
all.patients$first_statin <- as.Date(all.patients$first_statin, format = "%d/%m/%Y")
all.patients$dtcens <- as.Date(all.patients$dtcens, format = "%d/%m/%Y")
all.patients$first_cvd_all <- as.Date(all.patients$first_cvd_all, format = "%d/%m/%y")
all.patients$dtvalid <- as.Date(all.patients$dtvalid, format = "%d/%m/%Y")
all.patients$index_date_A <- as.Date(all.patients$index_date_A, format = "%d/%m/%Y")

all.patients$do85_datenum <- as.numeric(all.patients$do85)
all.patients$first_statin_datenum <- as.numeric(all.patients$first_statin)
all.patients$dtcens_datenum <- as.numeric(all.patients$dtcens)
all.patients$first_cvd_all_datenum <- as.numeric(all.patients$first_cvd_all)
all.patients$dtvalid_datenum <- as.numeric(all.patients$dtvalid)
all.patients$index_date_A_datenum <- as.numeric(all.patients$index_date_A)

str(all.patients)
head(all.patients)
head(all.patients.orig)

### Using these values, I want to calculate 'start of follow up', 'end of follow up', and 'had a statin yes/no'
# Index date is lastest of 25th birthday, registration with practice, 1 year valid follow up, this is start of follow up
# date of censoring is earliest of death, transer out, cvd event, statin prescription. dtcens variable already contains death and transfer out, so
# just need to consider the others

## Create a variable for censoring date
all.patients$followup_end_datenum <- pmin(all.patients$dtcens_datenum, all.patients$first_cvd_all_datenum, 
                                          all.patients$first_statin_datenum,all.patients$do85_datenum)

### Calculate a 0/1 variable for if they have a statin before being censored
all.patients$statin_before_cens <- as.numeric(all.patients$first_statin_datenum <= all.patients$followup_end_datenum)

## Note if the statin happens on the same day as dtcens, do85 or first_cvd, we want to exclude it
all.patients$statin_before_cens[all.patients$first_statin_datenum == all.patients$first_cvd_all_datenum ] <- 0
all.patients$statin_before_cens[all.patients$first_statin_datenum == all.patients$dtcens_datenum ] <- 0
all.patients$statin_before_cens[all.patients$first_statin_datenum == all.patients$do85_datenum ] <- 0

head(all.patients,n=20)

### First create the values for the cut-offs
## Create empty vector
d<-rep(0,22)

## Create yearly intervals
for (i in 1:22){d[i]<-round(365.25*(i-1))}

## Shift these numbers so they correspond to the date value numbers, we want intervals to at half way through the years
## As this is when NICE guidance was changed
d<-d+as.numeric(as.Date("01/07/1998",format="%d/%m/%Y"))

## Change the first element to start of follow up, which is 1st Jan 1998
d[1] <- as.numeric(as.Date("01/01/1998",format="%d/%m/%Y"))


## Now calculate for each patient, valid follow time each year
## For each year create a vector which will contain the follow up time for each patient in that year
ftime.list<-vector("list",21)

for (j in 1:21){
  ftime.list[[j]] <- pmax(rep(0,nrow(all.patients)), 
                          pmin(rep(d[(j+1)],nrow(all.patients)),all.patients$followup_end_datenum) - 
                            pmax(rep(d[j],nrow(all.patients)),all.patients$index_date_A)
  )
}


head(ftime.list[[1]])
head(ftime.list[[2]])
head(ftime.list[[3]])
head(ftime.list[[4]])
head(ftime.list[[5]])
head(ftime.list[[6]])
head(ftime.list[[7]])
head(ftime.list[[8]])
head(ftime.list[[9]])
head(ftime.list[[10]])
head(ftime.list[[11]])
head(ftime.list[[12]])
head(ftime.list[[13]])

## Now sum up the follow up times and store the output in a vector
t.followup<-rep(0,21)
for (j in 1:21){t.followup[j]<-sum(ftime.list[[j]])/365.25}


## Now do total number of prescriptions in each year, i.e. patients initiated on a stati
t.events<-rep(0,21)
for (j in 1:21){
  t.events[j]<-sum(as.numeric((d[j] < all.patients$first_statin_datenum) & 
                                (all.patients$first_statin_datenum <= d[j+1]) & 
                                (all.patients$statin_before_cens == 1)))
}


incidence<-rep(0,21)
for (j in 1:21){incidence[j]<-t.events[j]/t.followup[j]}

inc.tab<-cbind(t.followup,t.events,1000*incidence)
rownames(inc.tab)<-1998:2018
colnames(inc.tab)<-c("followup","t.events","incidence")

inc.tab<-data.frame(inc.tab)
total<-c(sum(inc.tab[,1]),sum(inc.tab[,2]))
total<-c(total,1000*total[2]/total[1])


inc.tab<-rbind(inc.tab,total)
rownames(inc.tab)[22]<-c("total")

inc.tab
print("save image")
rm(list=setdiff(ls(),list("inc.tab")))

inc.tab$year <- c(1998:2018,NA)
inc.tab

## Make a ggplot
library(ggplot2)
incidence.ggplot <- ggplot(data = inc.tab, aes(x= year, y = incidence)) +
  geom_line() + geom_point() +
  scale_x_continuous(name = "Year", breaks = c(1998,2000,2002,2004,2006,2008,2010,2012,2014,2016), labels = paste(c("98","00","02","04","06","08",
                                                                         "10","12","14","16"),"-",
                                                                       c("99","01","03","05","07","09",
                                                                         "11","13","15","17"),sep=""),limits=c(1998,2016)) +
  geom_vline(xintercept = 2013.5) +
  ylab("Incidence rate per 1000 person years")

incidence.ggplot
ggsave("figures/Vis2019/Figure1.png",incidence.ggplot, dpi = 1000)

save.image("R_out_Vis2019/run_analysis_incidence_over_time.RData")
