### SET THE ROOT DIRECTORY

### SET THE ROOT DIRECTORY

### In this program we will run the majority of the main analysis
### 1) Calculate the average risk of patients that are initiated on statins each year (EHR derived risks, and the coded risks)
### 2) Calculate the proportion of patients in each risk category that are initiated on statins each year (EHR derived risks, and the coded risks)
### 3) For patients that have a coded risk score, compare the EHR derived risks with the coded risk scores

### The EHR derived risks are saved in the calculate_qrisk3_scores_data.RData workspace, in a dataframe called 'risks.all'
### The coded risk scores are already in the dataset, statin_users_analysis_dataset.csv, although there are missing values for quite a lot of patients

### Note I used the term 'data.driven' to refer to the EHR derived risks a lot in this code

library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)

### First load the EHR derived risk scores
load("R_out_Vis2019/calculate_qrisk3_scores_data.RData")

### Next load the original dataset, which has statin initiation date for each patient, and also the coded risk score
data <- read.table("data/CSV2019/statin_users_analysis_dataset.csv",header=TRUE, sep = ",")

## Sort both by patid for merging
data <- arrange(data,patid)
risks.all <- arrange(risks.all,patid)

head(data$patid)
head(risks.all$patid)

## Remove excess variables from data
anal.dat <- select(data, patid, pracid, first_statin, qrisk_coded = Qrisk_score)
# Note we are removing gender, to use gender from the risks.all dataset, where female = 1, male = 2, to match
# the coding in the qirks algorithm and consistency across code

str(anal.dat)
summary(anal.dat$qrisk_coded)

## First remove all qrisk_coded values that are == 0 or higher than 100
anal.dat$qrisk_coded[anal.dat$qrisk_coded > 100] <- NA
anal.dat$qrisk_coded[anal.dat$qrisk_coded <= 0] <- NA


## Add data driven risk scores and gender from risks.all
anal.dat$qrisk_data <- risks.all$qrisk_data
anal.dat$gender <- risks.all$gender

## Turn first_statin into a date variable we can condition on
anal.dat$first_statin <- as.numeric(as.Date(anal.dat$first_statin, format = "%d/%m/%Y"))

####### We now have all the data needed stored in anal.dat ##########

####### 1) Calculate mean risk of patients initiated on treatment each year
####### 1) Calculate mean risk of patients initiated on treatment each year
####### 1) Calculate mean risk of patients initiated on treatment each year

### First create the values for the cut-offs for each year
## Create empty vector
d<-rep(0,21)

## Create yearly intervals
for (i in 1:21){d[i]<-round(365.25*(i-1))}

## Shift these numbers so they correspond to the date value numbers, we want intervals to at half way through the years
## As this is when NICE guidance was changed
d<-d+as.numeric(as.Date("01/07/1998",format="%d/%m/%Y"))

## Change the first element to start of follow up, which is 1st Jan 1998
d[1] <- as.numeric(as.Date("01/01/1998",format="%d/%m/%Y"))


## Now get split dataset into catorgies based on these dates
anal.dat$date_cat <- cut(anal.dat$first_statin, d, labels = 1998:2017)
nrow(anal.dat)*prop.table(table(anal.dat$date_cat))

## Also want to create a variable which has the qrisk_data values, but only for patients with a qrisk_coded
anal.dat$qrisk_data_for_coded <- anal.dat$qrisk_data
anal.dat$qrisk_data_for_coded[is.na(anal.dat$qrisk_coded)] <- NA


## Create mean qrisk variables stratified by year, for coded qrisk and that derived from data
mean.qrisk.data <- anal.dat %>% 
  group_by(date_cat) %>%
  summarise(N_qrisk_data = sum(!is.na(qrisk_data)),
            mean_qrisk_data = mean(qrisk_data),
            N_qrisk_coded = sum(!is.na(qrisk_coded)),
            mean_qrisk_coded = mean(qrisk_coded, na.rm=TRUE),
            mean_qrisk_data_for_coded = mean(qrisk_data_for_coded, na.rm=TRUE)) %>% 
  as.data.frame()

mean.qrisk.data$prop.coded <- 100*mean.qrisk.data$N_qrisk_coded/mean.qrisk.data$N_qrisk_data

## Want to turn this into a plot
## Seperate out data for coded qrisk scores and data derived qrisk scores, then combine into a single dataset for ggplot
mean.data <- select(mean.qrisk.data,date_cat,Average_risk = mean_qrisk_data)
mean.data$score_type <- "Data"
mean.coded <- select(mean.qrisk.data,date_cat,Average_risk = mean_qrisk_coded)
mean.coded$score_type <- "Coded"


mean.plot.data = rbind(mean.data,mean.coded)

## Convert some variables to the correct format
mean.plot.data$date_cat <- as.numeric(as.character(mean.plot.data$date_cat))
mean.plot.data$score_type <- as.factor(mean.plot.data$score_type)


mean_qrisk_score_plot.1 <- ggplot(data = mean.plot.data[mean.plot.data$score_type %in% c("Coded","Data"), ], aes(x= date_cat, y = Average_risk, group = score_type)) +
  geom_line(aes(color = score_type)) + geom_point(aes(color = score_type)) + 
  scale_color_manual(values=c("red4","navyblue"), name="Risk type", labels=c("Coded","EHR\nderived")) +
  scale_x_continuous(name = "Year", breaks = 1998:2017, labels = paste(c("98","99","00","01","02","03","04","05","06","07","08","09",
                                                                         "10","11","12","13","14","15","16","17"),"-",
                                                                       c("99","00","01","02","03","04","05","06","07","08","09",
                                                                         "10","11","12","13","14","15","16","17","18"),sep=""), limits = c(2006,2017)) +
  geom_vline(xintercept = 2013.5) +
  ylab("Average risk") 



ggsave("figures/Vis2019/Figure2.png",mean_qrisk_score_plot.1, dpi = 1000)


####### 2) Compare data.driven risks with coded risks
####### 2) Compare data.driven risks with coded risks
####### 2) Compare data.driven risks with coded risks

### Going to produce scatter plots to compare coded qrisk score with data driven qrisk scores (stratified by year)

### Create different data.sets for each year
data2011 <- data.frame("data.driven" = anal.dat$qrisk_data_for_coded[anal.dat$date_cat %in% c("2011") & !is.na(anal.dat$qrisk_data_for_coded)],
                       "coded" = anal.dat$qrisk_coded[anal.dat$date_cat %in% c("2011") & !is.na(anal.dat$qrisk_data_for_coded)],
                       "Year" = 2011)

data2012 <- data.frame("data.driven" = anal.dat$qrisk_data_for_coded[anal.dat$date_cat %in% c("2012") & !is.na(anal.dat$qrisk_data_for_coded)],
                   "coded" = anal.dat$qrisk_coded[anal.dat$date_cat %in% c("2012") & !is.na(anal.dat$qrisk_data_for_coded)],
                   "Year" = 2012)

data2013 <- data.frame("data.driven" = anal.dat$qrisk_data_for_coded[anal.dat$date_cat %in% c("2013") & !is.na(anal.dat$qrisk_data_for_coded)],
                       "coded" = anal.dat$qrisk_coded[anal.dat$date_cat %in% c("2013") & !is.na(anal.dat$qrisk_data_for_coded)],
                       "Year" = 2013)

data2014 <- data.frame("data.driven" = anal.dat$qrisk_data_for_coded[anal.dat$date_cat %in% c("2014") & !is.na(anal.dat$qrisk_data_for_coded)],
                       "coded" = anal.dat$qrisk_coded[anal.dat$date_cat %in% c("2014") & !is.na(anal.dat$qrisk_data_for_coded)],
                       "Year" = 2014)

data2015 <- data.frame("data.driven" = anal.dat$qrisk_data_for_coded[anal.dat$date_cat %in% c("2015") & !is.na(anal.dat$qrisk_data_for_coded)],
                       "coded" = anal.dat$qrisk_coded[anal.dat$date_cat %in% c("2015") & !is.na(anal.dat$qrisk_data_for_coded)],
                       "Year" = 2015)

data2016 <- data.frame("data.driven" = anal.dat$qrisk_data_for_coded[anal.dat$date_cat %in% c("2016") & !is.na(anal.dat$qrisk_data_for_coded)],
                       "coded" = anal.dat$qrisk_coded[anal.dat$date_cat %in% c("2016") & !is.na(anal.dat$qrisk_data_for_coded)],
                       "Year" = 2016)

data2017 <- data.frame("data.driven" = anal.dat$qrisk_data_for_coded[anal.dat$date_cat %in% c("2017") & !is.na(anal.dat$qrisk_data_for_coded)],
                       "coded" = anal.dat$qrisk_coded[anal.dat$date_cat %in% c("2017") & !is.na(anal.dat$qrisk_data_for_coded)],
                       "Year" = 2017)



plot2011 <- ggplot(data = data2011, aes(x=data.driven,y=coded)) + geom_point(alpha = 0.05) + ggtitle("2011") + 
  geom_abline(slope = 1, intercept = 0, color = "blue") + 
  xlim(limits = c(0,100)) + ylim(limits = c(0,100)) + xlab("EHR derived risk") + ylab("Coded risk")

plot2012 <- ggplot(data = data2012, aes(x=data.driven,y=coded)) + geom_point(alpha = 0.05) + ggtitle("2012") + 
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  xlim(limits = c(0,100)) + ylim(limits = c(0,100)) + xlab("EHR derived risk") + ylab("Coded risk")

plot2013 <- ggplot(data = data2013, aes(x=data.driven,y=coded)) + geom_point(alpha = 0.05) + ggtitle("2013") + 
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  xlim(limits = c(0,100)) + ylim(limits = c(0,100)) + xlab("EHR derived risk") + ylab("Coded risk")

plot2014 <- ggplot(data = data2014, aes(x=data.driven,y=coded)) + geom_point(alpha = 0.05) + ggtitle("2014") + 
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  xlim(limits = c(0,100)) + ylim(limits = c(0,100)) + xlab("EHR derived risk") + ylab("Coded risk")

plot2015 <- ggplot(data = data2015, aes(x=data.driven,y=coded)) + geom_point(alpha = 0.05) + ggtitle("2015") + 
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  xlim(limits = c(0,100)) + ylim(limits = c(0,100)) + xlab("EHR derived risk") + ylab("Coded risk")

plot2016 <- ggplot(data = data2016, aes(x=data.driven,y=coded)) + geom_point(alpha = 0.05) + ggtitle("2016") + 
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  xlim(limits = c(0,100)) + ylim(limits = c(0,100)) + xlab("EHR derived risk") + ylab("Coded risk")

plot2017 <- ggplot(data = data2017, aes(x=data.driven,y=coded)) + geom_point(alpha = 0.05) + ggtitle("2017") + 
  geom_abline(slope = 1, intercept = 0, color = "blue") + 
  xlim(limits = c(0,100)) + ylim(limits = c(0,100)) + xlab("EHR derived risk") + ylab("Coded risk")

scatter.plot <- ggarrange(plot2011,plot2012,plot2013,plot2014,plot2015,plot2016,nrow = 2,ncol=3)
scatter.plot
ggsave("figures/Vis2019/Figure4.png",scatter.plot, height = 7, width = 10.5, dpi = 1000)



####### 3) Plot risk cateogory of patients initiated on statins each year
####### 3) Plot risk cateogory of patients initiated on statins each year
####### 3) Plot risk cateogory of patients initiated on statins each year


### I need to group the data by date_cat, and then calculate the proportion of observations in each category
### For that I first need create a variable that does the categorisation
### Then I can just do a prop.table function on that variable, for the given by_group

## Start by creating the variable which is whether a patient is low, medium or high risk
anal.dat$risk_cat_data <- cut(anal.dat$qrisk_data, c(0,10,20,100), labels = c("<10%","10-20%",">20%")) 
anal.dat$risk_cat_coded <- cut(anal.dat$qrisk_coded, c(0,10,20,100), labels = c("<10%","10-20%",">20%")) 
head(anal.dat)

# prop.table(table(anal.dat$risk_cat_data))
# prop.table(table(anal.dat$risk_cat_coded))

## Now I can get a proportion for each two year category
category.qrisk.data <- anal.dat %>% 
  group_by(date_cat, risk_cat_data) %>%
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)) %>%
  as.data.frame()

head(anal.dat)
category.qrisk.coded <- anal.dat %>% 
  drop_na(qrisk_coded) %>%
  group_by(date_cat, risk_cat_coded) %>%
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)) %>%
  as.data.frame()


### Now want to plot these as line plots
### Do it seperately for data.driven and coded


### QRISK CODED
## First I want to assign an xvalue which is useful for plotting
category.qrisk.coded$date_cat <- as.numeric(category.qrisk.coded$date_cat)-1 + 1998
category.qrisk.coded

qrisk_coded_category_plot <- ggplot(data = category.qrisk.coded, aes(x= date_cat, y = freq, group = risk_cat_coded)) +
  geom_line(aes(color = risk_cat_coded)) + geom_point(aes(color = risk_cat_coded)) + 
  scale_y_continuous(limits=c(0,0.65)) +
  scale_x_continuous(name = "Year", breaks = 1998:2017, labels = paste(c("98","99","00","01","02","03","04","05","06","07","08","09",
                                                                         "10","11","12","13","14","15","16","17"),"-",
                                                                       c("99","00","01","02","03","04","05","06","07","08","09",
                                                                         "10","11","12","13","14","15","16","17","18"),sep=""), limits = c(2011,2017)) +
  geom_vline(xintercept = 2013.5) +
  ylab("Proportion of patients in risk group") +
  labs(color = "Risk\ncategory") + ggtitle("Coded risks")

qrisk_coded_category_plot
ggsave("figures/Vis2019/qrisk_coded_category_plot.jpeg",qrisk_coded_category_plot, dpi = 300)


### QRISK DATA
category.qrisk.data$date_cat <- as.numeric(category.qrisk.data$date_cat)-1 + 1998
category.qrisk.data

qrisk_data_category_plot <- ggplot(data = category.qrisk.data, aes(x= date_cat, y = freq, group = risk_cat_data)) +
  geom_line(aes(color = risk_cat_data)) + geom_point(aes(color = risk_cat_data)) + 
  scale_y_continuous(limits=c(0,0.65)) +
  scale_x_continuous(name = "Year", breaks = 1998:2017, labels = paste(c("98","99","00","01","02","03","04","05","06","07","08","09",
                                                                         "10","11","12","13","14","15","16","17"),"-",
                                                                       c("99","00","01","02","03","04","05","06","07","08","09",
                                                                         "10","11","12","13","14","15","16","17","18"),sep=""), limits = c(2011,2017)) +
  geom_vline(xintercept = 2013.5) +
  ylab("Proportion of patients in risk group") +
  labs(color = "Risk\ncategory") + ggtitle("EHR derived risks")

qrisk_data_category_plot
ggsave("figures/Vis2019/qrisk_data_category_plot.jpeg",qrisk_data_category_plot, dpi = 300)

qrisk_category_plot <- ggarrange(qrisk_data_category_plot, qrisk_coded_category_plot, nrow = 1, ncol = 2, common.legend = TRUE)
qrisk_category_plot
ggsave("figures/Vis2019/Figure3.png",qrisk_category_plot, dpi = 1000)

rm(list=setdiff(ls(),list("mean.plot.data","mean.qrisk.data","mean_qrisk_score_plot","qrisk_data_category_plot","qrisk_coded_category_plot",
                          "category.qrisk.data","category.qrisk.coded","qrisk_category_plot")))

save.image("R_out_Vis2019/run_analysis_risk_change.RData")