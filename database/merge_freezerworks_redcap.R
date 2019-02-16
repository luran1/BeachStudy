##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        February 08, 2019 
# IRB:
# Description: Data management to merge freezerworks and redcap data.  
# Data: C:\Users\djlemas\Dropbox (UFL)\02_Projects\BEACH_STUDY\RedCap
# Obj: export merged dataset that can link freezerworks and redcap.

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\RedCap\\ALL_DATA\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\RedCap\\ALL_DATA\\",sep="");data.dir
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\RedCap\\tables\\",sep="");out.dir

# Set Working Directory
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

# library(readxl)
# install.packages(data.table)
library(data.table)
library(tidyr)
library(dplyr)
library(reshape2)

# **************************************************************************** #
# ***************  TheBreastfeedingAndE_BILLING_DATA_2018-06-08_1318.csv                                              
# **************************************************************************** # 

#Read Data
data.file.name="TheBreastfeedingAndE_BILLING_DATA_2018-06-08_1318.csv";data.file.name
data.file.path=paste0(data.dir,"\\",data.file.name);data.file.path
billing<- read.csv(data.file.path);

# look at data
dat=billing
head(dat); str(dat); names(dat)

# **************************************************************************** #
# ***************  General data formatting                                             
# **************************************************************************** # 

# what is the ordering of redcap_events
levels(dat$redcap_event_name)
levels(df$redcap_event_name)

# format variables
df <- dat %>% 
  mutate(redcap_event_name = factor(redcap_event_name, 
                                    levels = c("third_trimester_arm_1", 
                                               "two_week_arm_1", 
                                               "two_month_arm_1",
                                               "six_month_arm_1")))

# drop NA observations
dat.s=df %>%
  na.omit() %>%
  group_by(test_id, redcap_event_name) %>%
  arrange(crc_date_of_service) 

# how much per visit/participant?
test=dat.s %>%
  group_by(test_id, redcap_event_name) %>%
  summarize(count=n_distinct(crc_service),
            bill_mean=mean(crc_amount_due, na.rm=T),
            bill_sum=sum(crc_amount_due))
test %>%
  group_by(redcap_event_name) %>%
  summarize(count=n_distinct(test_id),
    mean(bill_sum),
            min(bill_sum),
            max(bill_sum))

# how much per visit?
dat.s %>%
  group_by(redcap_event_name) %>%
  summarize(count=n_distinct(test_id),
            bill_mean=mean(crc_amount_due, na.rm=T),
            bill_sum=sum(crc_amount_due))

# 3rd  $100
# 2wk  $160
# 2mo  $160
# 12mo $160

# CRC= $640
# Inc= $160
# Part= $800

80*800= $64,000

