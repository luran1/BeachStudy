##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        June 13, 2018 
# IRB:
# Description: Data management for BMI and MOD groupings. 
# Data: C:\Users\djlemas\Dropbox (UFL)\02_Projects\BEACH_STUDY\RedCap
# Obj: Format data and basic analysis.

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location
# location="Dominick";location

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
library(data.table)
library(tidyr)
library(dplyr)
library(reshape2)

# **************************************************************************** #
# ***************  TheBreastfeedingAndE_BMI_MOD_DATA_2018-06-13_2054.csv                                              
# **************************************************************************** # 

#Read Data
data.file.name="TheBreastfeedingAndE_BMI_MOD_DATA_2018-06-13_2054.csv";data.file.name
data.file.path=paste0(data.dir,"\\",data.file.name);data.file.path
dat<- read.csv(data.file.path);dat
head(dat); str(dat); names(dat)

# format variables
df <- dat %>% 
  group_by(test_id) %>%
  select(test_id, redcap_event_name, mom3t_prepreg_bmi, mom2wk_mod) %>%
  mutate(redcap_event_name = factor(redcap_event_name, 
                                    levels = c("third_trimester_arm_1", 
                                               "two_week_arm_1", 
                                               "two_month_arm_1",
                                               "six_month_arm_1",
                                               "twelve_month_arm_1"))) %>%
  mutate(mom3t_prepreg_bmi=first(mom3t_prepreg_bmi)) %>%
  mutate(mom2wk_mod=first(na.omit(mom2wk_mod))) 

# recode
df$bmi_grp=NA
df$bmi_grp=ifelse(df$mom3t_prepreg_bmi<27,1,df$bmi_grp)
df$bmi_grp=ifelse(df$mom3t_prepreg_bmi>29,2,df$bmi_grp)

# drop NA observations
dat.s=df %>%
  na.omit() %>%
  group_by(test_id, redcap_event_name) %>%
  arrange(crc_date_of_service) 

# how much per visit/participant?
test=dat.s %>%
  group_by(test_id, redcap_event_name) %>%
  summarize(count=n_distinct(test_id),
            bill_mean=mean(crc_amount_due, na.rm=T),
            bill_sum=sum(crc_amount_due))
test %>%
  group_by(redcap_event_name) %>%
  summarize(mean(bill_sum),
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

