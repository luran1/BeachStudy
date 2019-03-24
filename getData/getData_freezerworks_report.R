##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        March 23, 2019 
# IRB:
# Description: Data management for freezerworks aliquot data. 
# Data: C:\Users\djlemas\Dropbox (UFL)\02_Projects\FREEZERWORKS\BEACH_Study\Export
# Obj: Format data and basic analysis.

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
location="djlemas";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_Study\\Export\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_Study\\Export\\",sep="");data.dir
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_Study\\Export\\",sep="");out.dir

# Set Working Directory
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(tidyr)
library(dplyr)

# **************************************************************************** #
# ***************  BEACH-CRCBilling_DATA_2019-03-23_1057.csv                                              
# **************************************************************************** # 

#Read Data
data.file.name="Test_Export.csv";data.file.name
data.file.path=paste0(data.dir,"\\",data.file.name);data.file.path
freezer<- read.csv(data.file.path);

# look at data
dat=freezer
head(dat); str(dat); names(dat)

# **************************************************************************** #
# ***************  General data formatting                                             
# **************************************************************************** # 

# what is the ordering of redcap_events
levels(dat$clinic_visit)
# 
# [1] ""              "12_months"     "2_months"      "2_week"        "2_weeks"       "3rd_trimester"
# [7] "6_months"

# need to identify the 2_week and 2_weeks entries. 
# 50  187  206 1485 1794 1901
which(dat$clinic_visit=="2_weeks")

# set the order of redcap_events
df <- dat %>% 
  mutate(clinic_visit = factor(clinic_visit, 
                                    levels = c("3rd_trimester", 
                                               "2_week", 
                                               "2_month",
                                               "6_months",
                                               "12_month")))
# check odering of levels
levels(df$clinic_visit)

# drop NA observations
dat.s=df %>%
  na.omit() %>%
  group_by(part_id, clinic_visit) %>%
  arrange(date) 

# how many tubes per participant?
dat.s %>%
  group_by(part_id,clinic_visit) %>%
  summarize(count=n_distinct(barcode))
            

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


