##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        March 23, 2019 
# IRB:
# Description: Data management for CTSI/CRC billing and services data 
#              extracted from RedCap. 
# Data: C:\Users\djlemas\Dropbox (UFL)\02_Projects\BEACH_STUDY\RedCap\ALL_DATA
# Obj: Format data and basic analysis.

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
location="djlemas";location

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

library(tidyr)
library(dplyr)

# **************************************************************************** #
# ***************  BEACH-CRCBilling_DATA_2019-03-23_1057.csv                                              
# **************************************************************************** # 

#Read Data
data.file.name="BEACH-CRCBilling_DATA_2019-03-23_1057.csv";data.file.name
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

# set the order of redcap_events
df <- dat %>% 
  mutate(redcap_event_name = factor(redcap_event_name, 
                                    levels = c("baseline_arm_1",
                                               "third_trimester_arm_1", 
                                               "two_week_arm_1", 
                                               "two_month_arm_1",
                                               "six_month_arm_1",
                                               "twelve_month_arm_1")))
# check odering of levels
levels(df$redcap_event_name)

# set the services ordering


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





# My working code 

library(tidyr)
library(dplyr)
library(ggplot2)

# Reading in the RedCap data excel sheet
df <- read.table("TheBreastfeedingAndE-CRCBilling_DATA_2019-04-15_1133.txt", header = TRUE)



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



library(tidyr)
library(dplyr)
library(ggplot2)

# Reading in the RedCap data excel sheet
df <- read.table("TheBreastfeedingAndE-CRCBilling_DATA_2019-04-15_1133.txt", header = TRUE)



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


#arranging the data frame by date of service and organizing by month and year
test1 <- dat.s%>%
  mutate(crc_date_of_service = as.Date(crc_date_of_service, format = "%m/%d/%Y"))%>%
  mutate(crc_date_of_service = strftime(crc_date_of_service, "%y-%m"))%>%
  arrange(crc_date_of_service)


#setting the theme for the graphs   
theme_set(theme_classic())

# distribution of visits each month(histogram) vs cost
h <- ggplot(test1, aes(crc_date_of_service, crc_amount_due)) + scale_fill_brewer(palette = "Spectral")
h + geom_histogram(aes(fill=redcap_event_name), stat = "Identity",
                   bins=24,
                   col="black", 
                   size=.1) +   # change number of bins
  labs(title="Clinical visits per Month cost breakdown", 
       subtitle="from july of 2017-January 2019") +
  theme(axis.text.x = element_text(angle=70, vjust =.6))





# cost of each event as of now(bar chart)
b <- ggplot(test1,aes(redcap_event_name,crc_amount_due)) 
b + geom_bar(stat="identity", width=.5, fill="orange3",
             size =.1)+ 
  labs(title="cost total of each redcap event",
       subtitle="costs are done through billing sum of previous data frame") +
  theme(axis.text.x = element_text(angle=70, vjust =.6))








# 3rd  $100
# 2wk  $160
# 2mo  $160
# 12mo $160

# CRC= $640
# Inc= $160
# Part= $800

80*800= $64,000

# total costs over time from 2016-2019.


