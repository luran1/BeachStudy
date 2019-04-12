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

# Directory Locations
work.dir=paste0(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_Study\\Export\\");work.dir
data.dir=paste0(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_Study\\Export\\");data.dir

# Set Working Directory
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(tidyr)
library(dplyr)

# **************************************************************************** #
# ***************  BEACH_DATA_EXPORT_11APR19.txt                                              
# **************************************************************************** # 

# Read Data
data.file.name="BEACH_DATA_EXPORT_11APR19.csv";data.file.name
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

# look for "2_weeks" vs "2_week"
which(dat$clinic_visit=="2_weeks") 
change=dat%>%
  filter(clinic_visit=="2_weeks")
  print(change$part_id)
  

# set the order of redcap_events
df <- dat %>% 
  mutate(clinic_visit = factor(clinic_visit, 
                                    levels = c("3rd_trimester", 
                                               "2_week", 
                                               "2_months",
                                               "6_months",
                                               "12_months")))
# check odering of levels
levels(df$clinic_visit)
table(df$clinic_visit)

# change dates
df$Clinic.visit.date=as.Date(df$Clinic.visit.date, "%m/%d/%Y")
dim(df) # 1995
length(unique(df$Participant_ID)) #85

names(df)
df$Mom_Baby

# create mom-baby variable
# d1=df%>%
#   mutate(part_id=as.character(Participant_ID))
#   d1$mom_baby=ifelse(grepl("A",d1$part_id)==T,"mom",d1$part_id)
#   d1$mom_baby=ifelse(grepl("B$",d1$part_id)==T,"baby",d1$mom_baby)
#   head(d1)
#   d1[,c("Participant_ID","mom_baby","part_id2")]
  
# create paired mom-baby
df$part_id_link=gsub("A","",df$Participant_ID)
df$part_id_link=gsub("B$","",df$part_id_link)
 
# drop NA observations
dat.s=df %>%
  group_by(Participant_ID, clinic_visit) %>%
  arrange(Clinic.visit.date) 
  dim(dat.s) # 2021
  length(unique(dat.s$Participant_ID)) #85
  names(dat.s)
  
# how many visits
  dat.s%>%
    group_by(clinic_visit)%>%
    summarize(count=n_distinct(part_id_link))
  table(dat.s$clinic_visit)

# how many tubes per participant?
part_count=dat.s %>%
  group_by(part_id_link) %>%
  summarize(count=n_distinct(crc_specimen_barcode))
  mean(part_count$count) # 43.4 tubes
            
# how many sample types
dat.s %>%
  group_by(Aliquot.Type) %>%
  summarize(count=n_distinct(crc_specimen_barcode))

# how many tubes per sample type
dat.s %>%
  group_by(Freezer.Section) %>%
  summarize(count=n_distinct(crc_specimen_barcode))

# what about those that have completed 12-month visit
table(dat.s$clinic_visit)  #43 sample
year.complete=dat.s %>%
  filter(any(clinic_visit %in% "12_months"))
dim(year.complete)
length(unique(year.complete$Participant_ID)) # 9 participants (mom-baby)
length(unique(year.complete$part_id_link)) # 5 participants (mom-baby)
# why is there a missing mom-baby?
year.all=dat.s%>%
filter(part_id_link%in%c("BLS001","BLS002","BLS003","BLS011","BLS016"))

# how many tubes per sample type- AMONG - people completed 12-month visit
d2=year.all %>%
  group_by(part_id_link,Freezer.Section)%>%
  summarize(count=n_distinct(crc_specimen_barcode))

d2%>%
  group_by(Freezer.Section)%>%
  summarize(mean=mean(count),
            min=min(count),
            max=max(count))

# n= 5 mom-babies
# need to compute how many samples for 100 mom-baby pairs
# need to compute number of boxes, racks and shelves.
# Freezer.Section   mean   min   max
# <fct>            <dbl> <dbl> <dbl>
# 1 2ml Box 9*9      18       10    25
# 2 5ml Box 7*7      22       12    31
# 3 Blood Card Box    2        1     3
# 4 EZ Sampler Box    4.2      3     5
# 5 Mixed Box         6.33     3     9
# 6 Vaginal Swab Box  3.2      2     5

