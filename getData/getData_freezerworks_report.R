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
library(tidyverse)
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

names(dat)

# what is the ordering of redcap_events
levels(dat$clinic_visit)
unique(as.character(dat$Aliquot.Type))
unique(as.character(dat$tube.type))


# look for "2_weeks" vs "2_week"
which(dat$clinic_visit=="2_weeks") 
change=dat%>%
  filter(clinic_visit=="2_weeks")
  print(change$Participant_ID)

# which "3rd Trimester" vs "3rd_trimester"  # BLS059A needs to be changed
  change=dat%>%
    filter(clinic_visit=="3rd Trimester")
  print(change$Participant_ID)
  
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

# output data for import to redcap
redcap=dat.s %>%
  select(Participant_ID,clinic_visit,Clinic.visit.date,Mom_Baby,Aliquot.Type,crc_specimen_barcode,crc_specimen_number,
         tube.type,Aliquot.Number) %>%
  rename(test_id=Participant_ID, 
         biosample_study_visit=clinic_visit,
         biosample_collection_date=Clinic.visit.date,
         biosample_mom_baby=Mom_Baby,
         biosample_aliquot_type=Aliquot.Type,
         crc_specimen_barcode=crc_specimen_barcode,
         crc_specimen_number=crc_specimen_number,
         biosample_tube_type=tube.type,
         biosample_aliquot_numb=Aliquot.Number)%>%
  mutate(redcap_event_name=NA,
         redcap_repeat_instrument="biological_specimen_collection")%>%
  arrange(test_id,biosample_study_visit,biosample_collection_date,biosample_aliquot_type)%>%
  group_by(test_id,biosample_study_visit) %>% 
  mutate(redcap_repeat_instance=row_number())%>%
  select(test_id,redcap_event_name,redcap_repeat_instrument,redcap_repeat_instance,everything())%>%
  mutate(redcap_event_name=case_when(biosample_study_visit=="3rd_trimester" ~ "third_trimester_arm_1",
                                     biosample_study_visit=="2_week" ~ "two_week_arm_1",
                                     biosample_study_visit=="2_months" ~ "two_month_arm_1",
                                     biosample_study_visit=="6_months" ~ "six_month_arm_1",
                                     biosample_study_visit=="12_months" ~ "twelve_month_arm_1"))%>%
  mutate(redcap_event_name = factor(redcap_event_name, 
                               levels = c("third_trimester_arm_1", 
                                          "two_week_arm_1", 
                                          "two_month_arm_1",
                                          "six_month_arm_1",
                                          "twelve_month_arm_1")))%>%
  mutate(biosample_collection_date=format(biosample_collection_date, "%m/%d/%Y"))

# clinic_visit
1, 3rd_trimester
2, 2_week
3, 2_months
4, 6_months
5, 12_months

# mom-baby
0, mom
1, baby

# aliquot type
1, plasma
2, urine
3, saliva
4, milk- skim
5, milk- whole
6, milk-lipid
7, stool
8, vaginal
9, blood
10, formula

# tube type
1, 2ml
2, ez sample
3, vaginal vial
4, 5ml
5, tiny
6, blood card
7, other
8, 15ml
9, saliva tube
10, 50ml

# replace NA with blanks
df <- sapply(redcap, as.character)
df[is.na(df)] <- " "
df1=as.data.frame(df)

# checks
unique(df1$redcap_event_name)
table(df1$redcap_event_name)
table(df1$biosample_study_visit)

# export test data: BLS001A
redcap.bls001=df1%>%
  filter(test_id=="BLS001A")%>%
write_csv(path =paste0(work.dir,"redcap.bls001.csv",na = ""))

# need redcap instance


