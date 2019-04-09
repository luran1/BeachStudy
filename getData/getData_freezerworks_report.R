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
out.dir=paste0(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_Study\\Export\\");out.dir

# Set Working Directory
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(tidyr)
library(dplyr)

# **************************************************************************** #
# ***************  freezerExport_BEACH_27Mar19.txt                                              
# **************************************************************************** # 

# Read Data
data.file.name="freezerExport_BEACH_27Mar19.csv";data.file.name
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
                                               "2_month",
                                               "6_months",
                                               "12_month")))
# check odering of levels
levels(df$clinic_visit)

# change dates
df$Clinic.visit.date=as.Date(df$Clinic.visit.date, "%m/%d/%Y")

# drop NA observations
dat.s=df %>%
  group_by(Participant_ID, clinic_visit) %>%
  arrange(Clinic.visit.date) 

# how many tubes per participant?
part_count=dat.s %>%
  group_by(Participant_ID) %>%
  summarize(count=n_distinct(crc_specimen_barcode))
  mean(part_count$count) # 23.5 tubes
            
# how many tubes per sample type
dat.s %>%
  group_by(Aliquot.Type) %>%
  summarize(count=n_distinct(crc_specimen_barcode))


# what about those that have completed 12-month visit
dat.s %>%
  mutate(visit_score=)
  filter(clinic_visit=="12_month")
