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

# create mom-baby variable
d1=df%>%
  mutate(part_id=as.character(Participant_ID))
  d1$mom_baby=ifelse(d1$part_id %in% grepl("A",d1$part_id),"mom",d1$part_id)

  mutate(mom_baby=ifelse(part_id %in% grepl("A",part_id),"mom",part_id))


grepl("A",d1$mom_baby)

# drop NA observations
dat.s=df %>%
  group_by(Participant_ID, clinic_visit) %>%
  arrange(Clinic.visit.date) 
  dim(dat.s) # 1995
  length(unique(dat.s$Participant_ID)) #85
  names(dat.s)
  
# how many visits
  dat.s%>%
    group_by(clinic_visit)%>%
    summarize(count=n_distinct(Participant_ID))
  table(dat.s$clinic_visit)


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
