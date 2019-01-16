##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:           Dominick Lemas 
# Start Date:       January 15, 2019 
# IRB:
# Description: Compute NDSR report using raw NDSR output data 
# Data: 
# Obj: Format ndsr data to output averages for macro/micro nutrients

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\NDSR\\BLS053A",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\NDSR\\BLS053A",sep="");data.dir
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\NDSR\\BLS053A",sep="");out.dir

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
# ***************  Data File: 03                                              
# **************************************************************************** # 

#Read Data
data.file.name="BLS053A03.txt";data.file.name
data.file.path=paste0(data.dir,"\\",data.file.name);data.file.path
dat<- read.csv(data.file.path, sep="\t", header=F);dat
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
df$bmi_grp=ifelse(df$mom3t_prepreg_bmi<27,"NW",df$bmi_grp)
df$bmi_grp=ifelse(df$mom3t_prepreg_bmi>29,"Ob",df$bmi_grp)
df$bmi_grp=as.factor(df$bmi_grp)
df$mom2wk_mod=as.factor(df$mom2wk_mod)
df$mod==NA
df$mod=ifelse(df$mom2wk_mod==2,"CS",df$mom2wk_mod)
df$mod=ifelse(df$mom2wk_mod==1,"VG",df$mod)

# Select out data
df %>%
  group_by(bmi_grp,mod) %>%
  filter(redcap_event_name=='third_trimester_arm_1') %>%
  tally() 


  

