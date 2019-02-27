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
location="djlemas";location

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
# library(data.table)
# library(tidyr)
library(tidyverse)
library(dplyr)
# library(reshape2)
# library(lubridate)

# **************************************************************************** #
# ***************  Data File: 03                                              
# **************************************************************************** # 

# Input RedCap Names
redcap.file.name="NDSR_names.csv";redcap.file.name
redcap.file.path=paste0(data.dir,"\\",redcap.file.name);redcap.file.path
redcap<- read_csv(redcap.file.path, col_names=TRUE);redcap
head(redcap); str(redcap); names(redcap)
  # format data
  redcap$X6=paste0("X",redcap$column)
  redcap=redcap%>%
    select(everything(),"ndsr_col"="X6")

# Input Diet Data
ndsr.file.name="BLS053A03.txt";ndsr.file.name
ndsr.file.path=paste0(data.dir,"\\",ndsr.file.name);ndsr.file.path
dat<- read_tsv(ndsr.file.path, col_names=FALSE);dat
head(dat); str(dat); names(dat)

# merge names from redcap into ndsr
overlap.old=intersect(redcap$ndsr_col,names(dat))
length(names(dat))
length(redcap$ndsr_col)
length(overlap.old)
dat1=dat
names(dat1)=ifelse(names(dat) %in% redcap$ndsr_col, redcap$variable_name, NA)
names(dat)
names(dat1)

# drop columns with NA
dat2=dat1%>%
  select(!NA)

# record day number
record_day_number=length(as.character(unique(dat1[[3]])))

# create average for each column
df=dat1 %>%
  summarize(mean(X7),mean(X8)) %>%
  select('var'='mean(X7)', everything())

# export data
df1=as.data.frame(df)
  

