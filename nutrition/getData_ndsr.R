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
library(data.table)
library(tidyr)
library(tidyverse)
library(dplyr)
library(reshape2)
library(lubridate)

# **************************************************************************** #
# ***************  Data File: 03                                              
# **************************************************************************** # 

#Read Data
data.file.name="BLS053A03.txt";data.file.name
data.file.path=paste0(data.dir,"\\",data.file.name);data.file.path
dat<- read_tsv(data.file.path, col_names=FALSE);dat
head(dat); str(dat); names(dat)

# record day number
record_day_number=length(as.character(unique(dat[[3]])))

# create average for each column
df=dat %>%
  summarize(mean(X7),mean(X8)) %>%
  select('var'='mean(X7)', everything())

# export data
df1=as.data.frame(df)
  

