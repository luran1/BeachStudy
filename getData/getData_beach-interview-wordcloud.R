##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        March 27, 2019 
# IRB:
# Description: wordcloud data analysis for BEACH Interview project 
# Data: C:\Users\djlemas\Dropbox (UFL)\02_Projects\BEACH_INTERVIEW\03_Nvivo_Analysis\wordcloud
# Obj: Format data and basic analysis.

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
location="djlemas";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_INTERVIEW\\03_Nvivo_Analysis\\wordcloud\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_INTERVIEW\\03_Nvivo_Analysis\\wordcloud\\",sep="");data.dir
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_INTERVIEW\\03_Nvivo_Analysis\\wordcloud\\",sep="");out.dir

# Set Working Directory
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(tidyr)
library(dplyr)
library(readxl)


# **************************************************************************** #
# ***************  BIS Participants-WFQ.xlsx                                              
# **************************************************************************** # 

# file parameters
n_max=10000
bf.file.name="BIS Participants-WFQ.xlsx";bf.file.name


#Read Data
bf.group=read_xlsx(paste0(data.dir,bf.file.name), sheet = "Sheet1", range = NULL, col_names = TRUE,
                      col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                      guess_max = min(1000, n_max));bf.group


# **************************************************************************** #
# ***************  PRG Participants-WFQ.xlsx                                              
# **************************************************************************** # 

# file parameters
n_max=10000
prego.file.name="PRG Participants-WFQ.xlsx";prego.file.name


#Read Data
prego.group=read_xlsx(paste0(data.dir,prego.file.name), sheet = "Sheet1", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));prego.group


# **************************************************************************** #
# ***************  General data formatting                                             
# **************************************************************************** # 

# breastfeeding group
bf.group.trim=bf.group%>%
  select(Word,Length)%>%
  rename_all(tolower)
  bf.group.trim$group=c("bf")

# pregnant group
preg.group.trim=prego.group%>%
    select(Word,Length)%>%
    rename_all(tolower)
   preg.group.trim$group=c("preg")
  
# merge data
cloud=bind_rows(list(bf.group.trim, preg.group.trim))

# what is the ordering of redcap_events
cloud$group=as.factor(cloud$group)

# ready for word cloud analysis
