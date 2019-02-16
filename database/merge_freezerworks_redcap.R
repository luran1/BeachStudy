##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        February 08, 2019 
# IRB:
# Description: Data management to merge freezerworks and redcap data.  
# Data: C:\Users\djlemas\Dropbox (UFL)\02_Projects\BEACH_STUDY\RedCap
# Obj: export merged dataset that can link freezerworks and redcap.


# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(readr)
library(dplyr)

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_Study\\merge\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_Study\\merge\\",sep="");data.dir
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_Study\\merge\\",sep="");out.dir

# Set Working Directory
setwd(work.dir)
list.files()

# Data file names
data.file.name.freezerworks="BEACH_Freezer_Export_13Feb19.txt";data.file.name.freezerworks
data.file.name.redcap="RedCap_Freezerworks_Link_noPHI_1158Lemas_13Feb19.txt";data.file.name.redcap

# **************************************************************************** #
# ***************  Import Data                                              
# **************************************************************************** # 

#Read RedCap Data
#----------------
redcap.file.path=paste0(data.dir,"",data.file.name.redcap);redcap.file.path
redcap<- read_tsv(redcap.file.path);

# look at data
head(redcap); str(redcap); names(redcap)
length(redcap$Participant_ID)               # 1217
length(unique(redcap$crc_specimen_barcode)) # 1181
length(unique(redcap$Participant_ID))       # 68

# format for merge
df.r=redcap%>%
  select(Participant_ID, crc_specimen_number, crc_specimen_barcode, clinic_visit_date.r, clinic_visit)

#Read fREEZERWORKS Data
#----------------
freezer.file.path=paste0(data.dir,"",data.file.name.freezerworks);freezer.file.path
freezer<- read_tsv(freezer.file.path);

# look at data
head(freezer); str(freezer); names(freezer)
length(freezer$Participant_ID)               # 1860
length(unique(freezer$crc_specimen_barcode)) # 1860
length(unique(freezer$Participant_ID))       #83

# format for merge
df.f=freezer%>%
  select(Participant_ID, crc_specimen_number, crc_specimen_barcode, "Globally Unique Aliquot ID", clinic_visit_date, clinic_visit)%>%
  rename(globally_unique_aliquot_ID="Globally Unique Aliquot ID")


# **************************************************************************** #
# ***************  merging data sets
# **************************************************************************** # 

# freezerworked MERGED into Redcap. Size of data set will be redcap # rows

merge.r=left_join(df.r, df.f, by=c("Participant_ID","crc_specimen_barcode"))
length(unique(merge.r$crc_specimen_barcode)) # 1181
length(unique(merge.r$Participant_ID))       # 68

# **************************************************************************** #
# ***************  format data set
# **************************************************************************** #

merge.r1=merge.r%>%
  rename(crc_specimen_number=crc_specimen_number.x, crc_specimen_number=crc_specimen_number.y)

names(merge.r1)
# what is the ordering of redcap_events
levels(merge.r$clinic_visit)

# format variables
df <- dat %>% 
  mutate(redcap_event_name = factor(redcap_event_name, 
                                    levels = c("3rd_trimester", 
                                               "2_week", 
                                               "2_months",
                                               "6_months",
                                               "12_months")))

# **************************************************************************** #
# ***************  Export data set
# **************************************************************************** #

names(merge.r)
merged.file.name.redcap="merged_redcap_freezer.txt"
merge.file.path=paste0(out.dir,"",merged.file.name.redcap="merged_redcap_freezer.txt");merge.file.path
write.csv(merge.r, file=merge.file.path,row.names=FALSE)

