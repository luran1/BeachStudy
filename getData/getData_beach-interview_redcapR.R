##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        March 22, 2019 
# IRB:
# Description: RedCap API system for BEACH Interview project

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(keyringr)
library(redcapAPI)
library(REDCapR)

# Login to Gatorlink VPN

# Get Redcap API Token
# # https://cran.r-project.org/web/packages/keyringr/vignettes/Avoiding_plain_text_passwords_in_R_with_keyringr.html
credential_label <- "interview_api"
credential_path <- paste(Sys.getenv("USERPROFILE"), '\\DPAPI\\passwords\\', Sys.info()["nodename"], '\\', credential_label, '.txt', sep="")
uri <- "https://redcap.ctsi.ufl.edu/redcap/api/"
interview_token<-decrypt_dpapi_pw(credential_path)
print(interview_token)

# Create connections
rcon <- redcapConnection(url=uri, token=interview_token)

# list of instruments
exportInstruments(rcon)

# list of events
exportEvents(rcon)

# list records
exportRecords(rcon)

# export field names
exportFieldNames(rcon)

# Variables
desired_fields_v1 <- c("record_id","int_study_grp","interphone_date","int_consent_date","int_consent_complete",
                       "int_interview_date","int_audio_length_min","int_complete", # study 
                       "interphone_prepreg_bmi","interphone_age","mom3t_prepreg_bmi",
                       "mompa_walk_slow","mompa_walk_quick", "mompa_walk_hills", "mompa_jog", "mompa_prenatal_exer", "mompa_swim","mompa_dance", # physical activity
                       "int_guide_education","int_guide_employmnt","int_guide_occupation"
                       )

# pull data
ds_some_rows_v1 <- redcap_read(
  batch_size=150L,
  redcap_uri = uri, 
  token      = interview_token, 
  fields     = desired_fields_v1
  )$data

# check data pull
str(ds_some_rows_v1)
ds_some_rows_v1[1]

# how many consented

# ready for some data analysis!


