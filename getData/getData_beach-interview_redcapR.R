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

# Error: $ operator is invalid for atomic vectors
# https://github.com/nutterb/redcapAPI/issues/126

desired_records_v1 <- c("BIS001A","BIS002A","BIS003A","BIS004A",											
                        "BIS005A","BIS006A","BIS007A","BIS008A","BIS009A","BIS010A","BIS011A","BIS013A",
                        "BIS014A","BIS018A","BIS019A","BIS022A","BIS023A","BIS024A","BIS025A","BIS026A",
                        "BIS027A","BIS028A","BIS029A","BIS030A","BIS031A","BIS032A","BIS033A","BIS034A",
                        "PRG001","PRG002","PRG003","PRG004","PRG005","PRG006","PRG007","PRG008","PRG009",
                        "PRG010","PRG011","PRG012","PRG013","PRG014","PRG015","PRG016","PRG017","PRG018",
                        "PRG020")	

desired_fields_v1 <- c("record_id", "int_consent_date","interphone_prepreg_bmi","interphone_age","int_audio_length_min",
                       "mompa_walk_slow","mompa_walk_quick", "mompa_walk_hills", "mompa_jog", "mompa_prenatal_exer", "mompa_swim",
                       "mompa_dance")

ds_some_rows_v1 <- redcap_read(
  redcap_uri = uri, 
  token      = interview_token, 
  records    = desired_records_v1,
  fields     = desired_fields_v1
  )$data

