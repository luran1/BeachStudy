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

desired_records_v1 <- c("BIS001A")
desired_fields_v1 <- c("record_id", "int_consent_date")

ds_some_rows_v1 <- redcap_read(
  redcap_uri = uri, 
  token      = interview_token, 
  records    = desired_records_v1,
  fields     = desired_fields_v1
)$data

