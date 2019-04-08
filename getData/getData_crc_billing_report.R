##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        March 23, 2019 
# IRB:
# Description: Data management for CTSI/CRC billing and services data 
#              extracted from RedCap. 
# Data: C:\Users\djlemas\Dropbox (UFL)\02_Projects\BEACH_STUDY\RedCap\ALL_DATA
# Obj: Format data and basic analysis.

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
location="djlemas";location

# Directory Locations
work.dir=paste0(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\RedCap\\ALL_DATA\\",sep="");work.dir
data.dir=paste0(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\RedCap\\ALL_DATA\\",sep="");data.dir
out.dir=paste0(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\RedCap\\tables\\",sep="");out.dir

# Set Working Directory
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(tidyr)
library(dplyr)
library(ggplot2)
library(keyringr)
library(redcapAPI)
library(REDCapR)

# **************************************************************************** #
# ***************  Pull data from redcap with api                                              
# **************************************************************************** # 

# Get Redcap API Token
# # https://cran.r-project.org/web/packages/keyringr/vignettes/Avoiding_plain_text_passwords_in_R_with_keyringr.html
credential_label <- "beach_api"
credential_path <- paste(Sys.getenv("USERPROFILE"), '\\DPAPI\\passwords\\', Sys.info()["nodename"], '\\', credential_label, '.txt', sep="")
uri <- "https://redcap.ctsi.ufl.edu/redcap/api/"
beach_token<-decrypt_dpapi_pw(credential_path)
print(beach_token)

# Create connections
rcon <- redcapConnection(url=uri, token=beach_token)

# crc variables
desired_fields_v1=c("test_id","redcap_event_name","redcap_repeat_instrument",
            "redcap_repeat_instance","crc_date_of_service","crc_service",
            "crc_unit_cost", "crc_billable_quant", "crc_amount_due")  

# events to retain
exportEvents(rcon)
events_to_retain  <- c("third_trimester_arm_1", "two_week_arm_1", "two_month_arm_1", "twelve_month_arm_1")

# list of instruments
exportInstruments(rcon)

# list of events
exportEvents(rcon)

# list records
exportRecords(rcon)

# export field names
exportFieldNames(rcon)

# consented records
consent.records.v1=c("BLS001A","BLS002A","BLS003A","BLS006A",
                  "BLS007A","BLS008A","BLS010A","BLS011A",
                  "BLS012A","BLS013A","BLS014A","BLS015A",
                  "BLS016A","BLS019A","BLS020A","BLS023A",
                  "BLS025A","BLS026A","BLS027A","BLS028A",
                  "BLS029A","BLS030A","BLS032A","BLS033A",
                  "BLS034A","BLS035A","BLS036A","BLS037A",
                  "BLS038A","BLS039A","BLS040A","BLS041A",
                  "BLS043A","BLS044A","BLS045A","BLS048A",
                  "BLS049A","BLS050A","BLS051A","BLS052A",
                  "BLS053A", "BLS054A","BLS055A","BLS056A",
                  "BLS057A","BLS058A","BLS059A","BLS060A")

# pull data
ds_some_rows_v1 <- redcap_read(
  batch_size=300,
  records= consent.records.v1,
  redcap_uri = uri, 
  token      = beach_token, 
  fields     = desired_fields_v1
)$data

# look at data
dat=billing
head(dat); str(dat); names(dat)

# **************************************************************************** #
# ***************  General data formatting                                             
# **************************************************************************** # 

# what is the ordering of redcap_events
levels(dat$redcap_event_name)

# set the order of redcap_events
df <- dat %>% 
  mutate(redcap_event_name = factor(redcap_event_name, 
                                    levels = c("baseline_arm_1",
                                               "third_trimester_arm_1", 
                                               "two_week_arm_1", 
                                               "two_month_arm_1",
                                               "six_month_arm_1",
                                               "twelve_month_arm_1")))
# check odering of levels
levels(df$redcap_event_name)

# set the services ordering


# drop NA observations
dat.s=df %>%
  na.omit() %>%
  group_by(test_id, redcap_event_name) %>%
  arrange(crc_date_of_service) 

# how much per visit/participant?
test=dat.s %>%
  group_by(test_id, redcap_event_name) %>%
  summarize(count=n_distinct(crc_service),
            bill_mean=mean(crc_amount_due, na.rm=T),
            bill_sum=sum(crc_amount_due))
test %>%
  group_by(redcap_event_name) %>%
  summarize(count=n_distinct(test_id),
    mean(bill_sum),
            min(bill_sum),
            max(bill_sum))

# how much per visit?
dat.s %>%
  group_by(redcap_event_name) %>%
  summarize(count=n_distinct(test_id),
            bill_mean=mean(crc_amount_due, na.rm=T),
            bill_sum=sum(crc_amount_due))

#ggplot2 histgram bar graph

#Adjusting the crc_date_of_service to be date and joining the test and test2 data frames
test1 <- dat.s%>%
  mutate(DATE = as.Date(crc_date_of_service, format = "%m/%d/%Y"))%>%
  mutate(DATE = strftime(DATE,"%m/%Y"))

test2 <- inner_join(test1,test)
test2 <- arrange(test2, DATE)



theme_set(theme_classic())

# distribution of visits each month(histogram)
h <- ggplot(test2, aes(DATE, bill_sum)) + scale_fill_brewer(palette = "Spectral")
h + geom_histogram(aes(fill=redcap_event_name), stat = "Identity",
                   bins=24,
                   col="black", 
                   size=.1) +   # change number of bins
  labs(title="Clinical visits per Month cost breakdown", 
       subtitle="from july of 2017-January 2019") +
  theme(axis.text.x = element_text(angle=70, vjust =.6))

# cost of each event as of now(bar chart)
b <- ggplot(test2,aes(redcap_event_name,bill_sum)) 
b + geom_bar(stat="identity", width=.5, fill="orange3",
             size =.1)+ 
  labs(title="cost total of each redcap event",
       subtitle="costs are done through billing sum of previous data frame") +
  theme(axis.text.x = element_text(angle=70, vjust =.6))

# a box plot of the clincal visits by cost 
bp <- ggplot(test2, aes(redcap_event_name, crc_amount_due))
bp + geom_boxplot(varwidth=T, fill="red2") + 
  labs(title="Box plot", 
       subtitle="Cost grouped by Clinical Visits",
       caption="Source: test2",
       x="clinical visit",
       y="billing amount") + 
  theme(axis.text.x = element_text(angle=70, vjust =.6))






# 3rd  $100
# 2wk  $160
# 2mo  $160
# 12mo $160

# CRC= $640
# Inc= $160
# Part= $800

80*800= $64,000

# total costs over time from 2016-2019.


