pacman::p_load(httr, readr, tidyverse)

base.url<-"https://kobo.msf.org/"
url<-paste0(base.url,"api/v2/")

token <- "b27f6edd57100d09d7c0021b8fc04dec828edbdd"

url <- paste0(url,
       paste0("assets", "?"))
r<-content(GET(url),as="parsed")
do.call(rbind.data.frame,r$dataElements)


install.packages("remotes")
remotes::install_gitlab("dickoa/robotoolbox")

library("robotoolbox")
token <- kobo_token(username = "cxb_epi_data", password = "CXB_EAT_MRT_1971", 
                    url = "https://kobo.msf.org/")

kobo_setup(url = "https://kobo.msf.org/", token = token)
kobo_settings()

library(dplyr)
l <- kobo_asset_list()

#daily signal tally
daily_signal_tally <- kobo_data(kobo_asset("acfGrxnmkTaFvSTdHnrQ3x"))

#signal verification
signal_verification <- kobo_data(kobo_asset("a9bYeVttuVuSzCcgmk9qpo"))

#field assessment
field_assessment <- kobo_data(kobo_asset("a4V5jXFEHjwALe8ztWQNvX"))

#risk assessment
risk_assessment <- kobo_data(kobo_asset("a6GKjVvR7dyEPeXDtQ9U37"))

#response
response <- kobo_data(kobo_asset("a5BCfuooic3qN8Y27pR44M"))


