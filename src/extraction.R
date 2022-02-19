library(httr)
library(rvest)
library(dplyr)
library(lubridate)
library(stringr)
library(R.utils)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()



if(format(lastModified("../data/vakcinos.csv"), "%Y-%m-%d") < Sys.Date()){
  print("Extracting data...")
  vakcinos <- read.csv("https://opendata.arcgis.com/datasets/ffb0a5bfa58847f79bf2bc544980f4b6_0.csv", encoding="UTF-8")
  atvejai_ir_mirtys <- read.csv("https://opendata.arcgis.com/datasets/ba35de03e111430f88a86f7d1f351de6_0.csv", encoding="UTF-8")
  covid19_statistika_dashboards <- read.csv("https://opendata.arcgis.com/datasets/d49a63c934be4f65a93b6273785a8449_0.csv", encoding="UTF-8")
  
  adm <- read.csv("https://raw.githubusercontent.com/mpiktas/covid19lt/master/raw_data/administrative_levels.csv", encoding="UTF-8")
  
  vakcinos %>%
    write.csv("../data/vakcinos.csv", row.names = FALSE)
  
  atvejai_ir_mirtys %>%
    arrange(desc(date, municipality_code)) %>%
    write.csv("../data/atvejai_ir_mirtys.csv", row.names = FALSE)
  
  covid19_statistika_dashboards %>%
    arrange(desc(date, municipality_code))%>%
    write.csv("../data/covid19_statistika_dashboards.csv", row.names = FALSE)
  
}else{
  print("Uploading vakcinos.csv")
  vakcinos <- data.frame(read.csv("../data/vakcinos.csv",
                                  header = T,
                                  encoding = "UTF-16"))
  print("Uploading atvejai_ir_mirtys.csv")
  atvejai_ir_mirtys <- data.frame(read.csv("../data/atvejai_ir_mirtys.csv",
                                  header = T,
                                  encoding = "UTF-16"))
  print("Uploading covid19_statistika_dashboards.csv")
  covid19_statistika_dashboards <- data.frame(read.csv("../data/covid19_statistika_dashboards.csv",
                                  header = T,
                                  encoding = "UTF-16"))
}




