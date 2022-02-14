library(data.table)
library(ggplot2)
library(tidyverse) # for data manipulation (mutate, filter, ymd etc)
library(rgdal) # Bindings for the 'Geospatial' Data Abstraction Library
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
setwd('..')
getwd()
# Uploading data sets
vakcinos <- data.frame(read.csv("data//COVID19_vakcinavimas.csv",
                                header = T,
                                encoding = "UTF-8"))

atvejai_ir_mirtys <- data.frame(read.csv("data//Agreguoti_COVID19_atvejai_ir_mirtys.csv",
                                         header = T,
                                         encoding = "UTF-8"))

covid19_statistika_dashboards <- data.frame(read.csv("data//COVID19_statistika_dashboards.csv",
                                                     header = T,
                                                     encoding = "UTF-8"))


# It is always a good practice to check what kind of data we have with str() and/or head() functions
str(vakcinos)
str(atvejai_ir_mirtys)
head(vakcinos)
head(atvejai_ir_mirtys)

# Checking the uniqueness of variables
unique(atvejai_ir_mirtys$age_gr)
unique(atvejai_ir_mirtys$sex)
unique(atvejai_ir_mirtys$municipality_name)



# Data cleaning part: cleaning data that has age groups as "Nenustatyta" or "Centenariniai"
df_atvejai_ir_mirtys <- filter(atvejai_ir_mirtys, municipality_name != "Nenustatyta")





# COVID19 deaths by age groups - just for fun
plot1 <- filter(df_atvejai_ir_mirtys, !(age_gr == "Nenustatyta" | age_gr == "Centenarianai"))
ggplot(plot1, aes(x = age_gr, y = deaths_cov1)) +
  geom_bar(stat = "identity", fill = "#004980", alpha = 0.6) +
  stat_summary(aes(label = stat(y)), fun = 'sum', geom = 'text', col = 'black', vjust = -0.3) +
  scale_y_continuous(labels = function(l) {l = l / 1000; paste0(l, "K")})  +
  #xlab("Age groups") + ylab("Covid19 deaths")
  labs(
    title = "Deaths per age group",
    subtitle = "Lithuania",
    x = "Age groups",
    y="Covid19 deaths"
  )


df_atvejai_ir_mirtys$covid_deaths <- rowSums( df_atvejai_ir_mirtys[,8:10] )





Lithuania <- aggregate(cbind(df_atvejai_ir_mirtys$new_cases,
                             df_atvejai_ir_mirtys$deaths_all,
                             df_atvejai_ir_mirtys$deaths_cov1,
                             df_atvejai_ir_mirtys$deaths_cov2,
                             df_atvejai_ir_mirtys$deaths_cov3),
                       by=list(Category=df_atvejai_ir_mirtys$municipality_name), FUN=sum)

#Lithuania <-Lithuania[-c(7), ] 


# Some data manipulation for popout boxes
vakcinos <- vakcinos[!duplicated(vakcinos$pseudo_id), ]
vakcinacija <- aggregate(vakcinos$pseudo_id,by=list(Category=vakcinos$muni_declared), FUN = length)
vakcinacija <- vakcinacija %>% 
  rename(
    pasiskiepije = x)


atvejai_ir_mirtys$date <- as.Date(atvejai_ir_mirtys$date, format =  "%Y-%m-%d")
atveju_skaicius <- aggregate(cbind(atvejai_ir_mirtys$new_cases,
                                   atvejai_ir_mirtys$deaths_all,
                                   atvejai_ir_mirtys$deaths_cov1,
                                   atvejai_ir_mirtys$deaths_cov2,
                                   atvejai_ir_mirtys$deaths_cov3),
                             by=list(Category=atvejai_ir_mirtys$date), FUN=sum)

# let's plot 7-days average graphs of new cases and deaths to see the (obvious) relation
require(RcppRoll)
atveju_skaicius$cases_7day <- roll_mean(atveju_skaicius$V1, n = 7, align = "right", fill = NA)
atveju_skaicius$cases_7day <- as.numeric(format(atveju_skaicius$cases_7day, digits = 4))
atveju_skaicius$deaths_7day <- roll_mean(atveju_skaicius$V3, n = 7, align = "right", fill = NA)
atveju_skaicius$deaths_7day <- as.numeric(format(atveju_skaicius$deaths_7day, digits = 4))





# Let's have a look at cases and deaths registered in the whole Lithuania
atveju_skaicius_cum <- filter(atveju_skaicius, Category > '2020-02-07')
str(atveju_skaicius_cum)
coeff <- 30
atveju_skaicius %>%
  ggplot(aes(Category, cases_7day)) +
  geom_line(color = "orange") +
  theme(legend.position = "none") +
  geom_line(aes(x = Category, y = deaths_7day * coeff), color = "red") +
  scale_y_continuous(
    labels = scales::comma,
    name = "Cases",
    sec.axis = sec_axis(deaths_7day ~ . / coeff,
                        name = "Deaths",
                        labels = scales::comma)) +
  theme(
    axis.title.y = element_text(color = "orange", size = 17),
    axis.title.y.right = element_text(color = "red", size = 17)) +
  labs(
    title = "Lithuania: Cases vs. Deaths",
    subtitle = "7-Day Average",
    x = "Date")

#-------------------------------------------------------------------------------------------------------------

# uploading Lithuania map and its shapes by regions
my_spdf <- readOGR( 
  dsn= "shapes", 
  layer="admin_ribos",
  use_iconv = T,
  encoding = "UTF-8"
)

# As before, it is nice to look at the data
head(my_spdf@data)
str(my_spdf@data)

# We want to make a map with districts -- ADMIN_LEVE = 5
my_spdf <- my_spdf[my_spdf$ADMIN_LEVE == "5",]


# Before merging two datasets, we should make sure they are of the same length
count(my_spdf@data)
count(Lithuania)

# Since we have rather complicated situation when one dataset has one type of naming and another one has another type of
# naming, we should think of a solution to solve this problem.
Lithuania <- Lithuania %>%
  mutate(rajonas = sort(my_spdf@data$NAME))


# Merging two datasets
Lithuania <- merge(Lithuania, vakcinacija, by = "Category")


# Filter some additional information
additional_info <- covid19_statistika_dashboards %>%
  filter(date==max(date))
additional_info <- additional_info[,c("municipality_name", "population", "incidence", "cumulative_totals", "active_de_jure",
                                      "active_sttstcl", "dead_cases", "recovered_de_jure", "recovered_sttstcl", "map_colors")]

Lithuania <- merge(Lithuania, additional_info, by.x = "Category", by.y = "municipality_name")
Lithuania$proc <- Lithuania$pasiskiepije/Lithuania$population*100
Lithuania$proc <- format(round(Lithuania$proc, 2), nsmall = 2)

library(raster) # needed for creating shapefile

# merge on common variable, here called 'key'
m <- merge(my_spdf, Lithuania, by.x = "NAME", by.y = "rajonas")

# After merging datasets we should save the final .shp as shapefile again
shapefile(m, "shapes//output.shp", overwrite = T)


# Uploading saved shape file back to our working directory
LT <- spTransform(m, CRS("+init=epsg:4326"))


#Creating data frame of Lithuania's coordinates so that our map would know where is the starting view point
country <- 'Lithuania'
lon <- 25
lat <- 55.272843
lt_loc <- data.frame(country,lon,lat)


#----------------------------------------------------------------------------------------------------
