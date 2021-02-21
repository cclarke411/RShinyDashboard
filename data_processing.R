library(tidyverse)
library(lubridate)
library(dplyr)

scms_df <- read_csv("data/scms_df.csv")

countries_sf <- countries110 %>%
  st_as_sf()
options(warn=-1)

colnames(scms_df) <- tolower(make.names(colnames(scms_df)))

countries_df<- read_csv("data/countries.csv") %>%
  filter(name %in% scms_df$country)

countries_df <- countries_df %>%
  rename(country = name) %>%
  distinct(country,.keep_all = TRUE)


scms_df<- scms_df %>%
  rename(line.item.insurance.usd = line.item.insurance..usd.,
         po.so = po...so..,
         pq = pq..,
         asn.dn = asn.dn..,
         weight.kilograms = weight..kilograms.,
         freight.cost.usd = freight.cost..usd.)

scms_df <- scms_df %>%
  rename(country = country)


scms_df <- scms_df %>%
  mutate(scheduled.date = dmy(scheduled.delivery.date)) %>%
  mutate(year = year(scheduled.date))

scms_df<-scms_df %>%
  mutate(new_weight = str_extract(scms_df$weight.kilograms,"(?<=\\:)(.*?)(?=\\))")) %>%
  as_tibble() %>%
  type_convert()

scms_df<- scms_df %>%
  mutate(new_id = case_when(
    is.na(new_weight) ~ id,
    is.numeric(new_weight) ~ new_weight
  )
  )

scms_df <- scms_df %>%
  rowwise %>%
  mutate(weight.kilograms = .[.$id == new_id,"weight.kilograms"][[1]])

scms_df$weight.kilograms <- scms_df$weight.kilograms %>%
  as.numeric() 

scms_df <- scms_df %>%
  mutate(
    weight.kilograms = case_when(
      is.na(weight.kilograms)~ 0,
      !is.na(weight.kilograms) ~ weight.kilograms
    )
  ) 

scms_df <- scms_df %>%
  group_by(country,product.group) %>%
  mutate(weight.group = mean(weight.kilograms)) %>%
  mutate(line.item.mean = mean(line.item.quantity)) %>%
  mutate(prod.weight.mean = weight.group/line.item.mean) %>%
  mutate(weight.kilograms.final = case_when(
    weight.kilograms != 0 ~ weight.kilograms,
    weight.kilograms == 0 ~ floor(line.item.mean*prod.weight.mean)
  ))

colnames(scms_df) <- str_replace_all(names(scms_df),"[/./]","_")


scms_countries_df<- right_join(countries_df,scms_df,by="country")
