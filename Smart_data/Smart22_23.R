###### SMART Patrol data - 2022-2023 #########
library(tidyverse)

### reading in 2022 data

## Wildlife Data

#animal findings
af <- read.csv(file = "Smart_data//Smart2022/1. Data Patroli 2022 (animal findings).csv", header = TRUE)
af <- af[-c(32:51),]
#animal marks
am <- read.csv(file = "Smart_data//Smart2022/2. Data Patroli 2022 (Animal marks).csv", header = TRUE)
#animals (dead)
ad <- read.csv(file = "Smart_data//Smart2022/3. Data Patroli 2022 (Dead animal).csv", header = TRUE)

## Human Activity

#illegal logging
il <- read.csv(file = "Smart_data//Smart2022/1. Data Patroli 2022 (illegal logging).csv", header = TRUE)
#land and forest fires
ff <- read.csv(file = "Smart_data//Smart2022/2. Data Patroli 2022 (land and forest fires).csv", header = TRUE)
#poaching
po <- read.csv(file = "Smart_data//Smart2022/3. Data Patroli 2022 (Poaching).csv", header = TRUE)
#work tools and transportation
wt <- read.csv(file = "Smart_data//Smart2022/4. Data Patroli 2022 (Work tool and transportation).csv", header = TRUE)
#land encroachment
le <- read.csv(file = "Smart_data//Smart2022/5. Data Patroli 2022 (Land encroachment).csv", header = TRUE)


### Exploring Wildlife Data
##animal findings
head(af)

#most animal observations are later in the year, in Oct & Nov
hist(dmy(af$Patrol.Start.Date), breaks = 10)

#number of wildlife observations by HD. Most are in Penjalaan and Panjang
table(af$Station)

#the most common observations are orangutans (4) and kelasi (3)
table(af$Jenis.satwa)

#table of observations
af_table <- af %>%
  filter(str_trim(Jenis.satwa) != "") %>%
  group_by(Station, Jenis.satwa) %>%
  count() %>%
  pivot_wider(names_from = Station, values_from = n, values_fill = 0)

af_table

#unique animal observations by site - Penjalaan (7) and Rantau Panjang (7) have the most, out of 20 total
colSums(af_table != 0)

##animal markings
head(am)

#distribution of findings across the year
hist(dmy(am$Patrol.Start.Date), breaks = 10)

#number of markings by HD. Most are in Pemangkat and Penjalaan
table(am$Station)

#the most common markings are OH (466), boar (95), and sun bear (51)
table(am$Jenis.satwa)

#observation table
am_table <- am %>%
  filter(str_trim(Jenis.satwa) != "") %>%
  group_by(Station, Jenis.satwa) %>%
  count() %>%
  pivot_wider(names_from = Station, values_from = n, values_fill = 0)

as.data.frame(am_table)

#unique observations by site - Pemangkat (17) and Padu Banjar (14) have the most, out of 30 total. 
colSums(am_table != 0)


#################################
### Exploring Human Activity Data

## illegal logging
head(il)

#distribution of logging observations across the year - most obs are in the summer months
hist(dmy(il$Patrol.Start.Date), breaks = 10)

#number of logging observations by site - most are at Padu Banjar (26) and Pemangkat (24)
table(il$Station)

#what species are being logged? Mostly Koompassia malaccensis and Tetramerista glabra, both common in construction
table(il$Jenis.tumbuhan)


## land and forest fires - only 4 recordings made, all in Padu Banjar, 3 in Dec and 1 in Apr
head(ff)


## poaching 
head(po)

#distribution of poaching signs across the year
hist(dmy(po$Patrol.Start.Date), breaks = 10)

#number of poaching signs by site - most in Penjalaan (17) and Padu Banjar (14)
table(po$Station)

#which species are targeted by poaching? Most target Sus scrofa (17) but many deer (6) as well
table(po$Jenis.satwa)


## work tool and transportation
head(wt)

#these observations are spread fairly evenly throughout HD sites
table(wt$Station)

#most objects found include bicycles (5) and Pondok (4)
table(wt$Tipe.temuan)


## land encroachment
head(le)

#distribution of LE signs across the year
hist(dmy(le$Patrol.Start.Date), breaks = 10)

#number of LE events recorded by HD site - the vase majority (58/67) recorded in Padu Banjar
table(le$Station)

#what LE types were found? The majority were palm oil plantation (23) 
table(le$Tipe.temuan)

#observation table - palm oil plantations are the most common LE type, found in 3 sites
le %>%
  group_by(Station, Tipe.temuan) %>%
  count() %>%
  pivot_wider(names_from = Station, values_from = n, values_fill = 0)
  