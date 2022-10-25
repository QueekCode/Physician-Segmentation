library(lubridate)
library(tidyverse)

df = read.csv("data/Reformatted.csv")

# Formatting time period
df$Time.Period = as.character(df$Time.Period)
for (i in 1:nrow(df)){
  if (nchar(df$Time.Period[i]) == 7){
    df$Time.Period[i] = paste("0", df$Time.Period[i], sep = "")
  }
}
df$Time.Period = paste(paste(substring(df$Time.Period, 0, 4), "20", sep = ""), 
                       substring(df$Time.Period, 5, 6), sep = "")
df$Time.Period = as.POSIXct(df$Time.Period, format = "%m%d%Y")

# Coordinates
cities = read.csv("data/cities.csv")
cities = cities[1:50,1:3]
df = left_join(df, cities, by = "City")




df$Language = ifelse(df$Language == "E", "English", "French")

df$Office.Type = ifelse(df$Office.Type == "O",
                        "Office",
                        ifelse(df$Office.Type == "C",
                               "Clinic",
                               ifelse(df$Office.Type == "XO",
                                      "Hospital",
                                      "Other")
                               )
                        )

df$Interest[df$Interest == "NUL"] = "No Primary Interest"
df$Interest[df$Interest == "ANE"] = "Anesthesia"
df$Interest[df$Interest == "PHC"] = "Pharmacology & Toxicology"
df$Interest[df$Interest == "MMI"] = "Microbiology"
df$Interest[df$Interest == "PSY"] = "Psychiatry"
df$Interest[df$Interest == "CAR"] = "Cardiology"
df$Interest[df$Interest == "IM"] = "Internal Medicine"
df$Interest[df$Interest == "ONC"] = "Oncology"
df$Interest[df$Interest == "ID"] = "Infectious Diseases"
df$Interest[df$Interest == "GEN"] = "Genetics"
df$Interest[df$Interest == "ORS"] = "Orthopedic Surgery"
df$Interest[df$Interest == "NME"] = "Nuclear Medicine"
df$Interest[df$Interest == "PEG"] = "Pediatric Gastroenterology"
df$Interest[df$Interest == "CIC"] = "Critical Care Medicine"
df$Interest[df$Interest == "ENM"] = "Endocrinology & Metabolism"
df$Interest[df$Interest == "CIN"] = "Clinician Investigator"
df$Interest[df$Interest == "HEP"] = "Hepatology"
df$Interest[df$Interest == "HEM"] = "Hematology"
df$Interest[df$Interest == "NUT"] = "Nutrition"
df$Interest[df$Interest == "FM"] = "Family Medicine"
df$Interest[df$Interest == "GSU"] = "General Surgery"
df$Interest[df$Interest == "EMG"] = "Emergency Medicine"
df$Interest[df$Interest == "PAL"] = "Palliative Care"

par(mfrow=c(1,1), mar=c(13.1, 4.1, 4.1, 2.1))
barplot(table(df$Interest), las=2)
nrow(df[df$Interest == "No Primary Interest",])
nrow(df[df$Interest != "No Primary Interest",])
# how can 5/6 doctors have no primary interest??? 
barplot(table(df$Interest[df$Interest!="No Primary Interest"]), las=2)
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))

barplot(table(df$Office.Type))

# Quick Overview
summary(df)
unique(df$Prim.Spec)
unique(df$Interest)
unique(df$Office.Type)
unique(df$Language)
unique(df$Status)
unique(df$Product.Group)
boxplot(df$Average.Rx.Volume)


## Infliximab - remicade, humera - human recombinant TNFa antibody - 1st line of response/golden standard - used for crohns - ulcerative colitis
## Golimumab - used if non-responsive - human monoclonal TNFa antibody - 2nd/3rd line - not approved for crohns - ulcerative colitis
## USTEKINUMAB - 1st/2nd line
## Vedolizumab -  
## Adalimumab - same MOA as Infliximab



drugs = aggregate(df[,c("Average.Rx.Volume")], 
                  by = list(df$Product.Group), FUN=sum)




# nightmare nightmare nightmare

library(raster)
library(ggplot2)

states    <- c("Minnesota", "Michigan", "Pennsylvania", "New York")
provinces <- c("Ontario", "Manitoba", "Québec")
us <- raster::getData("GADM",country="USA",level=1)
canada <- raster::getData("GADM",country="CAN",level=1)
us.states <- us[us$NAME_1 %in% states,]
ca.provinces <- canada[canada$NAME_1 %in% provinces,]


  
ggplot(us.states,aes(x=long,y=lat,group=group))+
  geom_path()+
  geom_path(data=ca.provinces)+
  coord_map()+
  xlim(-96, -73)+
  ylim(41, 57)

mappr = aggregate(df[,c("Average.Rx.Volume")], 
          by = list(df$Product.Group, df$Longitude, df$Latitude), FUN=sum)

colnames(mappr) = c("Drug", "Latitude", "Longitude", "Rx.Volume")

ggplot(mappr[mappr$Drug == "INFLIXIMAB",], aes(x=Longitude, 
               y = Latitude))+
  geom_point(col = "red", 
             aes(size = Rx.Volume))+
  geom_path(data=ca.provinces, aes(x = long, y = lat, group = group))+
  geom_path(data=us.states, aes(x = long, y = lat, group = group))+
  coord_map()+
  aes(group = NULL)+
  xlim(-96, -73)+
  ylim(41, 57)+
  labs(
    title = "Infliximab",
    size = "Total Average Rx Volume"
  )

ggplot(mappr[mappr$Drug == "USTEKINUMAB",], aes(x=Longitude, 
                                               y = Latitude))+
  geom_point(col = "blue", 
             aes(size = Rx.Volume))+
  geom_path(data=ca.provinces, aes(x = long, y = lat, group = group))+
  geom_path(data=us.states, aes(x = long, y = lat, group = group))+
  coord_map()+
  aes(group = NULL)+
  xlim(-96, -73)+
  ylim(41, 57)+
  labs(
    title = "Ustekinumab",
    size = "Total Average Rx Volume"
  )

ggplot(mappr[mappr$Drug == "VEDOLIZUMAB",], aes(x=Longitude, 
                                                y = Latitude))+
  geom_point(col = "green", 
             aes(size = Rx.Volume))+
  geom_path(data=ca.provinces, aes(x = long, y = lat, group = group))+
  geom_path(data=us.states, aes(x = long, y = lat, group = group))+
  coord_map()+
  aes(group = NULL)+
  xlim(-96, -73)+
  ylim(41, 57)+
  labs(
    title = "Vedolizumab",
    size = "Total Average Rx Volume"
  )


ggplot(mappr[mappr$Drug == "GOLIMUMAB",], aes(x=Longitude, 
                                                y = Latitude))+
  geom_point(col = "orange", 
             aes(size = Rx.Volume))+
  geom_path(data=ca.provinces, aes(x = long, y = lat, group = group))+
  geom_path(data=us.states, aes(x = long, y = lat, group = group))+
  coord_map()+
  aes(group = NULL)+
  xlim(-96, -73)+
  ylim(41, 57)+
  labs(
    title = "Golimumab",
    size = "Total Average Rx Volume"
  )


ggplot(mappr[mappr$Drug == "ADALIMUMAB",], aes(x=Longitude, 
                                              y = Latitude))+
  geom_point(col = "magenta", 
             aes(size = Rx.Volume))+
  geom_path(data=ca.provinces, aes(x = long, y = lat, group = group))+
  geom_path(data=us.states, aes(x = long, y = lat, group = group))+
  coord_map()+
  aes(group = NULL)+
  xlim(-96, -73)+
  ylim(41, 57)+
  labs(
    title = "Adalimumab",
    size = "Total Average Rx Volume"
  )

