setwd("/Users/apple/Desktop/AirportAPI/R")
all_air <- read.csv("../CSV/all-airport-data.csv", header=T)
air2019 <- read.csv("../CSV/airline_2019-07-01.csv", header=T)
# find unique airport codes in the 2019 data set
ids <- sort(unique(air2019$Origin))
# find airport codes that are in both data sets and missing codes
intersected_ids <- ids[ids %in% all_air$Loc.Id]
missing_ids <- ids[!ids %in% all_air$Loc.Id]
# create the latitude and longitude columns using the ARP latitudes and ARP longituds in all_air
latitudes <- rep(-1, length(ids))
longitudes <- rep(-1, length(ids))
for (i in 1:length(ids)){
  if (ids[i] %in% intersected_ids){
    latitudes[i] <- all_air[all_air$Loc.Id == ids[i], ]$ARP.Latitude
  }
}
for (i in 1:length(ids)){
  if (ids[i] %in% intersected_ids){
    longitudes[i] <- all_air[all_air$Loc.Id == ids[i], ]$ARP.Longitude
  }
}
# create the airport_data dataframe and add missing values later
airport_data <- data.frame("ID"=ids,
                           "longitude"=longitudes,
                           "latitude"=latitudes)
## find the coordinates of the missing airport IDs on airnav.com
missing_ids # print out the missing IDs
airport_data[airport_data$ID=="AZA",2:3] <- c("111-39-19.6530W", "033-18-28.1660N")
airport_data[airport_data$ID=="BKG",2:3] <- c("093-12-01.9600W", "036-31-55.4970N")
airport_data[airport_data$ID=="FCA",2:3] <- c("114-15-21.6000W", "048-18-37.8000N")
airport_data[airport_data$ID=="HHH",2:3] <- c("080-41-50.6430W", "032-13-28.1820N")
airport_data[airport_data$ID=="ISN",2:3] <- c("103-45-01.9960W", "048-15-35.2260N")
airport_data[airport_data$ID=="MQT",2:3] <- c("087-23-46.9380W", "046-20-56.9700N")
airport_data[airport_data$ID=="SCE",2:3] <- c("077-50-51.3000W", "040-51-00.0000N")
airport_data[airport_data$ID=="SPN",2:3] <- c("145-43-47.9510E", "015-07-12.8950N")
airport_data[airport_data$ID=="USA",2:3] <- c("080-42-32.8770W", "035-23-15.9730N")
airport_data[airport_data$ID=="YUM",2:3] <- c("114-36-21.5540W", "032-39-23.6660N")
save(airport_data, file = "../data/airport_data.rdata")
