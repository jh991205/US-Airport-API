direct_from <- vector("list", length(airport_data$ID))
names(direct_from) <- airport_data$ID
# loop through all airports and check for direct flights
for (i in names(direct_from)){
  #cat("origin port", i, "\n")
  direct_from[[i]] <- unique(c(direct_from[[i]], air2019[which(air2019$Origin==i),"Dest"]))
  for (j in direct_from[[i]]){
    if (!i %in% direct_from[[j]]){
      # add the airport to the list for its destination if not already present
      direct_from[[j]] <- unique(c(direct_from[[j]], i))
      #cat("added to ", j, " port ", direct_from[[j]], "\n")
    }
  }
}
setwd("/Users/apple/Desktop/AirportAPI/R")
save(direct_from, file = "../data/direct_from.rdata")
