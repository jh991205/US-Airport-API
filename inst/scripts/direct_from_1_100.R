library("maps")
library("sp")
usa_poly <- map("usa")
usa_poly$x <- c(NA,usa_poly$x,NA)
usa_poly$y <- c(NA,usa_poly$y,NA)
nai <- which( is.na( usa_poly$x ) )
# define a grid of longitude and latitude points
n1 <- 180
n2 <- 90
lo <- seq(usa_poly$range[1], usa_poly$range[2], length.out = n1)
la <- seq(usa_poly$range[3], usa_poly$range[4], length.out = n2)
lonlat_grid <- as.matrix( expand.grid( lo, la ) )
# figure out which points are inside USA
in_usa <- rep(FALSE, nrow(lonlat_grid))
for(j in 1:(length(nai)-1)){
  in_this <- sp::point.in.polygon( lonlat_grid[,1],
                                   lonlat_grid[,2],
                                   usa_poly$x[ (nai[j]+1):(nai[j+1]-1) ], usa_poly$y[ (nai[j]+1):(nai[j+1]-1) ]
  )
  in_usa <- in_usa | in_this
}



set.seed(123)
# create original direct_from object
direct_from_1 <- vector("list", length(airport_data$ID))
air2019 <- read.csv("../CSV/airline_2019-07-01.csv", header=T)
names(direct_from_1) <- airport_data$ID
for (i in names(direct_from_1)){
  direct_from_1[[i]] <- unique(c(direct_from_1[[i]], air2019[which(air2019$Origin==i),"Dest"]))
  for (j in direct_from_1[[i]]){
    if (!i %in% direct_from_1[[j]]){
      direct_from_1[[j]] <- unique(c(direct_from_1[[j]], i))
    }
  }
}
#remove all the duplicate direct connections of airports that are within 100mi
#count for the amount of connections that are removed
count = 0
for (i in names(direct_from_1)) {
  airport_i <- c(airport_data_num[airport_data_num$ID==i, 2:3][[1]],
                 airport_data_num[airport_data_num$ID==i, 2:3][[2]])
  for (j in names(direct_from_1)) {
    if(i == j) {
      next
    } else {
      airport_j <- c(airport_data_num[airport_data_num$ID==j, 2:3][[1]],
                     airport_data_num[airport_data_num$ID==j, 2:3][[2]])
      distance <- compute_gcd2(airport_i,airport_j)
      if(distance < 100) {
        intersection <- intersect(direct_from_1[[i]],direct_from_1[[j]])
        count = count + length(intersection)
        if(length(intersection) > 0) {
          for(k in 1:length(intersection)) {
            rm = intersection[k]
            direct_from_1[[i]] = direct_from_1[[i]][direct_from_1[[i]] != rm]
            direct_from_1[[rm]] = direct_from_1[[rm]][direct_from_1[[rm]] != i]
          }
        }
      }
    }
  }
}
#list of airports that are in area that had a low proportion covered from direct_from object analysis
areas_not_covered <- c("PDX","BZN","BIS","FSD","CID","ELP","EKO","SLN","VEL","GTR")
#add connections to random airports to the above list
#The amount of connection that is added is equivalent to count from the previous part
random300 <- sample(direct_from_1, 300)
total_added <- count
for(i in names(random300)) {
  original_length <- length(direct_from_1[[i]])
  new_lst <- sort(unique(append(direct_from_1[[i]],areas_not_covered)))
  new_length <- length(new_lst)
  num_added <- new_length - original_length
  if(num_added > total_added) {
    new_lst <- sort(unique(append(direct_from_1[[i]],areas_not_covered[1:total_added])))
    direct_from_1[[i]] = new_lst
    for(j in new_lst) {
      direct_from_1[[j]] = sort(unique(append(direct_from_1[[j]],i)))
    }
    break
  }
  total_added <- total_added - num_added
  direct_from_1[[i]] = new_lst
  for(j in new_lst) {
    direct_from_1[[j]] = sort(unique(append(direct_from_1[[j]],i)))
  }
}

# subset to the points in USA
lonlat_usa <- as.matrix( lonlat_grid[ in_usa, ] )
data = as.data.frame(lonlat_usa)
data$prop <- 0
acc = 0
for(i in 1 : nrow(data)) {
  points_reachable_from_1_100<- points_reachable_from(c(lonlat_usa[,1][i],lonlat_usa[,2][i]), x = 100, direct_from_1)
  prop = nrow(points_reachable_from_1_100)/nrow(lonlat_usa)
  data[,3][i] = prop
  acc  = acc + prop
}
saveRDS(data, file = "direct_from_1_100.rds")
