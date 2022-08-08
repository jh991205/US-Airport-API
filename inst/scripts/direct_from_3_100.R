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

# whole matrix
airport_data_mat <- cbind(airport_data$longitude, airport_data$latitude)
rownames(airport_data_mat) <- airport_data$ID
colnames(airport_data_mat) <- c("longitude", "latitude")
full_port_dist <- distfun(airport_data_mat, airport_data_mat)

# indices of potentially redundant airports (airports located within 100 miles from each other), calculated from the full distance matrix
dup_ind <- which(full_port_dist<100 & full_port_dist!=0, arr.ind = T)

direct_from_3 <- vector("list", length(airport_data$ID))
names(direct_from_3) <- airport_data$ID
for (i in names(direct_from_3)){
  direct_from_3[[i]] <- unique(c(direct_from_3[[i]], air2019[which(air2019$Origin==i),"Dest"]))
  for (j in direct_from_3[[i]]){
    if (!i %in% direct_from_3[[j]]){
      direct_from_3[[j]] <- unique(c(direct_from_3[[j]], i))
    }
  }
}

num_iter_4c <- 10
# check for redundant direct flies in the direct_from_1 object
# randomly remove connections to potentially redundant airports (but keep at least 1)
for (iter in 1:num_iter_4c){
  removed_4c <- 0
  for (i in names(direct_from_3)){ # loop through all airports
    intersection <- c()
    for (j in 1:nrow(dup_ind)){ # loop through indices for potentially redundant connections
      col <- dup_ind[j,2]
      if (rownames(dup_ind)[j] %in% direct_from_3[[i]] &
          i == names(direct_from_3)[col]){
        intersection <- c(intersection, rownames(dup_ind)[j])
      }
    }

    if (length(intersection)>=2){
      if (length(intersection)>=3){
        to_rm <- sample(intersection, 2, replace=F) # randomly sample 2 redundant airports to remove
        direct_from_3[[i]] <- direct_from_3[[i]][!direct_from_3[[i]] %in% to_rm]
        for (removed in to_rm){ # keep symmetry
          direct_from_3[[removed]] <- direct_from_3[[removed]][direct_from_3[[removed]]!=i]
        }
        removed_4c <- removed_4c + 2
      }else{
        to_rm <- sample(intersection, 1, replace=F) # randomly sample 1 redundant airport to remove
        direct_from_3[[i]] <- direct_from_3[[i]][!direct_from_3[[i]] %in% to_rm]
        direct_from_3[[to_rm]] <- direct_from_3[[to_rm]][direct_from_3[[to_rm]]!=i]
        removed_4c <- removed_4c + 1
      }
    }
  }

  # randomly add back 11 connections for removed redundant airports
  # if no redundant airports left, randomly select 20 airports and add or remove 1 or 2 connections
  if (removed_4c > 0){
    to_add <- sample(names(direct_from_3), removed_4c, replace=F) # 11 airports to add back
    add_to <- sample(names(direct_from_3)[!names(direct_from_3) %in% to_add],
                     removed_4c, replace=F) # 11 airports to add back to
    # add connections by 1-to-1 correspondance of the two vectors sampled above
    counter_4c <- 0
    for (i in to_add){
      counter_4c <- counter_4c + 1
      direct_from_3[[i]] <- c(direct_from_3[[i]], add_to[counter_4c])
      direct_from_3[[add_to[counter_4c]]] <-
        c(direct_from_3[[add_to[counter_4c]]], i) # keep symmetry
    }
  }
  else{
    #cat("All redundant airports removed", "\n")
    n <- 60
    to_change <- sample(names(direct_from_3), n, replace=F) # n airports to add or remove connections
    delta <- 0 # track the difference between added and removed connection; want it to be 0
    counter2 <- 0
    for (port in to_change){
      counter2 <- counter2 + 1
      if (counter2 %% 2 == 0){ # remove 1 if even index
        delta <- delta - 1
        change_to <- sample(names(direct_from_3)[!names(direct_from_3) %in% to_change], 1, replace=F)
        direct_from_3[[port]] <- direct_from_3[[port]][!direct_from_3[[port]] %in% change_to]
        for (port1 in change_to){
          direct_from_3[[port1]] <- direct_from_3[[port1]][direct_from_3[[port1]]!=port] # keep symmetry
        }

      }
      else{ # add 1 if odd index
        delta <- delta + 1
        change_to <- sample(names(direct_from_3)[!names(direct_from_3) %in% to_change], 1, replace=F)
        direct_from_3[[port]] <- c(direct_from_3[[port]], change_to)
        for (port1 in change_to){
          direct_from_3[[port1]] <- c(direct_from_3[[port1]], port)
        }
      }
    }
  }
}

# subset to the points in USA
lonlat_usa <- as.matrix( lonlat_grid[ in_usa, ] )
data = as.data.frame(lonlat_usa)
data$prop <- 0
acc = 0
for(i in 1 : nrow(data)) {
  points_reachable_from_3_100<- points_reachable_from(c(lonlat_usa[,1][i],lonlat_usa[,2][i]), x = 100, direct_from_3)
  prop = nrow(points_reachable_from_3_100)/nrow(lonlat_usa)
  data[,3][i] = prop
  acc  = acc + prop
}
saveRDS(data, file = "direct_from_3_100.rds")
