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

direct_from_2 <- vector("list", length(airport_data$ID))
names(direct_from_2) <- airport_data$ID
for (i in names(direct_from_2)){
  direct_from_2[[i]] <- unique(c(direct_from_2[[i]], air2019[which(air2019$Origin==i),"Dest"]))
  for (j in direct_from_2[[i]]){
    if (!i %in% direct_from_2[[j]]){
      direct_from_2[[j]] <- unique(c(direct_from_2[[j]], i))
    }
  }
}
n_iter_4b <- 10
for (iter in 1:n_iter_4b){

  # randomly sample some airports to make changes
  n_select <- 30
  chosen_port <- sample(names(direct_from_2), n_select, replace=F)
  for (i in chosen_port){

    # randomly add some random number of connections for each airport
    could_add <- names(direct_from_2)[!names(direct_from_2) %in% direct_from_2[[i]]]
    n_add <- sample(seq(2,20), 1) # add between 2-20 random connections
    to_add <- sample(could_add, n_add, replace=F)
    direct_from_2[[i]] <- c(direct_from_2[[i]], to_add)
    for (k in to_add){
      direct_from_2[[k]] <- c(direct_from_2[[k]], i)
    } # maintain symmetry

    # randomly remove the same number of connections for each airport
    to_rm <- sample(direct_from_2[[i]], n_add, replace=F)
    direct_from_2[[i]] <- direct_from_2[[i]][!direct_from_2[[i]] %in% to_rm]
    for (j in to_rm){
      direct_from_2[[j]] <- direct_from_2[[j]][direct_from_2[[j]] != i]
    } # maintain symmetry
  }
}

# subset to the points in USA
lonlat_usa <- as.matrix( lonlat_grid[ in_usa, ] )
data = as.data.frame(lonlat_usa)
data$prop <- 0
acc = 0
for(i in 1 : nrow(data)) {
  points_reachable_from_2_100<- points_reachable_from(c(lonlat_usa[,1][i],lonlat_usa[,2][i]), x = 100, direct_from_2)
  prop = nrow(points_reachable_from_2_100)/nrow(lonlat_usa)
  data[,3][i] = prop
  acc  = acc + prop
}
saveRDS(data, file = "direct_from_2_100.rds")
