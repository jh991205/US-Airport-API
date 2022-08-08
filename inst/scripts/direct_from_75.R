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
# subset to the points in USA
lonlat_usa <- as.matrix( lonlat_grid[ in_usa, ] )
data = as.data.frame(lonlat_usa)

data$prop <- 0
acc = 0
for(i in 1 : nrow(data)) {
  points_reachable_from_75<- points_reachable_from(c(lonlat_usa[,1][i],lonlat_usa[,2][i]), x = 75, direct_from)
  prop = nrow(points_reachable_from_75)/nrow(lonlat_usa)
  data[,3][i] = prop
  acc  = acc + prop
}
saveRDS(data, file = "original_data_75.rds")
