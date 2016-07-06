
#Returns the index of the nearest plot(x2,y2) to each of the target plots (x1,y1)
NearestPlot <- function(x1,y1,x2,y2) {

  res = numeric(length(x1))

  for (i in 1:length(x1)) {
    dists = sqrt((x1[i] - x2)^2 + (y1[i] - y2)^2)
    res[i] = which.min(dists)
    }
  return(res)

} #NearestPlot


#looks up MAT and MAP for given lat/lon values
GetClimate <-function(df) {
  require(raster)
  require(dplyr)
  #create raster grid for tiles
  xNum<-0:11
  yNum<-0:4

  xLon<-seq(from=-165,to=165,by=30)
  yLat<-seq(from=75,to=-45,by=-30)

  NumMat<-outer(yNum,xNum,paste,sep="")
  NumLookup<-as.vector(NumMat)

  NumRast <- raster(matrix(1:60,nrow=5),xmn=-180,xmx=180,ymn=-60,ymx=90)

  LonLatLookup<-expand.grid(Lat=yLat,Lon=xLon)
  row.names(LonLatLookup) <- NumMat

  #get the set of tiles corresponding to the lons/lats arguments
  tiles <- raster::extract(NumRast,cbind(df$long,df$lat))
  unique.tiles <- unique(tiles)

  plot.mat <- numeric(length(tiles))
  plot.map <- numeric(length(tiles))
  #download each of the tiles
  for (i.tile in unique.tiles) {
    temp.tile <- getData('worldclim',
                         var = 'bio',
                         res = 0.5,
                         lon = LonLatLookup[i.tile,"Lon"],
                         lat = LonLatLookup[i.tile,"Lat"])

    plot.mat[tiles==i.tile] <- raster::extract(temp.tile,
                                       cbind(df$long[tiles==i.tile],
                                             df$lat[tiles==i.tile]),
                                       layer=1,nl=1)/10
                                  #retrieve MAT, divide by 10 for deg C
    plot.map[tiles==i.tile] <- raster::extract(temp.tile,
                                       cbind(df$long[tiles==i.tile],
                                             df$lat[tiles==i.tile]),
                                       layer=12,nl=1) #retrieve MAP
    nas <- which(tiles==i.tile & is.na(plot.mat))

    #assign climate of nearest plot to all nas
    if (length(nas)>0) {
      good <- which(tiles==i.tile & !is.na(plot.mat))

      near <- NearestPlot(df$long[nas],df$lat[nas],df$long[good],df$lat[good])

      plot.mat[nas] <- plot.mat[good[near]]
      plot.map[nas] <- plot.map[good[near]]
    }
  }
  df$MAT <-  plot.mat
  df$MAP <-  plot.map
  return(list(MAT=df$MAT,MAP=df$MAP))

} #GetClimate

