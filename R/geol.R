## function to get geol for x y coordinates in lamber93
read_brgm_dep <- function(d, path){
    print(d)
 name <- sub('.shp',
             '',
             grep('s_fgeol_L93.shp',
                  list.files(file.path(path, d)),
                  ignore.case = TRUE,
                  value = TRUE))
 test <- readOGR(file.path(path, d),
                name,
                encoding = 'LATIN1')
 test <- spChFIDs(test, paste0(d, '_', sapply(slot(test, "polygons"), slot, "ID")))
 return(test)
}


GetGeol <- function(df){

require(maptools)
require(rgdal)
require(taRifx.geo)
## path_brgm <- "/run/user/1001/gvfs/smb-share:server=syno,share=ign/BRGM/Cartes_harmonisees/L93/ArcView/"
## dep <- c('74', '73', '38', '26', '05')
## list_brgm <- lapply(dep, read_brgm_dep, path = path_brgm)
## names_keep <- names(list_brgm[[1]])
## list_brgm_clean <- lapply(list_brgm,
##                           function(spdf, keep) spdf[, names(spdf) %in% keep],
##                           keep = names_keep)
## bb <- list_brgm_clean[[1]]
## brgm <- taRifx.geo::rbind.SpatialPolygonsDataFrame(list_brgm_clean[[1]],
##                                                    list_brgm_clean[[2]],
##                                                    fix.duplicated.IDs = TRUE)
## brgm <- rbind(brgm, list_brgm_clean[[3]], fix.duplicated.IDs = TRUE)
## brgm <- rbind(brgm, list_brgm_clean[[4]], fix.duplicated.IDs = TRUE)
## brgm <- rbind(brgm, list_brgm_clean[[5]], fix.duplicated.IDs = TRUE)
## writeOGR(brgm, ".", "GIS_data/brgm", driver="ESRI Shapefile")

brgm <- readOGR('GIS_data',
                'brgm')
##
proj4string(brgm) <- CRS("+init=epsg:2154")
df_l93<- spTransform(df, CRS("+init=epsg:2154"))  # spTransform makes the projection

test <- over(df_l93, brgm[, "LITHOLOGIE"])
test$LITHOLOGIE <-  as.factor(as.character(test$LITHOLOGIE))
levels(test$LITHOLOGIE) <- c('boulder', 'limestone',  'limestone',  'limestone',
                             'limestone',  'limestone',  'limestone', 'conglomerate',
                             'conglomerate', 'sandstone', 'marl', 'schist',
                             'moraine', 'gneiss', 'schist')

return(test$LITHOLOGIE)
}

GetElev <- function(df){
require(raster)
mnt <- raster("/run/user/1001/gvfs/smb-share:server=syno,share=ign/BD_ALTI/500m/mnt_500m_rgf/w001001.adf")
elev <- extract(mnt, df)
df$elevation[is.na(df$elevation)] <- elev[is.na(df$elevation)]
return(df)
}

