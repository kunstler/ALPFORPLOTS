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
test <- over(df, brgm[, "LITHOLOGIE"])
test$LITHOLOGIE <-  as.factor(as.character(test$LITHOLOGIE))
df_conv <- data.frame(
     old= c("argile", "blocs", "calcaire", "calcaire argileux (80%<CO3<90%)",
            "calcaire bioclastique", "calcaire spathique (ou cristallin)",
            "calcaire sublithographique", "conglomérat", "conglomérat meuble à clastes jointifs",
            "grauwacke", "grès", "marne (33%<CO3<66%)", "micaschiste", "moraine",
            "orthogneiss", "schiste carboné"),
     new= c('clay', 'boulder', 'limestone',  'limestone',  'limestone',
            'limestone',  'limestone',  'conglomerate',
            'conglomerate', 'greywacke', 'sandstone', 'marl', 'schist',
            'moraine', 'gneiss', 'schist'))

vec_level <- levels(test$LITHOLOGIE)
levels(test$LITHOLOGIE)[match(df_conv$old,vec_level)] <- df_conv$new

return(test$LITHOLOGIE)
}


