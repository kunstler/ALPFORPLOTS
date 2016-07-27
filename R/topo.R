### Function to read the elevation slope and exposition from French MNT

GetTopo <- function(df_p){

 require(raster)
 require(rgdal)
 path_dem <- "/run/user/1001/gvfs/smb-share:server=syno,share=ign/BD_ALTI/25m/France_Entiere"

 dem <- raster(file.path(path_dem, "france_mnt_2154.tif"))
 # in L93
 x_min <- min(df_p$x_lamb93, na.rm = TRUE) - 4000
 x_max <- max(df_p$x_lamb93, na.rm = TRUE) + 4000
 y_min <- min(df_p$y_lamb93, na.rm = TRUE) - 4000
 y_max <- max(df_p$y_lamb93, na.rm = TRUE) + 4000
 e <- extent(x_min, x_max, y_min, y_max)
 dem <- crop(dem, e)
 slope <- terrain(dem, opt='slope', unit='degrees')
 aspect <- terrain(dem, opt='aspect', unit='degrees')
 elev <- raster::extract(dem, df_p)
 slope_v <- raster::extract(slope, df_p)
 aspect_v <- raster::extract(aspect, df_p)
 df_p$elevation <- elev
 df_p$slope <- slope_v
 df_p$aspect <- aspect_v

return(df_p)
}

