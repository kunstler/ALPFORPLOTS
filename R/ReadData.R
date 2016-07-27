###########################################
# Functions to read plots data ------------
###########################################

library(gdata)

# read plots coordinates TODO UPDATE BY MARC DONE
read_data_plot <- function(path_samba = "/run/user/1001/gvfs/smb-share:server=sdgrp1,share=services/EMGR/Projets/placette_foret/"){
 data_p <- read.xls(file.path(path_samba, 'données_autrestables',
                                 'metadonnees_placette_2015.xlsx'),
                       sheet = "placettes",
                       stringsAsFactors = FALSE)

 names(data_p) <- c('plot_id', 'paper_yn', 'owner_id', 'management', 'year_first_mes', 'N_census',
                       'area', "x_min", "x_max", "y_min", "y_max",
                       'elevation', 'GPS_loc',
                       'x_lamb93', 'y_lamb93')
 return(data_p)
}


 ## path_samba <- "/run/user/1001/gvfs/smb-share:server=sdgrp1,share=services/EMGR/Projets/placette_foret/"

#### functions read data
read_data_sites <- function(site, prefix, path = "/run/user/1001/gvfs/smb-share:server=sdgrp1,share=services/EMGR/Projets/placette_foret/"){
  test_xls <- drop(file.exists(file.path(path, "données_carto&mesures", site,
                                         paste0(prefix,
                                                tolower(site), '.xls'))))
  test_xlsx <- drop(file.exists(file.path(path, "données_carto&mesures", site,
                                          paste0(prefix,
                                                 tolower(site), '.xlsx'))))
  if(test_xls){
       print('xls')
       data_t <- read.xls(file.path(path, "données_carto&mesures", site,
                                    paste0(prefix, site, '.xls')),
                          stringsAsFactors = FALSE)
       names(data_t) <-  tolower(names(data_t))
       return(data_t)
  }else{
      if(test_xlsx){
          print('xlsx')
          data_t <- read.xls(file.path(path, "données_carto&mesures", site,
                                       paste0(prefix, site, '.xlsx')),
                             stringsAsFactors = FALSE)
          names(data_t) <-  tolower(names(data_t))
          return(data_t)
      }else{
          return(NA)
      }
   }
}

read_mesures_all <- function(path_s = "/run/user/1001/gvfs/smb-share:server=sdgrp1,share=services/EMGR/Projets/placette_foret/"){
sites <- list.dirs(path = file.path(path_s, "données_carto&mesures"),
                   full.names = FALSE,
                   recursive = FALSE)
list_mesures <- lapply(sites, read_data_sites, prefix = 'mesures_')
list_test_names <- lapply(list_mesures,
                          function(x, names_vec) all(names(x) == names_vec),
                          names_vec = names(list_mesures[[1]]))
if (!all(unlist(list_test_names))) stop('error not sames names in data')
data_mesures <- do.call(rbind, list_mesures)
return(data_mesures)
#done
}





read_carto_all <- function(path_s = "/run/user/1001/gvfs/smb-share:server=sdgrp1,share=services/EMGR/Projets/placette_foret/"){
sites <- list.dirs(path = file.path(path_s, "données_carto&mesures"),
                   full.names = FALSE,
                   recursive = FALSE)

list_carto <- lapply(sites, read_data_sites, prefix = 'Carto_')
list_test_names <- lapply(list_carto,
                          function(x, names_vec) all(names(x) == names_vec),
                          names_vec = names(list_carto[[1]]))
if (!all(unlist(list_test_names))) stop('error not sames names in data')
data_carto <- do.call(rbind, list_carto)
return(data_carto)
# done
}



# rename data
rename_data_c <- function(df){
names(df) <- c('map_year', 'map_id', 'plot_id', 'stem_id',
                   'quadrat_id', 'code_species',
                   'x', 'y', 'z', 'year_birth')
df$stem_id <- df$map_id
df$map_year<- NULL
df$map_id <-  NULL
return(df)
}


rename_data_m <- function(df){
names(df) <- c('measure_id', 'plot_id', 'year', 'stem_id',
                   'code_status', 'code_diam', 'dbh', 'h_tot',
                   'crown_h1', 'crown_h2', 'crown_h3', 'crown_h4',
                   'crown_r1', 'crown_r2', 'crown_r3', 'crown_r4',
                   'base_crown_h', 'strate')
df$stem_id <- paste0(df$stem_id, df$plot_id)
return(df)
}



## FUNCTIONS TO CLEAN DATA
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

fix_species_code <-  function(df){
 old_name <- c('SOR', '', 'CHE', 'SA spp', 'ALI', 'BOU', 'ALIB', 'SO',
                 'AL spp', 'QU spp', 'IF', 'MER', 'SAP', 'ERP',
               'FRE', 'BETU', 'CHA', 'ORM', 'SOAC')
 new_name <- c('SOAU', 'ND', 'QUSP', 'SASP', 'SOAR', 'BESP', 'SOAR',
               'SOAU', 'ALSP', 'QUSP', 'TABA', 'PRAV', 'ABAL',
               'ACPL', 'FREX', 'BESP', 'CABE', 'ULSP', 'SOAU')
 df$code_species <- trim(df$code_species)
 df$code_species <-  factor(df$code_species)
 names_species <- levels(df$code_species)
 names_species[match(old_name[old_name %in% names_species],
                     names_species)] <- new_name[old_name %in% names_species]
 levels(df$code_species) <-  names_species
 df$code_species <-  as.character(df$code_species)
 return(df)
}


fix_code_status <- function(df){
df$code_status[df$code_status == '0000-'] <- '0000'
df$code_status[df$code_status == '0'] <- '0000'
df$code_status[df$code_status == '9992'] <- '9990'
return(df)
}

get_id_data_dbh_min<-  function(data_m){
data_d_min<- tapply(data_m$dbh, INDEX = data_m$plot_id, min, na.rm = TRUE)
data_dbh_max_tree<- tapply(data_m$dbh,
                           INDEX = data_m$stem_id,
                           max, na.rm = TRUE)
data_dbh_max_tree[data_dbh_max_tree == -Inf] <-  NA
stem_id_remove <- names(data_dbh_max_tree)[data_dbh_max_tree <7.5 &
                                           !is.na(data_dbh_max_tree)]
return(stem_id_remove)
}

remove_tree_below_dbh_map <-  function(data_c, data_m){
vec <- get_id_data_dbh_min(data_m)
return(data_c[!(data_c$stem_id %in% vec), ])
}

remove_tree_below_dbh <-  function(data_m){
return(data_m[data_m$dbh>=7.5 & !is.na(data_m$dbh), ])
}

check_data_missing_map<-  function(data_c, data_m){
 data_missing_carto <- data_m[!data_m$stem_id %in% data_c$stem_id, ]
 if(nrow(data_missing_carto)>0){
     write.csv(data_missing_carto, file.path("output", "data_missing_carto.csv"), row.names = FALSE)
     stop("error trees with measurement but no recorder in carto check output/data_missing_carto.csv")
  }
}

rm_data_missing_measure<-  function(data_c, data_m){
print(paste0('N removed tree with no measure ',
             length(data_c[data_c$stem_id %in% data_m$stem_id, "stem_id"])))
return(data_c[data_c$stem_id %in% data_m$stem_id, ])
}

remove_site_m <- function(df_m,df_p){
sites_to_remove <- df_p$plot_id[df_p$paper_yn == 'no']

return(df_m[!(df_m$plot_id %in% sites_to_remove), ])
}

remove_site_c <- function(df_c, df_p){
sites_to_remove <- df_p$plot_id[df_p$paper_yn == 'no']

return(df_c[!(df_c$plot_id %in% sites_to_remove), ])
}


fix_all_c <-  function(df_c, df_m){
 df_c <- fix_species_code(df_c)
 check_data_missing_map(df_c, df_m)
 df_c <- rm_data_missing_measure(df_c, df_m)
 df_c$x <-  as.numeric(df_c$x)
 df_c$y <-  as.numeric(df_c$y)
 df_c$x <-  as.numeric(df_c$x)
 df_c$z <-  as.numeric(df_c$z)
 df_c$year_birth <-  as.integer(df_c$year_birth)

return(df_c)
}



fix_all_m <-  function(df_c, df_m){
 df_m <- fix_code_status(df_m)
 df_m$year <-  as.numeric(df_m$year)
 df_m$dbh <-  as.numeric(df_m$dbh)
 df_m$base_crown_m <-  NULL
 if(sum(duplicated(df_m$measure_id))>0)
     stop('duplicated measure_id')
 return(df_m)
}

add_var_p <- function(df_p){
require(maptools)
require(sp)
require(rgdal)
df_p <- df_p[!is.na(df_p$x_lamb93), ]
coordinates(df_p) <- c('x_lamb93', 'y_lamb93')
proj4string(df_p) <- CRS("+init=epsg:2154")
df_p2 <- spTransform(df_p,  CRS("+init=epsg:4326"))
df_p$lat <-  df_p2$y_lamb93
df_p$long <-  df_p2$x_lamb93
df_p<- GetTopo(df_p)
list_res <- GetClimate(df_p)
df_p$MAT <- list_res$MAT
df_p$MAP <- list_res$MAP
geol <- GetGeol(df_p)
df_p$geol <-  geol
df <-  as.data.frame(df_p)
nn <- names(df)
names(df)[nn == "x"] <- 'x_lamb93'
names(df)[nn == "y"] <- 'y_lamb93'
df <- df[, c(3:ncol(df), 1:2)]
return(as.data.frame(df))
}


# TEST if all site in meta
check_all_sites_in_c_m <- function(d_c, d_m, d_p){
 if(sum(!unique(d_c$plot_id) %in% d_p$plot_id)) stop('missing site of c in p')
 if(sum(!unique(d_m$plot_id) %in% d_p$plot_id)) stop('missing site of m in p')
}


## CHECK WRONG XY COORDINATES
plot_xy_map <-  function(site, data, d_p){
 df_t <- data[data$plot_id == site, ]
 if(sum(!is.na(df_t$x))>0 & sum(!is.na(df_t$y))>0){
  min_x <-  d_p[d_p$plot_id == site, 'x_min']
  min_y <-  d_p[d_p$plot_id == site, 'y_min']
  max_x <-  d_p[d_p$plot_id == site, 'x_max']
  max_y <-  d_p[d_p$plot_id == site, 'y_max']
 par(pty = 's')
  plot(as.numeric(df_t$x), as.numeric(df_t$y),
       main = site,
       xlab = 'x', ylab = 'y',
       xlim = range(c(df_t$x, min_x, max_x,
                      df_t$y, min_y, max_y),
                    na.rm = TRUE),
       ylim = range(c(df_t$y, min_y, max_y,
                      df_t$x, min_x, max_x),
                    na.rm = TRUE))
  polygon(c(min_x, max_x, max_x, min_x),
          c(min_y, min_y, max_y, max_y),
          border = 'red')
  points_pb <-  df_t$x > max_x | df_t$x < min_x| df_t$y > max_y | df_t$y < min_y
  points(df_t$x[points_pb], df_t$y[points_pb], col = 'red', pch = 16)
  }
}


## Delete tree outside mapping area

get_wrong_xy_stem_id<-  function(site, data, d_p){
 df_t <- data[data$plot_id == site, ]
 if(sum(!is.na(df_t$x))>0 & sum(!is.na(df_t$y))>0){
  min_x <-  d_p[d_p$plot_id == site, 'x_min']
  min_y <-  d_p[d_p$plot_id == site, 'y_min']
  max_x <-  d_p[d_p$plot_id == site, 'x_max']
  max_y <-  d_p[d_p$plot_id == site, 'y_max']
  points_pb <-  (df_t$x > max_x | df_t$x < min_x |
                 df_t$y > max_y | df_t$y < min_y) &
                !is.na(df_t$x) & !is.na(df_t$y)
  stem_id_pb <- df_t$stem_id[points_pb]
  }else{
  stem_id_pb <-  c()
  }
 return(stem_id_pb)
}


remove_wrong_xy_tree_m <- function(d_m, d_c, d_p){
 vec_wrong_xy_stem_id <- unlist(lapply(unique(d_c$plot_id),
                                       get_wrong_xy_stem_id,
                                       data = d_c, d_p = d_p))
 return(d_m[!d_m$stem_id %in% vec_wrong_xy_stem_id, ])
}


remove_wrong_xy_tree_c <- function(d_m, d_c, d_p){
 vec_wrong_xy_stem_id <- unlist(lapply(unique(d_c$plot_id),
                                       get_wrong_xy_stem_id,
                                       data = d_c, d_p = d_p))
 return(d_c[!d_c$stem_id %in% vec_wrong_xy_stem_id, ])
}



read_all_data_and_clean <-  function(){
 #### TEST
 data_p <- read_data_plot()
 data_m <- read_mesures_all()
 data_c <- read_carto_all()
 data_m <- rename_data_m(data_m)
 data_c <- rename_data_c(data_c)
 data_c <- fix_all_c(data_c, data_m)
 data_m <- fix_all_m(data_c, data_m)
 df_c <- remove_tree_below_dbh_map(data_c, data_m)
 df_m <- remove_tree_below_dbh(data_m)
 data_c <- remove_site_c(data_c, data_p)
 data_m <- remove_site_m(data_m, data_p)
 data_p <- data_p[data_p$paper_yn == "yes", ]
 check_all_sites_in_c_m(data_c, data_m, data_p)
 pdf(file.path('figures',  'map_site_error.pdf'))
 lapply(unique(data_c$plot_id), plot_xy_map, data = data_c, d_p = data_p)
 dev.off()
 data_m <- remove_wrong_xy_tree_m(data_m, data_c, data_p)
 data_c<- remove_wrong_xy_tree_c(data_m, data_c, data_p)
 data_p <- add_var_p(data_p)
 print('done')
 saveRDS(list(c = data_c, m = data_m, p = data_p), file.path('output', 'list_data.rds'))
}


## list_data <- read_all_data_and_clean(path_samba, path_samba_r)
save_data_c <-  function(){
    list_d <- readRDS(file.path('output', 'list_data.rds'))
    write.csv(list_d$c, file.path('output', 'data_c.csv'),
              row.names = FALSE)
}
save_data_m <-  function(){
    list_d <- readRDS(file.path('output', 'list_data.rds'))
    write.csv(list_d$m, file.path('output', 'data_m.csv'),
              row.names = FALSE)
}
save_data_p <-  function(){
    list_d <- readRDS(file.path('output', 'list_data.rds'))
    write.csv(list_d$p, file.path('output', 'data_p.csv'),
              row.names = FALSE)
}


get_data_c <-  function() read.csv(file.path('output', 'data_c.csv'),
                                   stringsAsFactors = FALSE)
get_data_m <-  function() read.csv(file.path('output', 'data_m.csv'),
                                  stringsAsFactors = FALSE)
get_data_p <-  function() read.csv(file.path('output', 'data_p.csv'),
                                  stringsAsFactors = FALSE)

## GENERATE EMPTY METADATA
generate_metadata_and_save<- function(data, name_data){
 metadata <-  data.frame(variables = names(data),
                         type = unlist(lapply(data, class)),
                         unit = NA, definition = NA)
 write.csv(metadata, file.path('output',
                     paste0('metadata_',
                            name_data,
                            '_e.csv')), , row.names = FALSE)
}


generate_metadata_c_e<- function(data, name_data = 'data_c') generate_metadata_and_save(data, name_data )
generate_metadata_m_e<- function(data, name_data = 'data_m') generate_metadata_and_save(data, name_data )
generate_metadata_p_e<- function(data, name_data = 'data_p') generate_metadata_and_save(data, name_data )


data_sp_code<-  function(data_c,
          path_samba = "/run/user/1001/gvfs/smb-share:server=sdgrp1,share=services/EMGR/Projets/placette_foret/"){
 data_code <- read.xls(file.path(path_samba, 'données_autrestables',
                                 'Code_species.xlsx'),
                       stringsAsFactors = FALSE)
sp_vec <- unique(data_c$code_species)
 if(sum(!sp_vec %in% data_code$Code_species)>0)stop("missing species in Code_species.xlsx")
d <- data_code[data_code$Code_species %in% sp_vec, ]
names(d) <- c("code_species", "latin_name")
write.csv(d, file.path('output', 'data_sp_code.csv'), row.names = FALSE)
}



## Check Table

data_census <-  function(data_m){
data_census <- table(data_m$plot_id, data_m$year)
write.csv(data_census, file.path('output', 'data_census.csv'), row.names = FALSE)
}

data_sp_site<-  function(data_c){
data_sp <- table(data_c$code_species, data_c$plot_id)
write.csv(data_sp, file.path('output', 'data_sp_site.csv'), row.names = FALSE)
}


## print_table_control(data_m, data_c) TODO ADD TO REMAKE


### QUALITY CHECK


### Growth error

growth_dead_tree <- function(i, data, yy, j){
 dbh1 <- data[data$stem_id== j &
              data$year == yy[i], 'dbh']
 dbh2 <- data[data$stem_id== j &
              data$year == yy[i+1], 'dbh']
 df <- data.frame(stem_id = j,
                  year1 = yy[i], year2 = yy[i+1],
                  dbh1 = dbh1, dbh2 = dbh2,
                  code_diam1 = data[data$stem_id== j &
                                    data$year == yy[i], 'code_diam'],
                  code_diam2 = data[data$stem_id== j &
                                    data$year == yy[i+1], 'code_diam'],
                  code_status1 = data[data$stem_id== j &
                                    data$year == yy[i], 'code_status'],
                  code_status2 = data[data$stem_id== j &
                                    data$year == yy[i+1], 'code_status']
                  )
 return(df)
}


growth_tree_all <- function(j, df){
years <- sort(df[df$stem_id== j, ]$year)
list_t <- vector('list')
list_t <- lapply(seq_len(length(years) -1), growth_dead_tree, df, years, j)
res <- do.call(rbind, list_t)
return(res)
}

save_data_growth <-  function(df){
require(parallel)
cl <- makeCluster(12, type="FORK")
trees_ids<- unique(df$stem_id)
list_all <- parLapply(cl, trees_ids, growth_tree_all, df)
stopCluster(cl)
res <- do.call(rbind, list_all)
res$G <- (res$dbh2-res$dbh1)/(res$year2-res$year1)
res$same_code_diam <-  res$code_diam1 == res$code_diam2
write.csv(res, file.path('output', 'df_growth.csv'), row.names = FALSE)
}

get_data_growth <-  function(){
read.csv(file.path('output', 'df_growth.csv'))
}


## df_growth <- data_growth(data_m)


cook_outlier_detec <- function(df, x, y){
 require(MASS)
 df <-  df[complete.cases(df[, c(x, y)]), ]
 fr <- as.formula(paste(y, " ~ ", x))
 ols <- lm(fr, df)
 d1 <- cooks.distance(ols)
 r <- stdres(ols)
 a <- cbind(df, d1, r)
 a_out <- a[d1 > 6*mean(d1), ]
 points(a_out[[x]], a_out[[y]], pch = 4)
 return(a_out$stem_id)
}


plot_quant_reg <- function(df, x, y,
                           probs_vec = c(0.005, 0.995),
                           smooth_dg = 3){
 require(quantreg)
 require(splines)
 df <-  df[complete.cases(df[, c(x, y)]), ]
 x_seq_pred <- seq(from = min(df[[x]], na.rm = TRUE),
                   to = max(df[[x]], na.rm = TRUE),
                   length.out = 100)
 fr <- as.formula(paste(y, " ~ ", paste("bs(", x, ", df = smooth_dg)")))
 df_pred <- data.frame( 0, x_seq_pred)
 names(df_pred) <- c(y, x)
 X <- model.matrix(fr, df_pred)
 for(tau in probs_vec){
         fit <- rq(fr, tau, data=df)
         accel.fit <- X %*% fit$coef
         lines(x_seq_pred,accel.fit, col = 'black')
         if(tau == probs_vec[1]){
         vec_pb <- df[[y]] < predict(fit)
         }else{
         vec_pb <- df[[y]] > predict(fit)
         }
         points(df[vec_pb, x], df[vec_pb, y], pch = 16, col = 'red')
         df[[paste0('tau',tau)]] <- vec_pb
         }
 return(df$stem_id[apply(df[, paste0('tau', probs_vec)], MARGIN = 1, sum)>0])
}

plot_growth_error <-  function(df){
 plot(df$dbh1, df$G, cex = 0.2,
      xlab = 'Intial DBH (cm)', ylab = 'Diameter Growth (cm/yr.)')
 quant_id <- plot_quant_reg(df, 'dbh1', 'G')
 cook_id <- cook_outlier_detec(df, 'dbh1', 'G')
}


save_growth_error <-  function(df){
 plot(df$dbh1, df$G, cex = 0.2,
      col = c('green', 'black')[unclass(factor(df$same_code_diam))],
      xlab = 'Intial DBH (cm)', ylab = 'Diameter Growth (cm/yr.)')
 abline(h = quantile(df$G, probs = c(0.0025, 0.9975), na.rm = TRUE),
        col = 'gray')
 quant_id <- plot_quant_reg(df, 'dbh1', 'G')
 cook_id <- cook_outlier_detec(df, 'dbh1', 'G')
 all_id <- c(as.character(quant_id), as.character(cook_id))
 write.csv(data.frame(stem_id = df[df$stem_id %in% all_id[duplicated(all_id)], ]),
           file = file.path('output', 'tree_wrong_growth.csv'),
           row.names = FALSE)
}

## check if dead tree are alive again TODO
save_stem_id_resurrected <- function(df){
 d <- df[df$code_status1 %in% c('9990', '9991') &
                df$code_status2 == '0000',]
 print(dim(d))
 write.csv(d, file.path('output', 'data_resurrected_tree.csv'), row.names = FALSE)
}

## save_stem_id_resurrected(df_growth)
## no trees



# check allometry TODO REMOVE base_crown_h

## test H tot and H crown
save_stem_id_wrong_crown_h<- function(df_m){
 vec_pb <- df_m$h_tot/apply(df_m[ , paste0('crown_h', 1:4)],
                             MARGIN = 1, mean, na.rm = TRUE)<1
 d <- df_m$stem_id[ vec_pb & !is.na(vec_pb)]
 print(dim(d))
 write.csv(d, file.path('output', 'data_wrong_crown_h_tree.csv'), row.names = FALSE)

}
#save_stem_id_wrong_crown_h(data_m)

# plots
plot_allo_error <- function(data){
 par(mfrow = c(2, 2))
 plot(data$dbh, data$h_tot, xlab = 'dbh', ylab = 'h', cex = 0.3)
 plot_quant_reg(data, 'dbh', 'h_tot')
 cook_outlier_detec(data, 'dbh', 'h_tot')
 data$crown_r <- apply(data[ , paste0('crown_r', 1:4)],
                       MARGIN = 1, mean, na.rm = TRUE)
 plot(data$dbh, data$crown_r,
      xlab = 'dbh', ylab= 'crown radius',
      cex = 0.3)
 plot_quant_reg(data, 'dbh', 'crown_r')
 cook_outlier_detec(data, 'dbh', 'crown_r')
 data$crown_h <- apply(data[ , paste0('crown_h', 1:4)],
                       MARGIN = 1, mean, na.rm = TRUE)
 vec_pb <- data$h_tot/data$crown_h<1
 plot(data$h_tot, data$crown_h,
      xlab = 'h', ylab= 'crown height',
      col = unclass(factor(vec_pb & !is.na(vec_pb))),
      cex = 0.3)
 plot_quant_reg(data, 'h_tot', 'crown_h')
 cook_outlier_detec(data, 'h_tot', 'crown_h')
 lines(0:100, 0:100, col = 'red')
}


save_allo_error <-  function(data){
 plot(data$dbh, data$h_tot, xlab = 'dbh', ylab = 'h', cex = 0.3,
      col = unclass(factor(data$plot_id)))
 abline(h=50)
 quant_id_1<- plot_quant_reg(data, 'dbh', 'h_tot')
 cook_outlier_detec(data, 'dbh', 'h_tot')
 data$crown_r <- apply(data[ , paste0('crown_r', 1:4)],
                       MARGIN = 1, mean, na.rm = TRUE)
 plot(data$dbh, data$crown_r,
      xlab = 'dbh', ylab= 'crown radius',
      cex = 0.3)
 quant_id_2 <- plot_quant_reg(data, 'dbh', 'crown_r')
 cook_outlier_detec(data, 'dbh', 'crown_r')
 vec_pb <- data$h_tot/apply(data[ , paste0('crown_h', 1:4)],
                             MARGIN = 1, mean, na.rm = TRUE)<1
 outlier_3 <- data$stem_id[vec_pb & !is.na(vec_pb)]
 write.csv(data.frame(stem_id = unique(c(quant_id_1, quant_id_2, outlier_3))),
           file = file.path('output', 'tree_wrong_allo.csv'),
           row.names = FALSE)

 vec_pb <- (data$h_tot>50 & !is.na(data$h_tot)) | (data$crown_r>7 & !is.na(data$crown_r))
 d <- data$stem_id[ vec_pb & !is.na(vec_pb)]
 print(dim(d))
 write.csv(d, file.path('output', 'data_wrong_allo2.csv'), row.names = FALSE)
}


## Tables for data paper

# TABLE 1 PLOT DESCRIPTION plot_id (check diff with plot_id) area, elvation, lat, long,

table_plot <- function(df_p){
table_p <- df_p[, c('plot_id', 'area', 'elevation', 'slope', 'aspect', 'lat',
                    'long', 'MAT', 'MAP', 'geol')]
write.csv(table_p, file.path('output', 'table_plot.csv'), row.names = FALSE)
}


# TABLE 2 plot_id year_first_meas N census, main species, N initial G initial

table_stand_descrip<- function(df_p, df_m, df_c, treshold_sp= 0.1){
require(dplyr)
table_p2 <- df_p[, c("plot_id", "area")]
df <- dplyr::left_join(df_m, df_c[, c('stem_id', 'code_species')], by = 'stem_id')
table_p3 <- df %>% dplyr::group_by(plot_id) %>%
                dplyr::summarise(first_year = min(year),
                          n_census = n_distinct(year))
df <- df %>% dplyr::filter(code_status %in% c('0', '8881', '8882')) %>%
         dplyr::arrange(year) %>% dplyr::distinct(stem_id)
main_sp <- tapply(df$code_species,
                  df$plot_id,
                  function(x) paste(names(table(x))[table(x)/
                                                    length(x)>treshold_sp],
                                    collapse = ' and '))
n_init<- tapply(df$code_species,
                df$plot_id,
                length)
ba_init<- tapply(pi*df$dbh^2/4,
                df$plot_id,
                sum)
table_p4 <- data.frame(plot_id = names(main_sp),
                       main_sp = main_sp,
                       n_init = n_init, ba_init = ba_init,
                       stringsAsFactors = FALSE)
tab <- dplyr::left_join(table_p2, table_p3, by = 'plot_id')
tab <- dplyr::left_join(tab, table_p4, by = 'plot_id')
tab$ba_init <- tab$ba_init/(tab$area * 10000)
tab$n_init <- tab$n_init/(tab$area)
write.csv(tab, file.path('output', 'table_stand_descrip.csv'), row.names = FALSE)
}

# TABLE 3 diam min , n of height measure , n of crown radius measure, n of crown height measure, dead and stump atestablish Y/N , loc xy or quadrat


table_stand_allo<- function(df_p, df_m, df_c){
require(dplyr)
df_m <- df_m %>% dplyr::rowwise()  %>%
    dplyr::mutate(crown_h = mean(c(crown_h1, crown_h2,
                            crown_h3, crown_h4),
                          na.rm = TRUE),
           crown_r = mean(c(crown_r1, crown_r2,
                            crown_r3, crown_r4),
                          na.rm = TRUE))
tab1 <- df_m %>% dplyr::group_by( plot_id) %>%
    dplyr::summarise(n_h = sum(!is.na(h_tot)),
              n_crown_h = sum(!is.na(crown_h)),
              n_crown_r = sum(!is.na(crown_r)))
df <- df_m %>%
         dplyr::arrange(year) %>% dplyr::distinct(stem_id)
tab2 <- df %>% dplyr::group_by(plot_id) %>%
    dplyr::summarise(dead_init_tf = sum(code_status %in% c("9991", "9990"))>0)

tab3 <- df_c%>% dplyr::group_by(plot_id) %>%
    dplyr::summarise(xy_tf = sum(!is.na(x))>0)

tab <- dplyr::left_join(tab1, tab2,  by = 'plot_id')
tab <- dplyr::left_join(tab, tab3,  by = 'plot_id')
write.csv(tab, file.path('output', 'table_stand_allo.csv'), row.names = FALSE)
}

#

species_code <- function(df_c, path_samba = "/run/user/1001/gvfs/smb-share:server=sdgrp1,share=services/EMGR/Projets/placette_foret/"){
code_species <- read.xls(file.path(path_samba, 'données_autrestables',
                                 'Code_species.xlsx'),
                       stringsAsFactors = FALSE)
names(code_species) <- c("code_species", "Latin.name")
df <- code_species[code_species$code_species %in% unique(df_c$code_species), ]
write.csv(df, file.path('output', 'species_code.csv'), row.names = FALSE)
}

status_code <- function(path_samba = "/run/user/1001/gvfs/smb-share:server=sdgrp1,share=services/EMGR/Projets/placette_foret/"){
code_status <- read.xls(file.path(path_samba, 'données_autrestables',
                                 'Code_status.xlsx'),
                       stringsAsFactors = FALSE)
write.csv(code_status, file.path('output', 'status_code.csv'), row.names = FALSE)
}

get_meta_c<- function(path_samba = "/run/user/1001/gvfs/smb-share:server=sdgrp1,share=services/EMGR/Projets/placette_foret/"){
df <- read.xls(file.path(path_samba, 'données_autrestables',
                                 'metadonnees_carto.xlsx'),
                       stringsAsFactors = FALSE)
write.csv(df, file.path('output', 'metadata_data_c.csv'), row.names = FALSE)
}

get_meta_m<- function(path_samba = "/run/user/1001/gvfs/smb-share:server=sdgrp1,share=services/EMGR/Projets/placette_foret/"){
df <- read.xls(file.path(path_samba, 'données_autrestables',
                                 'metadonnees_mesures.xlsx'),
                       stringsAsFactors = FALSE)
write.csv(df, file.path('output', 'metadata_data_m.csv'), row.names = FALSE)
}


get_meta_p<- function(path_samba = "/run/user/1001/gvfs/smb-share:server=sdgrp1,share=services/EMGR/Projets/placette_foret/"){
df <- read.xls(file.path(path_samba, 'données_autrestables',
                                 'metadonnees_plot.xlsx'),
                       stringsAsFactors = FALSE)
write.csv(df, file.path('output', 'metadata_data_p.csv'), row.names = FALSE)
}
