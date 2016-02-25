###########################################
# Functions to read plots data ------------
###########################################

library(gdata)

# read plots coordinates TODO UPDATE BY MARC DONE
read_data_plot <- function(path_samba = "/run/user/1001/gvfs/smb-share:server=sdgrp1,share=services/EMGR/Projets/placette_foret/"){
 data_plot <- read.xls(file.path(path_samba, 'données_autrestables',
                                 'metadonnees_placette_2015.xlsx'),
                       stringsAsFactors = FALSE)
 names(data_plot) <- c('site_id', 'owner_id', 'year_first_mes', 'N_census',
                       'area', "x_min", "x_max", "y_min", "y_max", 'aspect',
                       'elevation', 'GPS_loc',
                       'long', 'lat', 'x_lamb2_et', 'y_lamb2_et')
 return(data_plot)
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
}



# rename data
rename_data_c <- function(df){
names(df) <- c('map_year', 'map_id', 'site_id', 'tree_id',
                   'quadrat_id', 'code_species',
                   'x', 'y', 'z', 'year_birth') 
df$tree_id <- df$map_id
df$map_id <-  NULL
return(df)
}


rename_data_m <- function(df){
names(df) <- c('mesure_id', 'site_id', 'year', 'tree_id',
                   'code_status', 'code_diam', 'dbh', 'h_tot',
                   'crown_h1', 'crown_h2', 'crown_h3', 'crown_h4',
                   'crown_r1', 'crown_r2', 'crown_r3', 'crown_r4',
                   'base_crown_h', 'strate')
df$tree_id <- paste0(df$tree_id, df$site_id)
return(df)
}



## FUNCTIONS TO CLEAN DATA

fix_species_code <-  function(df){
 old_name <- c('SOR', '', 'CHE', 'SA spp', 'ALI', 'BOU', 'ALIB', 'SO',
                 'AL spp', 'QU spp', 'IF', 'MER', 'SAP', 'ERP',
               'FRE', 'BETU', 'CHA', 'ORM') 
 new_name <- c('SOAU', 'ND', 'QUSP', 'SASP', 'SOAR', 'BEPE', 'SOAR',
               'SOSP', 'ALSP', 'QUSP', 'TABA', 'PRAV', 'ABAL',
               'ACPL', 'FREX', 'BEPE', 'CABE', 'ULSP') 
 df$code_species <-  factor(df$code_species)
 names_species <- levels(df$code_species)
 names_species[match(old_name, names_species)] <- new_name
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
data_d_min<- tapply(data_m$dbh, INDEX = data_m$site_id, min, na.rm = TRUE)
data_dbh_max_tree<- tapply(data_m$dbh,
                           INDEX = data_m$tree_id,
                           max, na.rm = TRUE)
data_dbh_max_tree[data_dbh_max_tree == -Inf] <-  NA
tree_id_remove <- names(data_dbh_max_tree)[data_dbh_max_tree <7.5 &
                                           !is.na(data_dbh_max_tree)]
return(tree_id_remove)
}

remove_tree_below_dbh_map <-  function(data_c, data_m){
vec <- get_id_data_dbh_min(data_m)
return(data_c[!(data_c$tree_id %in% vec), ])
}    

remove_tree_below_dbh <-  function(data_m){
return(data_m[data_m$dbh>=7.5 & !is.na(data_m$dbh), ])
}

add_data_missing_map<-  function(data_c, data_m){
 data_missing_carto <- data_m[!data_m$tree_id %in% data_c$tree_id, ]
 data_t <- data_c[1:nrow(data_missing_carto), ]
 data_t[ , ] <-  NA
 data_t$site_id <- data_missing_carto$site_id
 data_t$tree_id <- data_missing_carto$tree_id
 data_t$map_year <- data_missing_carto$year
 df <- rbind(data_c, data_t)
 return(df)
}

rm_data_missing_measure<-  function(data_c, data_m){
return(data_c[data_c$tree_id %in% data_m$tree_id, ])
}

remove_site_m <- function(df_m){
sites_to_remove <- c('Bachat', 'Claret', 'Dent du Villard', 'La Cordeliere',
                     'Lanslebourg', 'LesArcs', 'Meillarot', 'NantGranges4',
                     'Pralognan', 'Riviere', 'Saisies1', 'Saisies2',
                     'Sixt-Molliet', 'Vaujany', 'Vercors1', 'Vercors3',
                     'Vercors4')

return(df_m[!(df_m$site_id %in% sites_to_remove), ])
}

remove_site_c <- function(df_c){
sites_to_remove <- c('Bachat', 'Claret', 'Dent du Villard', 'La Cordeliere',
                     'Lanslebourg', 'LesArcs', 'Meillarot', 'NantGranges4',
                     'Pralognan', 'Riviere', 'Saisies1', 'Saisies2',
                     'Sixt-Molliet', 'Vaujany', 'Vercors1', 'Vercors3',
                     'Vercors4')

return(df_c[!(df_c$site_id %in% sites_to_remove), ])
}    


fix_all_c <-  function(df_c, df_m){
 df_c <- fix_species_code(df_c)
 df_c <- remove_tree_below_dbh_map(df_c, df_m)
 df_c <- add_data_missing_map(df_c, df_m)
 df_c <- rm_data_missing_measure(df_c, df_m)
 df_c <- remove_site_c(df_c)
 df_c$x <-  as.numeric(df_c$x)
 df_c$y <-  as.numeric(df_c$y)    
 df_c$x <-  as.numeric(df_c$x)
 df_c$z <-  as.numeric(df_c$z)    
 df_c$year_birth <-  as.integer(df_c$year_birth)    
 df_c$map_year<-  as.integer(df_c$map_year)    

return(df_c)
}


fix_all_m <-  function(df_c, df_m){
 df_m <- fix_code_status(df_m)
 df_m <- remove_tree_below_dbh(df_m)
 df_m <- remove_site_m(df_m)
 df_m$year <-  as.numeric(df_m$year)
 df_m$dbh <-  as.numeric(df_m$dbh)
 df_m$base_crown_m <-  NULL
 return(df_m)
}


# TEST if all site in meta
check_all_sites_in_c_m <- function(d_c, d_m, d_p){
 if(sum(!unique(d_c$site_id) %in% d_p$site_id)) stop('missing site of c in p')
 if(sum(!unique(d_m$site_id) %in% d_p$site_id)) stop('missing site of m in p')
}


## CHECK WRONG XY COORDINATES
plot_xy_map <-  function(site, data, d_p){
 df_t <- data[data$site_id == site, ]
 if(sum(!is.na(df_t$x))>0 & sum(!is.na(df_t$y))>0){
  min_x <-  d_p[d_p$site_id == site, 'x_min']
  min_y <-  d_p[d_p$site_id == site, 'y_min']
  max_x <-  d_p[d_p$site_id == site, 'x_max']
  max_y <-  d_p[d_p$site_id == site, 'y_max']
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

get_wrong_xy_tree_id<-  function(site, data, d_p){
 df_t <- data[data$site_id == site, ]
 if(sum(!is.na(df_t$x))>0 & sum(!is.na(df_t$y))>0){
  min_x <-  d_p[d_p$site_id == site, 'x_min']
  min_y <-  d_p[d_p$site_id == site, 'y_min']
  max_x <-  d_p[d_p$site_id == site, 'x_max']
  max_y <-  d_p[d_p$site_id == site, 'y_max']
  points_pb <-  (df_t$x > max_x | df_t$x < min_x |
                 df_t$y > max_y | df_t$y < min_y) &
                !is.na(df_t$x) & !is.na(df_t$y)
  tree_id_pb <- df_t$tree_id[points_pb]
  }else{
  tree_id_pb <-  c()
  }
 return(tree_id_pb)
}


remove_wrong_xy_tree_m <- function(d_m, d_c, d_p){
 vec_wrong_xy_tree_id <- unlist(lapply(unique(d_c$site_id),
                                       get_wrong_xy_tree_id,
                                       data = d_c, d_p = d_p))
 return(d_m[!d_m$tree_id %in% vec_wrong_xy_tree_id, ])
}


remove_wrong_xy_tree_c <- function(d_m, d_c, d_p){
 vec_wrong_xy_tree_id <- unlist(lapply(unique(d_c$site_id),
                                       get_wrong_xy_tree_id,
                                       data = d_c, d_p = d_p))
 return(d_c[!d_c$tree_id %in% vec_wrong_xy_tree_id, ])
}    



read_all_data_and_clean <-  function(){
 data_plot <- read_data_plot()
 data_m <- read_mesures_all()
 data_c <- read_carto_all()
 data_m <- rename_data_m(data_m)
 data_c <- rename_data_c(data_c)
 data_c <- fix_all_c(data_c, data_m)
 data_m <- fix_all_m(data_c, data_m)
 check_all_sites_in_c_m(data_c, data_m, data_plot)
 pdf(file.path('figures', 'map_site_error.pdf'))
 lapply(unique(data_c$site_id), plot_xy_map, data = data_c, d_p = data_plot)
 dev.off()
 data_m <- remove_wrong_xy_tree_m(data_m, data_c, data_plot)
 data_c<- remove_wrong_xy_tree_c(data_m, data_c, data_plot)
 data_plot <- data_plot[data_plot$site_id %in% unique(data_c$site_id), ]
 print('done')
return(list(c = data_c, m = data_m, p = data_plot))
}


## list_data <- read_all_data_and_clean(path_samba, path_samba_r)

save_data_c <-  function(list_d) saveRDS(list_d$c, file.path('output', 'data_c.rds'))
save_data_m <-  function(list_d) saveRDS(list_d$m, file.path('output', 'data_m.rds'))
save_data_p <-  function(list_d) saveRDS(list_d$p, file.path('output', 'data_p.rds'))


get_data_c <-  function() readRDS(file.path('output', 'data_c.rds'))
get_data_m <-  function() readRDS(file.path('output', 'data_m.rds'))
get_data_p <-  function() readRDS(file.path('output', 'data_p.rds'))



## GENERATE EMPTY METADATA
generate_metadata_and_save<- function(data, name_data){
 metadata <-  data.frame(variables = names(data),
                         type = unlist(lapply(data, class)),
                         unit = NA, definition = NA)
 write.csv(metadata, file.path('output',
                     paste0('metadata_',
                            name_data,
                            '.csv')))
 write.csv(data, file.path('output',
                     paste0(name_data,
                            '.csv')))
}


generate_metadata_c <- function(data, name_data = 'data_c') generate_metadata_and_save(data, name_data )
generate_metadata_m <- function(data, name_data = 'data_m') generate_metadata_and_save(data, name_data )
generate_metadata_p <- function(data, name_data = 'data_p') generate_metadata_and_save(data, name_data )



## Check Table

data_census <-  function(data_m){
data_census <- table(data_m$site_id, data_m$year)
write.csv(data_census, file.path('output', 'data_census.csv'))
}

data_sp <-  function(data_c){
data_sp <- table(data_c$code_species, data_c$site_id)
write.csv(data_sp, file.path('output', 'data_sp_code.csv'))
}

### apply function print check tables

print_table_control <-  function(data_m, data_c){
data_census(data_m)
data_sp(data_c)
}

## print_table_control(data_m, data_c) TODO ADD TO REMAKE


### QUALITY CHECK

## DEAD TREES THAT ARE ALIVE AGAIN OK
## BAD GROWTH MEASUREMENTS OK




### Growth error

growth_dead_tree <- function(i, data, yy, j){
 dbh1 <- data[data$tree_id== j &
              data$year == yy[i], 'dbh']  
 dbh2 <- data[data$tree_id== j &
              data$year == yy[i+1], 'dbh']
 df <- data.frame(tree_id = j,
                  year1 = yy[i], year2 = yy[i+1],
                  dbh1 = dbh1, dbh2 = dbh2,
                  code_diam1 = data[data$tree_id== j &
                                    data$year == yy[i], 'code_diam'],
                  code_diam2 = data[data$tree_id== j &
                                    data$year == yy[i+1], 'code_diam'],
                  code_status1 = data[data$tree_id== j &
                                    data$year == yy[i], 'code_status'],
                  code_status2 = data[data$tree_id== j &
                                    data$year == yy[i+1], 'code_status']
                  )
 return(df)
}


growth_tree_all <- function(j, df){ 
years <- sort(df[df$tree_id== j, ]$year)
list_t <- vector('list')
list_t <- lapply(seq_len(length(years) -1), growth_dead_tree, df, years, j)
res <- do.call(rbind, list_t)
return(res)
}

save_data_growth <-  function(df){
require(parallel)
cl <- makeCluster(12, type="FORK")
trees_ids<- unique(df$tree_id)
list_all <- parLapply(cl, trees_ids, growth_tree_all, df)
stopCluster(cl)
res <- do.call(rbind, list_all)
res$G <- (res$dbh2-res$dbh1)/(res$year2-res$year1)
res$same_code_diam <-  res$code_diam1 == res$code_diam2
saveRDS(res, file.path('output', 'df_growth.rds'))
}

get_data_growth <-  function(){
readRDS(file.path('output', 'df_growth.rds'))
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
 return(a_out$tree_id)
}

mvoutlier_detec <- function(df, x, y){
 require(mvoutlier)
 df <-  df[complete.cases(df[, c(x, y)]), c(x, y)]
 x.out <- pcout(df, makeplot = FALSE)
 points(df[[x]][x.out$wfinal < quantile(x.out$wfinal, 0.07)],
        df[[y]][x.out$wfinal < quantile(x.out$wfinal, 0.07)],
        pch = c(3), col = 'blue')
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
 return(df$tree_id[apply(df[, paste0('tau', probs_vec)], MARGIN = 1, sum)>0])
}

plot_growth_error <-  function(df){
 plot(df$dbh1, df$G, cex = 0.2,
      col = c('green', 'black')[unclass(factor(df$same_code_diam))],
      xlab = 'Intial DBH (cm)', ylab = 'Diameter Growth (cm/yr.)')
 abline(h = quantile(df$G, probs = c(0.0025, 0.9975), na.rm = TRUE),
        col = 'gray')
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
 write.csv(data.frame(tree_id = all_id[duplicated(all_id)]),
           file = file.path('output', 'tree_wrong_growth.csv'),
           row.names = FALSE)
}

## check if dead tree are alive again TODO
save_tree_id_resurrected <- function(df){
 d <- df[df$code_status1 %in% c('9990', '9991') &
                df$code_status2 == '0000',]
 print(dim(d))
 write.csv(d, file.path('output', 'data_resurrected_tree.csv'))
}

## save_tree_id_resurrected(df_growth)
## no trees



# check allometry TODO REMOVE base_crown_h

## test H tot and H crown
save_tree_id_wrong_crown_h<- function(df_m){
 vec_pb <- df_m$h_tot/apply(df_m[ , paste0('crown_h', 1:4)],
                             MARGIN = 1, mean, na.rm = TRUE)<1
 d <- df_m$tree_id[ vec_pb & !is.na(vec_pb)]
 print(dim(d))
 write.csv(d, file.path('output', 'data_wrong_crown_h_tree.csv'))
}
#save_tree_id_wrong_crown_h(data_m)

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
 vec_pb <- data$h_tot/apply(data[ , paste0('crown_h', 1:4)],
                             MARGIN = 1, mean, na.rm = TRUE)<1
 plot(data$h_tot, apply(data[ , paste0('crown_h', 1:4)],
                        MARGIN = 1, mean, na.rm = TRUE),
      xlab = 'h', ylab= 'crown height',
      col = unclass(factor(vec_pb & !is.na(vec_pb))))
 lines(0:100, 0:100, col = 'red')
}


save_allo_error <-  function(data){
 plot(data$dbh, data$h_tot, xlab = 'dbh', ylab = 'h', cex = 0.3)
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
 outlier_3 <- data$tree_id[vec_pb & !is.na(vec_pb)]
 write.csv(data.frame(tree_id = unique(c(quant_id_1, quant_id_2, outlier_3))),
           file = file.path('output', 'tree_wrong_allo.csv'),
           row.names = FALSE)
}


## pdf(file.path('figures', 'allo_error.pdf'))
## plot_allo_error(data_m)
## dev.off()

map_plots <- function(df_p){
 require(maps)   
 map ('france', xlim = c(5, 7.2), ylim = c(44, 46.5))
 points(df_p$long, df_p$lat, col = 'red')
}



