# gather plotting data----
# get population polygons, graphs and set colors

mapFun <- function(spp = NULL, metric = NULL){
  
  if(spp == 'Steelhead'){
    full_pop <- sthd_pop
    if(metric == 'Intrinsic Potential'){
      huc10 <- huc10_metrics %>%
        rename(metric = sthd_ip)
    } else if(metric == 'Redd Capacity'){
      huc10 <- huc10_metrics %>%
        rename(metric = sthd_r)
    } else if(metric == 'Summer Parr Capacity'){
      huc10 <- huc10_metrics %>%
        rename(metric = sthd_sm)
    } else if(metric == 'Overwintering Capacity'){
      huc10 <- huc10_metrics %>%
        rename(metric = sthd_w)
    }
  } else if(spp == 'Sp/sm Chinook') {
    full_pop <- spsm_pop
    if(metric == 'Intrinsic Potential'){
      huc10 <- huc10_metrics %>%
        rename(metric = spsm_ip)
    } else if(metric == 'Redd Capacity'){
      huc10 <- huc10_metrics %>%
        rename(metric = spsm_r)
    } else if(metric == 'Summer Parr Capacity'){
      huc10 <- huc10_metrics %>%
        rename(metric = spsm_sm)
    } else if(metric == 'Overwintering Capacity'){
      huc10 <- huc10_metrics %>%
        rename(metric = spsm_w)
    }
  }
  
  
  # pop_layer <- full_pop %>%
  #   inner_join(pop_est %>%
  #                arrange(TRT_POPID, desc(spawn_year)) %>%
  #                group_by(TRT_POPID) %>%
  #                mutate(rank = 1:n(),
  #                       n = n()) %>%
  #                filter(rank <= 5) %>%
  #                group_by(TRT_POPID) %>%
  #                summarise(avg5 = round(mean(estimate))) %>%
  #                ungroup()
  #   )
  # 
  # p_all <- lapply(unique(pop_layer$TRT_POPID), popGraph)
  # 
  # # get site point estimates, graphs and set colors
  # 
  # site_est_spp <- site_est %>%
  #   filter(species == spp) %>%
  #   select(site_id, site_name, species, run, spawn_year, estimate, lower_ci, upper_ci, sd, cv) %>%
  #   st_set_geometry(NULL)
  # 
  # siteTable <- function(x){
  #   htmlTable(site_est_spp[site_est_spp$site_id==x,5:10],
  #             rnames = FALSE,
  #             header = c('Spawn Year', 'Estimate', 'Lower 95% CI', 'Upper 95% CI',
  #                        'Std. Dev.', 'CV'),
  #             caption = paste0(site_est_spp[site_est_spp$site_id==x,2][1],
  #                              " (",
  #                              x,
  #                              ")")
  #   )
  # }
  # 
  # site_tab <- lapply(unique(site_est_spp$site_id), siteTable)
  # 
  # site_est_layer <- site_est %>%
  #   filter(species == spp) %>%
  #   distinct(site_id, site_name, site_type_name, geometry)
  
  
  savedlist = list(full_pop, huc10) #, pop_layer, p_all, site_est_spp, site_tab, site_est_layer)
  names(savedlist) = c('full_pop', 'huc10')#,'pop_layer', 'p_all', 'site_est_spp', 'site_tab', 'site_est_layer')
  
  return(savedlist) 
}