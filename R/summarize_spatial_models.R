#' Title Summarize and map populations by summarized IP and capacity estimates.
#' Author Ryan Kinzer
#' Created March 8, 2024
#' 
#' # Author: Ryan N. Kinzer
# Purpose: Produce DFRM monitoring strategy maps.
# Created: 2/20/24
# Last Modified: 
# Notes: 

# Load needed libraries -----
library(tidyverse)
library(sf)
library(scales)
library(ggmap)
library(maps)
#library(albersusa)
library(patchwork)


crs_default <- 26911 #4326 #26911

# filepath
spatial_files <- 'C://Spatial_Files/QGIS Files/NAD83_11N_26911/'


# dissolve huc12 into huc10----
huc12 <- st_read(paste0(spatial_files,'HUC12/HUC12_snake_river.shp')) %>%
  st_transform(crs = crs_default)

huc10 <- huc12 %>%
  group_by(HUC_10, HU_10_NAME) %>%
  summarise()

# load stream layers----
qrf <- st_read(paste0(spatial_files,'TRT_ICB_IP/snake_basin_qrf_capacity.shp'))
ip <- st_read(paste0(spatial_files,'TRT_ICB_IP/snake_basin_intrinsic_potential.shp'))


# intrinsic potential plots----
# chinook ip
chnk_ip_stream <- ip %>%
  filter(CHINRATE != 0) #%>%
  # mutate(CHINRATE = factor(CHINRATE, levels = c(1, 2, 3), labels = c('Low', 'Medium', 'High')))

chnk_ip_stream_fig <- ggplot() +
  geom_sf(data = huc10, fill = 'black') +
  geom_sf(data = chnk_ip_stream, aes(colour = CHINRATE)) +
  #scale_colour_viridis_d(option = 'turbo', direction = -1) +
  #scale_color_distiller(palette = 'Spectral') +
  # scale_color_manual(values = c('High' = 'red','Medium' =  'green','Low' =  'blue')) +
  scale_colour_distiller(palette = "RdYlBu") +
  labs(title = "NOAA's Intrinsic Potential",
      subtitle = "Sp/sm Chinook Salmon",
      colour = NULL) +
  theme_void() +
  theme(legend.position = 'bottom')

# steelhead ip
sthd_ip_stream <- ip %>%
  filter(STHDRATE != 0) #%>%
  #mutate(STHDRATE = factor(STHDRATE, levels = c(1, 2, 3), labels = c('Low', 'Medium', 'High')))

sthd_ip_stream_fig <- ggplot() +
  geom_sf(data = huc10, fill = 'black') +
  geom_sf(data = sthd_ip_stream, aes(colour = STHDRATE)) +
  # scale_color_manual(values = c('High' = 'red','Medium' =  'green','Low' =  'blue')) +
  scale_colour_distiller(palette = "RdYlBu") +
  labs(subtitle = "Summer Steelhead",
       caption = 'Warmer colors have a higher historical potential based on geomorphic characteristics.',
       colour = NULL) +
  theme_void() +
  theme(legend.position = 'bottom')

ip_stream_fig <- (chnk_ip_stream_fig + sthd_ip_stream_fig) + plot_layout(guides = 'collect') &
  theme(legend.position='none')

#ip_stream_fig
ggsave('./figures/noaa_intrinsic_potential.png', ip_stream_fig, width = 11, height = 8.5)

# qrf

num_groups <- 4

chnk_qrf_stream <- qrf %>%
  filter(chnk == 1) %>%
  mutate(win_parr= cut(chnk_w,
                       breaks = quantile(.$chnk_w, probs = seq(0, 1, length.out = num_groups + 1))
                       )
  ) %>%
  mutate(sum_parr= cut(chnk_sm,
                       breaks = quantile(.$chnk_sm, probs = seq(0, 1, length.out = num_groups + 1))
  )
  )



chnk_qrf_sum_fig <- ggplot() +
  geom_sf(data = huc10, fill = 'black') +
  geom_sf(data = chnk_qrf_stream, aes(colour = sum_parr)) +
  scale_colour_brewer(palette = "RdYlBu") +
  labs(title = "Sp/sm Chinook Salmon",
       subtitle = "Summer Capacity") +
  theme_void() +
  theme(legend.position = 'none')


chnk_qrf_winter_fig <- ggplot() +
  geom_sf(data = huc10, fill = 'black') +
  geom_sf(data = chnk_qrf_stream, aes(colour = win_parr)) +
  scale_colour_brewer(palette = "RdYlBu") +
  labs(subtitle = "Winter Capacity") +
  theme_void() +
  theme(legend.position = 'none')


sthd_qrf_stream <- qrf %>%
  filter(sthd == 1) %>%
  mutate(win_parr= cut(sthd_w,
                        breaks = quantile(.$sthd_w, probs = seq(0, 1, length.out = num_groups + 1))
                       )
         ) %>%
  mutate(sum_parr= cut(sthd_sm,
                       breaks = quantile(.$sthd_sm, probs = seq(0, 1, length.out = num_groups + 1))
                       )
  )


sthd_qrf_sum_fig <- ggplot() +
  geom_sf(data = huc10, fill = 'black') +
  geom_sf(data = sthd_qrf_stream, aes(colour = sum_parr)) +
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "Summer Steelhead",
       subtitle = "Summer Capacity"
       ) +
  theme_void() +
  theme(legend.position = 'none')


sthd_qrf_winter_fig <- ggplot() +
  geom_sf(data = huc10, fill = 'black') +
  geom_sf(data = sthd_qrf_stream, aes(colour = win_parr)) +
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  labs(subtitle = "Winter Capacity",
       caption = 'Warmer colors have a higher predicted fish density based on geomorphic characteristics.') +
  theme_void() +
  theme(legend.position = 'none')

qrf_stream_fig <- (chnk_qrf_sum_fig + chnk_qrf_winter_fig) / (sthd_qrf_sum_fig + sthd_qrf_winter_fig) +
  theme(legend.position='none')

ggsave('./figures/qrf_stream_capacity.png', qrf_stream_fig, width = 11, height = 8.5)


# Summarize stream info into polygons----

# load population polygons
spsm_pops <- st_read(paste0(spatial_files,'Snake River Extant SpSm Chinook POP/Snake River Extant SpSm Chinook POP.shp')) %>% 
  st_transform(crs = crs_default)

sthd_pops <- st_read(paste0(spatial_files,'Snake River Extant Steelhead POP/Snake River Extant Steelhead POP.shp')) %>%
  st_transform(crs = crs_default)

fall_pops <- st_read(paste0(spatial_files,'Snake River Extant Fall Chinook POP/Snake River Extant Fall Chinook POP.shp')) %>%
  st_transform(crs = crs_default)



# combine populations to each huc10
huc10_pnt <- st_point_on_surface(huc10)

huc10_pops <- huc10_pnt %>%
  st_join(spsm_pops %>%
            select(spsm_MPG = MPG, spsm_POP_NAME = POP_NAME, spsm_TRT_POPID = TRT_POPID)) %>%
  st_join(sthd_pops %>%
            select(sthd_MPG = MPG, sthd_POP_NAME = POP_NAME, sthd_TRT_POPID = TRT_POPID)) %>%
  st_join(fall_pops %>%
            select(fall_MPG = MPG, fall_POP_NAME = POP_NAME, fall_TRT_POPID = TRT_POPID)) %>%
  st_set_geometry(NULL) %>%
  select(-HU_10_NAME)

huc10 <- inner_join(huc10, huc10_pops, by = 'HUC_10')



# metric function----
calc_weighted_value <- function(length, value){
  weighted_value <- sum(value * length)/sum(length)
  #weighted_value <- sum(value)
  return(weighted_value)
}

huc10_spsm <- full_join(
  qrf %>%
    st_set_geometry(NULL) %>%
    filter(chnk == 1) %>%
    group_by(HUC_10) %>%
    summarise(spsm_sm = calc_weighted_value(length = reach_leng, value = chnk_sm),
              spsm_sm2 = calc_weighted_value(length = reach_leng, value = chnk_sm2),
              spsm_w = calc_weighted_value(length = reach_leng, value = chnk_w),
              spsm_w2 = calc_weighted_value(length = reach_leng, value = chnk_w2),
              spsm_r = calc_weighted_value(length = reach_leng, value = chnk_r),
              spsm_r2 = calc_weighted_value(length = reach_leng, value = chnk_r2)),
  ip %>%
    st_set_geometry(NULL) %>%
    group_by(HUC_10) %>%
    summarise(spsm_ip =  calc_weighted_value(length = LENGTH, value = CHINRATE)),
  by = c('HUC_10')
)


huc10_sthd <- full_join(
  qrf %>%
    st_set_geometry(NULL) %>%
    filter(sthd == 1) %>%
    group_by(HUC_10) %>%
    summarise(sthd_sm = calc_weighted_value(length = reach_leng, value = sthd_sm),
              sthd_sm2 = calc_weighted_value(length = reach_leng, value = sthd_sm2),
              sthd_w = calc_weighted_value(length = reach_leng, value = sthd_w),
              sthd_w2 = calc_weighted_value(length = reach_leng, value = sthd_w2),
              sthd_r = calc_weighted_value(length = reach_leng, value = sthd_r),
              sthd_r2 = calc_weighted_value(length = reach_leng, value = sthd_r2)),
  ip %>%
    st_set_geometry(NULL) %>%
    group_by(HUC_10) %>%
    summarise(sthd_ip = calc_weighted_value(length = LENGTH, value = STHDRATE)),
  by = c('HUC_10')
)

huc10_metrics <- huc10 %>%
  left_join(huc10_spsm,
            by = c('HUC_10')) %>% 
  left_join(huc10_sthd,
            by = c('HUC_10'))


# summarized chinook plots
chnk_ip_fig <- ggplot() +
  geom_sf(data = huc10_metrics, aes(fill = spsm_ip)) +
  #scale_fill_viridis_c(option = 'mako', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu') +
  theme_void()

chnk_ip_fig

chnk_redds_fig <- ggplot() +
  geom_sf(data = huc10_metrics, aes(fill = spsm_r)) +
  #scale_fill_viridis_c(option = 'mako', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu') +
  theme_void()

chnk_redds_fig

chnk_sm_fig <-ggplot() +
  geom_sf(data = huc10_metrics, aes(fill = spsm_sm)) +
  #scale_fill_viridis_c(option = 'mako', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu') +
  theme_void()

chnk_sm_fig

chnk_w_fig <- ggplot() +
  geom_sf(data = huc10_metrics, aes(fill = spsm_w)) +
  #scale_fill_viridis_c(option = 'mako', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu') +
  theme_void()

chnk_w_fig

combined_spsm_plot <- (chnk_ip_fig + chnk_redds_fig) / (chnk_sm_fig + chnk_w_fig) +
  plot_annotation('Sp/sm Chinook Salmon', caption = 'NOAA IP and See et al. QRF',theme=theme(plot.title=element_text(hjust=0.5)))

ggsave('./figures/spsm_potential.png', combined_spsm_plot, width = 11, height = 8.5)


sthd_ip_fig <- ggplot() +
  geom_sf(data = huc10_metrics, aes(fill = sthd_ip)) +
  scale_fill_viridis_c(option = 'mako', direction = -1) +
  theme_void()

sthd_redds_fig <- ggplot() +
  geom_sf(data = huc10_metrics, aes(fill = sthd_r)) +
  scale_fill_viridis_c(option = 'mako', direction = -1) +
  theme_void()

sthd_sm_fig <-ggplot() +
  geom_sf(data = huc10_metrics, aes(fill = sthd_sm)) +
  scale_fill_viridis_c(option = 'mako', direction = -1) +
  theme_void()

sthd_w_fig <- ggplot() +
  geom_sf(data = huc10_metrics, aes(fill = sthd_w)) +
  scale_fill_viridis_c(option = 'mako', direction = -1) +
  theme_void()


combined_sthd_plot <- (sthd_ip_fig + sthd_redds_fig) / (sthd_sm_fig + sthd_w_fig) +
  plot_annotation('Summer Steelhead', caption = 'NOAA IP and See et al. QRF',theme=theme(plot.title=element_text(hjust=0.5)))

ggsave('./figures/sthd_potential.png', combined_sthd_plot, width = 11, height = 8.5)


tmp <- huc10_metrics %>%
  st_set_geometry(NULL) %>%
  mutate(pop_huc = case_when(
    !is.na(spsm_MPG) ~ TRUE,
    !is.na(sthd_MPG) ~ TRUE,
    !is.na(fall_MPG) ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  filter(pop_huc) %>%
  select(-pop_huc)

write_csv(tmp,
          file = './data/huc10_metrics.csv')

st_write(huc10_metrics, paste0(spatial_files,'HUC_10_TRT/HUC_10_snake_river_summaries.shp'), append = FALSE)