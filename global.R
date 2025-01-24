library(shiny)
library(shinydashboard)
library(leaflet)
library(leafpop)
library(tidyverse)
library(sf)

source('./R/mod_population.R')
source('./R/life_cycle.R')
source('./R/sim_life_cycle.R')
source('./R/tmp_func.R')
source('./R/mapFun.R')

# load points, rivers and polygons
#default_crs <- 4326

icc <- readRDS(here::here("data", "map_layers", "raw", "icc.rds"))

huc10_metrics <- readRDS(here::here("data", "map_layers", "summarized", "huc10_metrics.rds"))

spsm_pop <- readRDS(here::here("data", "map_layers", "raw", "spsm_pops.rds"))

sthd_pop <- readRDS(here::here("data", "map_layers", "raw", "sthd_pops.rds"))
                            
# debug
# leaflet() %>%
#   #addTiles() %>%
#   setView(lng = -115.5660,
#           lat = 45.4000,#44.9218,
#           zoom = 7) %>%
#   addProviderTiles(providers$Esri.WorldTopoMap) %>%
#   addPolygons(data = huc10_metrics,
#               group = 'Population Boundaries',
#               label = ~HU_10_NAME,
#               #layerId = ~HU_10_NAME,
#               #options = leafletOptions(pane = "pop_labels"),
#               fillColor = 'blue',
#               color = 'black',
#               opacity = 1
#   )


# initial vars // for testing at least
spawners <- 50
fec <- 4000
sex_p <- .5
parr_s <- .065
smolt_s <- .3
hydro_s <- .6
ocean_s <- .05
adult_s <- .90
spawn_s <- .95

# alphabetic pops
populations <- sort(c('Lolo Creek', 'Johnson Creek', 'Lostine River', 'Imnaha River', 'South Fork Clearwater River'))


# Function to create list of population graphs from QET data
# popGraph<- function(id){
#   
#   popFiltered <- pop_est %>%
#     #filter(TRT_POPID == 'SFSEC-s') %>%
#     filter(TRT_POPID == id) #%>%
#   #mutate(year5 = zoo::rollmean(estimate, 2, fill = NA))
#   
#   yint <- popFiltered %>%
#     arrange(TRT_POPID, desc(spawn_year)) %>%
#     group_by(TRT_POPID) %>%
#     mutate(rank = 1:n(),
#            n = n()) %>%
#     filter(rank <= 5) %>%
#     group_by(TRT_POPID) %>%
#     summarise(avg5 = round(mean(estimate))) %>%
#     ungroup() %>%
#     pull(avg5)
#   
#   p <- ggplot(popFiltered, aes(x = spawn_year)) +
#     geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = .5) +
#     geom_line(aes(y = estimate)) +
#     #geom_line(aes(y = year5)) +
#     geom_hline(yintercept = yint, linetype = 2, colour = 'blue') +
#     # stat_summary(fun.y = mean, geom = 'line', aes(group = 1)) +
#     geom_point(aes(y = estimate)) +
#     scale_x_continuous(breaks = scales::pretty_breaks()) +
#     theme_bw() +
#     labs(title = paste0(id,': last five-year average = ',yint),
#          x = 'Spawn Year',
#          y = 'Estimate')
#   
#   return(p)
# }