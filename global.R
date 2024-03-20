library(shiny)
library(shinydashboard)
library(leaflet)
library(leafpop)
library(tidyverse)

source('./R/mod_population.R')
source('./R/life_cycle.R')
source('./R/sim_life_cycle.R')
source('./R/tmp_func.R')

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
