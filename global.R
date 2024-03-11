library(shiny)
library(shinydashboard)
library(tidyverse)

source('./R/mod_population.R')
source('./R/tmp_func.R')

# initial vars // for testing at least
parr_s <- .065
smolt_s <- .3
hydro_s <- .6
ocean_s <- .05
adult_s <- .90
spawn_s <- .95
fec <- 4000
sex_p <- .50


populations <- c('Lolo Creek', 'Johnson Creek')