#' Title Stochastic life-cycle model to predict returns.
#'
#' Author Ryan Kinzer
#' Created March 8, 2024
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 
#' 
#' 
#' 
#' 
life_cycle <- function(){
  
  parr_s <- .065
  smolt_s <- .3
  hydro_s <- .6
  ocean_s <- .05
  adult_s <- .90
  spawn_s <- .95
  fec <- 4000
  sex_p <- .50
  
  n = 50 # spawners
  f = rbinom(1, n, sex_p) # female spawners
  eggs <- round(sum(rnorm(f, fec, sd = 250))) # deposited eggs
  parr <- rbinom(1, eggs, parr_s) # parr leaving the tributary
  smolt_lgr <- rbinom(1, parr, smolt_s) # smolt at LGR
  smolt_bon <- rbinom(1, smolt_lgr, hydro_s) # smolt at bon
  adults_bon <- rbinom(1, smolt_bon, ocean_s)
  adults_lgr <- rbinom(1, adults_bon, adult_s)
  adults_trib <- rbinom(1, adults_lgr, spawn_s)
  

  adults_trib/n # P:P
  adults_trib/parr # tributary to tributary
  adults_lgr/smolt_lgr# LGR to LGR
  adults_bon/smolt_bon
  
    
}