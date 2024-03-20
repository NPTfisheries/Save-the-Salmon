#' Title Script to generate fish returns.
#'
#' Author Ryan Kinzer
#' Created March 8, 2024

library(tidyverse)
source(here::here('R', 'life_cycle.R'))
source(here::here('R', 'sim_life_cycle.R'))

spawners = 50
sex_p <- .50
fecundity <- 4000
parr_s <- .9 #.065
smolt_s <- .3
hydro_s <- .6
ocean_s <- .05
adult_s <- .90
spawn_s <- .95


iterations = 5
num_generations <- 5

system.time(
  all_results <- sim_life_cycle(
    iterations,
    num_generations,
    spawners, sex_p, fecundity, parr_s, smolt_s, hydro_s, ocean_s, adult_s, spawn_s
  )
)



# combine results and name iterations
dat <- map(seq_len(iterations), 
           ~all_results[[.x]] %>% 
             bind_rows()
) %>%
  bind_rows(.id = 'iteration') %>%
  select(iteration, generation, everything())


ggplot(dat, aes(x = generation, y = adults_trib)) +
  geom_point(alpha = .25) +
  geom_line(aes(group = as.factor(iteration))) +
  geom_smooth() +
  NULL



# 'pp' = adults_trib/spawners,  # P:P
# 'sar_trib' = adults_trib/parr, # tributary to tributary
# 'sar_lgr' = adults_lgr/smolt_lgr, # LGR to LGR
# 'sar_bon' = adults_bon/smolt_bon


test_fun <- function(x){
  eggs <- round(sum(rnorm(50, 4000, sd = 250))) # deposited eggs
  parr <- rbinom(1, eggs, x) # parr leaving the tributary
  smolt_lgr <- rbinom(1, parr, .3) # smolt at LGR
  smolt_bon <- rbinom(1, smolt_lgr, .6) # smolt at bon
  adults_bon <- rbinom(1, smolt_bon, .05)
  adults_lgr <- rbinom(1, adults_bon, .9)
  adults_trib <- rbinom(1, adults_lgr, .95)
  
  return(adults_trib)
}


vals <- seq(.1, .9, .1)
for(val in vals) {
  tm <- system.time(
    test_fun(val)
  )
  cat('testing', val, ": ", tm['elapsed'], '\n')
}
