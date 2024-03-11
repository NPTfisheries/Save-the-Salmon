library(tidyverse)
source(here::here('R', 'life_cycle.R'))
source(here::here('R', 'sim_life_cycle.R'))

spawners = 50
sex_p <- .50
fecundity <- 4000
parr_s <- .065
smolt_s <- .3
hydro_s <- .6
ocean_s <- .05
adult_s <- .90
spawn_s <- .95


iter = 1000
num_generations <- 100


dat <- sim_life_cycle(
  
  iterations,
  num_generations,
  spawners, sex_p, fecundity, parr_s, smolt_s, hydro_s, ocean_s, adult_s, spawn_s
  
  
)
ggplot(dat, aes(x = generation, y = adults_trib)) +
  geom_point(alpha = .25) +
  #geom_line(aes(group = iter)) +
  geom_smooth()



# 'pp' = adults_trib/spawners,  # P:P
# 'sar_trib' = adults_trib/parr, # tributary to tributary
# 'sar_lgr' = adults_lgr/smolt_lgr, # LGR to LGR
# 'sar_bon' = adults_bon/smolt_bon