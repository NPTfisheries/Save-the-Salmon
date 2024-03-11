sim_life_cycle <- function(
    iterations,
    num_generations,
    spawners, sex_p, fecundity, parr_s, smolt_s, hydro_s, ocean_s, adult_s, spawn_s
){
  
  
  
  all_results <- vector("list", iterations)
  
  for (i in 1:iterations){
    
    # Initialize a list to store results
    results_list <- vector("list", num_generations)
    
    # Initial population
    results_list[[1]] <- life_cycle(spawners, sex_p, fecundity, parr_s, smolt_s, hydro_s, ocean_s, adult_s, spawn_s)
    results_list[[1]]$generation <- 1
    
    # Iterate over generations
    for (j in 2:num_generations) {
      # Use the results from the previous iteration as inputs
      prev_results <- results_list[[j - 1]]
      results_list[[j]] <- life_cycle(prev_results$adults_trib, sex_p, fecundity, parr_s, smolt_s, hydro_s, ocean_s, adult_s, spawn_s)
      results_list[[j]]$generation <- j
    }
    
    all_results[[i]] <- results_list
    
  }
  
  # combine results and name iterations
  dat <- map(seq_len(iterations), 
             ~all_results[[.x]] %>% 
               bind_rows()
  ) %>%
    bind_rows(.id = 'iteration') %>%
    select(iteration, generation, everything())
  
  return(dat)
    
}
  
