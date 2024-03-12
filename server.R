# SERVER
shinyServer(function(input, output) {
  
  # dynamically build the UI based on pops.
  output$pop_cards <- renderUI({
    tagList(
      lapply(populations, function(pop) {
        popUI(pop,
              .spawners = spawners,
              .fec = fec,
              .sex_p = sex_p,
              .parr_s = parr_s,
              .smolt_s = smolt_s,
              .hydro_s = hydro_s,
              .ocean_s = ocean_s,
              .adult_s = adult_s,
              .spawn_s = spawn_s)
      }) 
    )
  }) # renderUI
  
  # dynamically build all server module instances; pass data to pop_data_list 
  pop_data_list <- lapply(populations, function(pop) popServer(id = pop))
  
  # I think it's best to not have this totally reactive.
  observeEvent(input$build_lgr_plot, {
    
    # combine data from every popServer instance
    all_dat <- bind_rows(lapply(pop_data_list, function(dat) dat()))
    
    # lgr_df <- bind_rows(lolo_dat(), jc_dat()) %>% 
    lgr_df <- all_dat %>%
      group_by(iteration, generation) %>%
      summarize(sum = sum(adults_lgr)) %>%
      group_by(generation) %>%
      summarize(median_adults_lgr = median(sum))
    
    output$lgr_plot <- renderPlot({
      ggplot(lgr_df, aes(x = generation, y = median_adults_lgr)) +
        geom_point(alpha = .25) +
        geom_smooth()
    })
    
  }) # observeEvent
  
  
}) # server 