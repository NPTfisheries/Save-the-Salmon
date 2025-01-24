# SERVER
shinyServer(function(input, output, session) {
  
  # showModal(modalDialog(
  #   title = "Caution!!",
  #   tags$div(
  #     "The simulation portion of this application is a work in progress. Please do not use their results at this time."
  #   )
  # ))
  
  # Create basemap ----
  # Basic NPT map area and Snake Basin
  
  output$map <- renderLeaflet({
    leaflet() %>%
      #addTiles() %>%
      setView(lng = -115.5660,
              lat = 45.4000,#44.9218,
              zoom = 7) %>%
      addProviderTiles(providers$Esri.WorldTopoMap)#%>%
      #addPolylines(data = pnw_rivers, color = 'blue', weight = 1) %>%
      #addPolylines(data = stream_layer, color = 'blue', weight = 1)
  })# Create basemap ----

  
  # proxy map----
  
  # Get map data ----
  
  toListen <- reactive({
    list(input$basin_spp,input$huc_metric)
  })
  
  
  map_data <- eventReactive(toListen(), {
    mapFun(spp = input$basin_spp, metric = input$huc_metric)
  })
  

  
  observeEvent(toListen(), {

    huc_col <- colorNumeric(palette = "plasma", domain = map_data()$huc10$metric)
    #pop_col <- colorFactor(palette = 'viridis', domain = map_data()$pop_layer$MPG)
    
    # pal <- colorFactor(
    #   palette = topo.colors(9)[1:6],
    #   domain = map_data()$site_est_layer$site_type_name
    # )
    
    leafletProxy('map', session) %>%
      clearGroup('HUC10 Metrics') %>%
      clearGroup('Population Boundaries') %>%
      clearGroup('ICC Boundary') %>%
      removeControl("legend") %>% 
      removeLayersControl() %>%
      addMapPane(name = "pop_labels", zIndex = 410) %>% 
      addMapPane(name = "huc_labels", zIndex = 420) %>% # higher zIndex rendered on top
      addPolygons(data = map_data()$huc10,
                  label = ~HU_10_NAME,
                  #layerId = ~HU_10_NAME,
                  group = 'HUC10 Metrics',
                  popup = paste("<b>Name:</b>",map_data()$huc10$HU_10_NAME,"</br>"),
                  labelOptions = labelOptions(noHide = F, textsize = 12),
                  options = leafletOptions(pane = "huc_labels"),
                  color = 'black',
                  opacity = 1,
                  fillColor = ~huc_col(metric), weight = 1, fillOpacity = .5) %>%
      addPolygons(data = map_data()$full_pop,
                  #label = ~POP_NAME,
                  #layerId = ~POP_NAME,
                  group = 'Population Boundaries',
                  #popup = popupGraph(map_data()$p_all),
                  #labelOptions = labelOptions(noHide = F, textsize = 12),
                  options = leafletOptions(pane = "pop_labels"),
                  fillColor = NA,
                  color = 'black',
                  opacity = 1
                  #fillColor = ~pop_copop(MPG), weight = 1, fillOpacity = .5
      ) %>%
      addPolygons(data = icc,
                  group = 'ICC Boundary',
                  fillColor = NA,
                  fillOpacity = 0,
                  color = 'blue',
                  opacity = 1
      ) %>%
      # addCircleMarkers(data = map_data()$site_est_layer, radius = 5,
      #                  group = 'Sites',
      #                  popup = map_data()$site_tab,
      #                  color = ~pal(site_type_name),
      #                  label = ~site_id,
      #                  layerId = ~site_id,
      #                  labelOptions = labelOptions(noHide = F),
      #                  popupOptions = popupOptions(noHide = F, minWidth = 400, maxWidth = 400)) %>%
      # addLegend(data = map_data()$site_est_layer, position = "bottomleft", pal = pal, values = ~site_type_name,
      #           title = "Observation Type",
      #           opacity = 1,
      #           layerId = 'legend') %>%
      addLegend(data = map_data()$huc10, position = "topleft", pal = huc_col, values = ~metric,
                title = input$huc_metric,
                opacity = .5,
                layerId = 'legend') %>%
      addLayersControl(
        #baseGroups = c("HUC10 Metrics"),
        overlayGroups = c("Population Boundaries", "ICC Boundary"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("Population Boundaries", "ICC Boundary"))
  })
  
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
        geom_smooth() +
        scale_y_continuous(breaks = seq(0, 1000, 50)) +
        scale_x_continuous(breaks = seq(0, 100, 5)) +
        theme_bw() +
        ggtitle('Estimated Adults to Lower Granite Dam') +
        xlab('Generation') +
        ylab('Median of Adults') +
        theme(
          panel.grid.minor = element_blank()
        )
    })
    
  }) # observeEvent
  
  
}) # server 