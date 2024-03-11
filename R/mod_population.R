# module UI
popUI <- function(id, 
                  .spawners,
                  .fec,
                  .sex_p,
                  .parr_s, 
                  .smolt_s,
                  .hydro_s,
                  .ocean_s,
                  .adult_s,
                  .spawn_s
) {
  ns <- NS(id)
  
  tagList(
    column(width=3, 
           box(title = id, width = 12, solidHeader = TRUE, status = 'info',
               # collapsible parameter box
               box(title = paste(id, 'Parameters'), solidHeader = FALSE, status = NULL,
                   width = 12, collapsible = TRUE, collapsed = TRUE,
                   numericInput(inputId = ns('spawners'),
                                label='Spawners',
                                value = .spawners),
                   numericInput(inputId = ns('fec'),
                                label='Fecundity',
                                value = .fec),
                   sliderInput(inputId = ns('sex_p'),
                               label='Sex Proportion',
                               min = 0, max = 1, step = 0.01,
                               value = .sex_p),
                   sliderInput(inputId = ns('parr_s'),
                               label='Parr Survival',
                               min = 0, max = 1, step = 0.01,
                               value = .parr_s),
                   sliderInput(inputId = ns('smolt_s'),
                               label='Smolt Survival',
                               min = 0, max = 1, step = 0.01,
                               value = .smolt_s),
                   sliderInput(inputId = ns('hydro_s'),
                               label='Hydrosystem Survival',
                               min = 0, max = 1, step = 0.01,
                               value = .hydro_s),
                   sliderInput(inputId = ns('ocean_s'),
                               label='Ocean Survival',
                               min = 0, max = 1, step = 0.01,
                               value = .ocean_s),
                   sliderInput(inputId = ns('adult_s'),
                               label='Adult Survival',
                               min = 0, max = 1, step = 0.01,
                               value = .adult_s),
                   sliderInput(inputId = ns('spawn_s'),
                               label='Spawning Survival',
                               min = 0, max = 1, step = 0.01,
                               value = .spawn_s),
                   actionButton(inputId = ns('recalculate'),
                                label='Recalculate')
               ),
               # outputs
               # valueBoxOutput(ns("sar_est"), width = 12),
               column(12, 
                      plotOutput(outputId = ns('pop_plot'))
               )
           )
    )
  )
}

# module server
popServer <- function(id) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
      # estimate <- reactive({
      #   result <- tmp_func(
      #     parr_s = input$parr_s,
      #     smolt_s = input$smolt_s,
      #     hydro_s = input$hydro_s,
      #     ocean_s = input$ocean_s,
      #     adult_s = input$adult_s,
      #     spawn_s = input$spawn_s,
      #     fec = input$fec,
      #     sex_p = input$sex_p
      #   )
      #   return(result)
      # })
      
      # output$sar_est <- renderValueBox({
      #   valueBox(
      #     # value = paste0(as.character(round(estimate()*100,2)),'%'),
      #     value = nrow(pop_dat()),
      #     width = 12,
      #     color = 'fuchsia',
      #     subtitle = 'SAR Estimate'
      #   )
      # })
      
      
      # pop life cycle reactive data
      pop_dat <- eventReactive(input$recalculate, {
        
         sim_life_cycle(
          iterations = 10, 
          num_generations = 100,
          spawners = input$spawners,
          sex_p = input$sex_p,
          fecundity = input$fec, 
          parr_s = input$parr_s, 
          smolt_s = input$smolt_s, 
          hydro_s = input$hydro_s, 
          ocean_s = input$ocean_s, 
          adult_s = input$adult_s, 
          spawn_s = input$spawn_s
        )
        
      }, ignoreNULL = FALSE) #ignoreNULL = fire on startup
      
      output$pop_plot <- renderPlot({
        ggplot(pop_dat(), aes(x = generation, y = adults_trib)) +
          geom_point(alpha = .25) +
          geom_smooth()
      })
      
    } #function
  ) #moduleServer
}  
