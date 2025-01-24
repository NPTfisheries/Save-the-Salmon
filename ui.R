# UI
dashboardPage(
  dashboardHeader(
    title='Save the Salmon',
    tags$li(a(href = 'http://www.nptfisheries.org',
              img(src = 'NPTlogos2.png',
                  title = 'Company Home', height = "46px"),
              style = "padding-top:2px; padding-bottom:2px;"),
            class = "dropdown")
    ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Snake Basin Map",
        tabName = 'map',
        icon = icon('map')), 
        radioButtons(
          'basin_spp',
          label = 'Species',
          choices = c('Steelhead', 'Sp/sm Chinook'),
          selected = 'Steelhead'
        ),
        # radioButtons(
        #   'layer',
        #   label = 'Layer',
        #   choices = c('Stream', 'Watershed'),
        #   selected = 'Stream'
        # ),
        radioButtons(
          'huc_metric',
          label = 'Metric',
          choices = c(
            'Intrinsic Potential',
            'Redd Capacity',
            'Summer Parr Capacity',
            'Overwintering Capacity'
          ),
          selected = 'Intrinsic Potential'
        )#,
      # menuItem(
      #   "In Progress - Simulation",
      #   tabName = 'adult_return',
      #   icon = icon('th')
      #   )
      )
    ), 
  dashboardBody(
    includeCSS('./www/styles.css'),
    tabItems(
      tabItem(tabName = 'map',
              div(class="outer",
                  tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 225px; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                  leafletOutput("map", width = "100%", height = "100%")
                  )
              ),
      tabItem(tabName = 'adult_return',
        fluidRow(
          column(width = 12,
                 box(solidHeader = TRUE, status='primary',
                     title = 'Total Lower Granite Escapement',
                     actionButton(inputId='build_lgr_plot', label='Update LGR Plot'),
                     br(),
                     plotOutput(outputId='lgr_plot')
                     )
          )
        ),
        br(),
        fluidRow(
          uiOutput(outputId='pop_cards')
          )
        )
      ) #tabItems
    ) #dashboardBody
)