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
      menuItem("Snake Basin Map", tabName = 'map', icon = icon('map')),
      menuItem(
        "Adult Return Simulation",
        tabName = 'adult_return',
        icon = icon('th')
        )
      )
    ), 
  dashboardBody(
    includeCSS('./www/styles.css'),
    tabItems(
      tabItem(tabName = 'map',
              div(class="outer",
                  tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                  leafletOutput("map", width = "100%", height = "100%")
                  )
              ),
      tabItem(tabName = 'adult_return',
        fluidRow(
          column(2, offset=5, actionButton(inputId='build_lgr_plot', label='Update LGR Plot')),
          br(),
          column(10, offset=1, plotOutput(outputId='lgr_plot'))
          ),
        br(),
        fluidRow(
          uiOutput(outputId='pop_cards')
          )
        )
      ) #tabItems
    ) #dashboardBody
)