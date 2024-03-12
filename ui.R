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
  dashboardSidebar(disable = T),
  dashboardBody(
    includeCSS('./www/styles.css'),
    fluidRow(
      column(2, offset=5, actionButton(inputId='build_lgr_plot', label='Update LGR Plot')),
      br(),
      column(10, offset=1, plotOutput(outputId='lgr_plot'))),
    br(),
    fluidRow(
      uiOutput(outputId='pop_cards')
    )
  )
)