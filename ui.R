# UI
dashboardPage(
  dashboardHeader(
    title='Save the Salmon'
  ),
  dashboardSidebar(disable = T),
  dashboardBody(
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
)