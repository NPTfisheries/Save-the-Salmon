# UI
dashboardPage(
  dashboardHeader(
    title='Save the Salmon'
  ),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(
      popUI('Lolo Creek',
            .spawners = spawners,
            .fec = fec,
            .sex_p = sex_p,
            .parr_s = parr_s,
            .smolt_s = smolt_s,
            .hydro_s = hydro_s,
            .ocean_s = ocean_s,
            .adult_s = adult_s,
            .spawn_s = spawn_s),
      popUI('Johnson Creek',
            .spawners = spawners,
            .fec = fec,
            .sex_p = sex_p,
            .parr_s = parr_s,
            .smolt_s = smolt_s,
            .hydro_s = hydro_s,
            .ocean_s = ocean_s,
            .adult_s = adult_s,
            .spawn_s = spawn_s)
    )
  )
)