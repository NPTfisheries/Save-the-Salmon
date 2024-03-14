#' Placeholder function
#'
#' Author Tyler Stright
#' Created March 11, 2024
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 
#' 
#' 
#' 
#' 
tmp_func <- function(
  parr_s,
  smolt_s,
  hydro_s,
  ocean_s,
  adult_s,
  spawn_s,
  fec,
  sex_p
){
  
  # test values
  # parr_s <- .065
  # smolt_s <- .3
  # hydro_s <- .6
  # ocean_s <- .05
  # adult_s <- .90
  # spawn_s <- .95
  # fec <- 4000
  # sex_p <- .50
  
  parr <- fec * parr_s 
  # parr
  
  lgr_adults <- parr * smolt_s * hydro_s * ocean_s * adult_s
  # lgr_adults
  
  sar = lgr_adults/parr
  
  return(sar)
}