#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  matdata <- mod_search_server("search_ui_1")
  mod_Boxplot_of_Diff_server("Boxplot_of_Diff_ui_1", matdata = matdata)
  mod_Survival_analysis_server("Survival_analysis_ui_1", matdata = matdata)
}
