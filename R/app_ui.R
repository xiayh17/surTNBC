#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    # List the first level UI elements here 
    fluidPage(
      # Your application UI logic
      bs4Dash::dashboardPage(
        dark = FALSE,
        #freshTheme = dark_theme,
        title = "eSetAnno",
        header = bs4Dash::dashboardHeader(
          title = bs4Dash::dashboardBrand(
            title = "eSetAnno",
            color = "primary",
            href = "http://www.biotrainee.com/",
            image = "https://cdn.jsdelivr.net/gh/xiayh17/Figs@main/uPic/Snipaste_2021-03-02_20-02-43.png",
          )
        ),
        sidebar = bs4Dash::dashboardSidebar(
          bs4Dash::sidebarMenu(
            bs4Dash::menuItem("Search", tabName = "search",icon = icon('search')),
            bs4Dash::menuItem("Diff Boxplot", tabName = "diff",icon = icon('boxes')),
            bs4Dash::menuItem("Survival analysis", tabName = "sur",icon = icon('chart-line'))
            # bs4Dash::menuItem("Differentially Expressed Genes", tabName = "deg",icon = icon('dna')),
            # bs4Dash::menuItem("Gene Set Annotation", tabName = "gsaanno",icon = icon('random')),
            # bs4Dash::menuItem("Gene Set Enrichment Analysis", tabName = "gsea",icon = icon('globe-americas')),
            # bs4Dash::menuItem("Analysis Report", tabName = "report",icon = icon('file-contract'))
          )
        ),
        #controlbar = bs4Dash::dashboardControlbar(),
        #footer = bs4Dash::dashboardFooter(),
        body = bs4Dash::dashboardBody(
          bs4Dash::tabItems(
            bs4Dash::tabItem(tabName = "search",mod_search_ui("search_ui_1")),
            bs4Dash::tabItem(tabName = "diff",mod_Boxplot_of_Diff_ui("Boxplot_of_Diff_ui_1")),
            bs4Dash::tabItem(tabName = "sur",mod_Survival_analysis_ui("Survival_analysis_ui_1"))
            # bs4Dash::tabItem(tabName = "deg",mod_deg_ui("deg_ui_1")),
            # bs4Dash::tabItem(tabName = "gsaanno",mod_gsaanno_ui("gsaanno_ui_1")),
            # bs4Dash::tabItem(tabName = "gsea",mod_gsea_ui("gsea_ui_1")),
            # bs4Dash::tabItem(tabName = "report",mod_report_ui("report_ui_1"))
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'surTNBC'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

