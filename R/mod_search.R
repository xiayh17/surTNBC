#' search UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_search_ui <- function(id){
  ns <- NS(id)
  tagList(
    # hiden
    shinyjs::useShinyjs(),
    helpText("This step help you filter Database by Gene"),
    hr(),
    selectizeInput(ns("search"), 
                   "Search by Gene ID",
                   choices = NULL,
                   selected = character(0), 
                   multiple = FALSE, 
                   options = list(
      placeholder = 'Search by Gene ID,e.g: NT5DC2',
      onInitialize = I('function() { this.setValue(""); }'),
      maxOptions = 200
    )),
    shinyWidgets::actionBttn(
      inputId = ns("start"),
      label = "Search", 
      style = "stretch",
      color = "success"
    ),
    hr(),
    htmlOutput(ns("timer")),
    hr(),
    shinyWidgets::prettySwitch(
      inputId = ns("sim"),
      label = "Simple name", 
      value = TRUE,
      status = "success",
      slim = TRUE
    ),
    shinyjs::hidden(div(id = ns('loading2'),
                        downloadButton(ns("downloadgeneInfo"), "Download Gene Info"))),
    br(),
    shinyjs::hidden(div(id = ns('loading'),
                        shinycssloaders::withSpinner(
                          DT::DTOutput(ns("geneInfo")),
                          type = 6
                        )))

  )
}
    
#' search Server Functions
#'
#' @noRd 
mod_search_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    updateSelectizeInput(session, 'search', choices = namelist$F, selected = character(0), server = TRUE)
    
    data_sql <- reactive({
      # connect to database
      DBI::dbConnect(RSQLite::SQLite(), "inst/app/data/portal-database-output.sqlite")
    })
    
    keyword <- reactive({
      input$start
      isolate(input$search)
    })
    mydat <- reactive({
      con <- data_sql()
      keyword <- keyword()
      tm <- system.time({
        fdata <- dplyr::tbl(con, dbplyr::sql(paste0("SELECT * FROM iso_tpm2 WHERE names LIKE '%|", keyword,"|%'"))) %>%
          dplyr::collect() %>%
          tibble::remove_rownames() %>% tibble::column_to_rownames(var="names")
      })
      list(x=fdata,elapsed = tm['elapsed'])
    })
    
    fdata <- reactive({
      mydat()$x
    })
    
    resdata <- reactive({
      data <- fdata()
      
      if (input$sim) {
        rownames(data) <- gsub('\\|.*','',rownames(data))
        data
      } else {
        data
      }
      
    })
    
    observeEvent(input$start, {
      shinyjs::show(id = 'loading')
      
      
      # describe data dimension
      output$timer <- renderText({
        paste0("<font color=\"#0275D8\"><b>", round(mydat()$elapsed*1000), "</b></font>",
               " milliseconds" )
      })
      
      
      
      tar <- reactive({
        ncol(resdata())
      })
      output$geneInfo <- DT::renderDT(
        #output$preview3 <- reactable::renderReactable({
        DT::datatable( resdata(), escape = FALSE, selection="multiple",
                       rownames = TRUE,
                       style = "bootstrap4",
                       extensions = 'Buttons',
                       options=list(
                         #sDom  = '<"top">flrt<"bottom">ip',
                         #columnDefs = list(list(className = 'dt-center', targets = 5)),
                         pageLength = -1,
                         #lengthMenu = list(c(15, 50, 100, -1),c(15, 50, 100, "ALL")),
                         dom = 'Bfrtip',
                         columnDefs = list(list(
                           targets = c(10:(tar()-1)), visible = FALSE
                         )),
                         buttons = list(list(extend ='collection',
                                             buttons =  c('csv', 'excel', 'pdf'),text = 'Download View'
                         ),
                         list(extend ='colvis',text = 'Hide Columns')),
                         scrollX = TRUE,
                         scrollY = TRUE,
                         fixedColumns = TRUE,
                         fixedHeader = TRUE
                       )
        )
      )
      shinyjs::show(id = 'loading2')
      
      reactive({
        DBI::dbDisconnect(data_sql())
      })
      
    })
    
    output$downloadgeneInfo <- downloadHandler(
      filename = function() {
        paste("GTmatirx", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(resdata(), file)
      }
    )

    return(
      list(
        resdata = reactive({
          resdata()
        })
      ))
    
  })
}
    
## To be copied in the UI
# mod_search_ui("search_ui_1")
    
## To be copied in the server
# mod_search_server("search_ui_1")
