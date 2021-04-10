#' Survival_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Survival_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    bs4Dash::tabsetPanel(
      id = NULL,
      tabPanel(
        "Data Prepareration for analysis",
        icon = icon("table"),
        fluidRow(
          column(12,textOutput(ns("count")))
        )
      ),
      tabPanel(
        "Plot One by One",
        icon = icon("drafting-compass"),
        fluidRow(
          column(12,uiOutput(outputId = ns('tt'))),
          column(12,uiOutput(outputId = ns('ss'))),
          column(12,plotOutput(ns("splot")))
        )
      ),
      tabPanel(
        "Plot All in Onetime",
        icon = icon("layer-group"),
        fluidRow(
          downloadButton(ns('dallPlot'), 'Download plot as PDF'),
          downloadButton(ns('sep.plot'), 'Download Seperate Plots')
        )
      )
    )
  )
}
    
#' Survival_analysis Server Functions
#'
#' @noRd 
mod_Survival_analysis_server <- function(id, matdata = ""){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    a <- reactive({
      matdata$resdata()
    })
    
    b=utils::read.table('inst/app/data/clinical.txt',header = T)
    
    kp<- reactive({
      a <- a()
      grepl('PT',colnames(a))
    })
      
    count <- reactive({
      table(kp())
    })
    
    output$count <- renderText({
      paste0("There will be ",count()[1],"tumor samples contained in Survival analysis")
    })
    
    mat <- reactive({
      a <- a()
      kp <- kp()
      mat <- a[,!kp]
      colnames(mat)=gsub('Lib_','',
                         gsub('_quant','',  gsub('.rep_quant','',  colnames(mat))))
      mat
    })
 
    phe <- reactive({
      mat <- mat()
      b[match(colnames(mat), b$Project_ID),]
    })
    
    phe2 <- reactive({
      mat <- mat()
      phe <- phe()
      tmat <- t(mat)
      cbind(phe,tmat)
    })
    
    
    tmp.cat <- reactive({
      phe2 <- phe2()
      variables <- colnames(phe2)[6:(5+ncol(phe2)-4)-1]
      phe.cut <- survminer::surv_cutpoint(
        phe2,
        time = "RFS_time_Months",
        event = "RFS_Status",
        variables = variables,
        minprop = 0.0001
      )
      survminer::surv_categorize(phe.cut)
    })
    
    fits <- reactive({
      phe2 <- phe2()
      tmp.cat <- tmp.cat()
      # lapply
      formulae <- list()
      genes <- colnames(phe2)[6:(5+ncol(phe2)-4)-1]
      formulae <- lapply(genes, function(x) as.formula(paste0("survival::Surv(RFS_time_Months, RFS_Status) ~ ", x)))
      
      require(survminer)
      surv_fit(formulae, data = tmp.cat)
    })
    
    plotsf <- function() {
      fits <- fits()
      tmp.cat <- tmp.cat()
      survminer::ggsurvplot_list(fits, data = tmp.cat,
                      surv.median.line = "hv", # Add medians survival
                      pval = TRUE,             # Add p-value and tervals 
                      conf.int = TRUE,        # Add the 95% confidence band
                      risk.table = TRUE,      # Add risk table
                      # title = "NT5DC2",
                      tables.height = 0.2,
                      tables.theme = theme_cleantable(),
                      palette = "jco",
                      ggtheme = theme_bw()
      )
    }
    
    plots <- reactive({
      plotsf()
    })
    
    allHeight <- reactive({
      a<-a()
      ceiling(nrow(a)/4)*300
    })
    
    output$tt <- renderUI({
      shinyWidgets::sliderTextInput(
        inputId = ns("plotw"),
        label = "Choose one to plot:",
        grid = TRUE,
        width = "100%",
        choices = rownames(mat())
      )
    })
    
    output$ss <- renderUI({
      shinyWidgets::pickerInput(
        inputId = ns("picker"),
        label = "Quick search", 
        choices = rownames(mat()),
        options = list(
          `live-search` = TRUE)
      )
    })
    
    observeEvent(input$plotw,{
      output$ss <- renderUI({
        shinyWidgets::pickerInput(
          inputId = ns("picker"),
          label = "Quick search", 
          choices = rownames(mat()),
          selected = input$plotw,
          options = list(
            `live-search` = TRUE)
        )
      })
    })
    
    observeEvent(input$picker,{
      output$tt <- renderUI({
        shinyWidgets::sliderTextInput(
          inputId = ns("plotw"),
          label = "Choose one to plot:",
          grid = TRUE,
          width = "100%",
          selected = input$picker,
          choices = rownames(mat())
        )
      })
    })
    
    output$splot <- renderPlot(
      width = 1200,
      height = 600,
      res = 100,
      {
      plots()[[paste0("tmp.cat::",input$plotw)]]
    })
    
    allplots <- function() {
      survminer::arrange_ggsurvplots(plots()[1:length(plots())], print = FALSE, ncol = 1, nrow = allHeight()/300*4, title = "Survival plots")
    }

    # Create the button to download the plot as PDF
    output$dallPlot <- downloadHandler(
      filename = function() {
        paste('allsurPlot_', Sys.Date(), '.pdf', sep='')
      },
      content = function(file) {
        ggplot2::ggsave(file, allplots(), width = 1200/100, height = allHeight()*8/100, dpi = 300, units = "in", limitsize = FALSE)
      }
    )
    
    allsepplot <- function() {
      p <- plots()
      `%dopar%` <- foreach::`%dopar%`
      require(foreach)
      foreach::foreach(i = 1:length(p)) %dopar% {
        ggsave(
          filename =paste(gsub(".*\\::","",names(p))[i],"-survival_curves.pdf",sep = ""),
          plot = print(p[i], newpage = FALSE),
          device = 'pdf',
          width = 8,
          height = 9
        )
        # ggsave(paste(names(p)[i],".pdf",sep = ""), plot)
      }
    }
    
    # Download the plots
    output$sep.plot = downloadHandler(
      filename = function() {
        paste('allsurPlot_Sep', Sys.Date(), '.zip', sep='')
      },
      content = function( file){
        
        # Set temporary working directory
        owd <- setwd( tempdir())
        on.exit( setwd( owd))
        
        # Save the histograms (a loop can be used here for a bunch of plots)
        
        try(allsepplot(), silent=TRUE)
        
        # Zip them up
        zip( file, list.files(pattern = "survival_curves\\.pdf$"))
      }
    )
  })
}
    
## To be copied in the UI
# mod_Survival_analysis_ui("Survival_analysis_ui_1")
    
## To be copied in the server
# mod_Survival_analysis_server("Survival_analysis_ui_1")
