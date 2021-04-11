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
          column(12,hr()),
          column(12,htmlOutput(ns("count"))),
          column(12,hr()),
          column(12,
                 shinyWidgets::prettyRadioButtons(
                   inputId = ns("methods"),
                   label = "Choose a Group Methods: ",
                   choices = c("Auto" = "Auto", "Median" = "Median", "Trisection" = "Tri"),
                   icon = icon("eye-dropper"),
                   bigger = TRUE,
                   inline = TRUE,
                   fill = TRUE,
                   plain = TRUE,
                   status = "success",
                   animation = "pulse"
                 )
                 ),
          column(6,textOutput(ns("fail"))),
          column(6,br()),
          column(12,hr()),
          column(12,helpText("Group result")),
          column(12,hr()),
          column(12,DT::DTOutput(ns("table")))
        )
      ),
      tabPanel(
        "Plot One by One",
        icon = icon("drafting-compass"),
        fluidRow(
          # column(12,uiOutput(outputId = ns('tt'))),
          column(12,hr()),
          column(12,uiOutput(outputId = ns('ss'))),
          column(12,hr()),
          column(12,
                 shinycssloaders::withSpinner(
                   plotOutput(ns("splot")),
                   type = 6
                 )
                 )
        )
      ),
      tabPanel(
        "Plot All in Onetime",
        icon = icon("layer-group"),
        fluidRow(
          column(12,hr()),
          helpText("Please click button to download plot"),
          column(12,hr()),
          downloadButton(ns('dallPlot'), 'Download plot as PDF'),
          column(12,hr()),
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
      paste("There will be ",
            "<font color=\"#0275D8\"><b>",count()[1], "</b></font>",
            " tumor samples contained in Survival analysis")
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
      if (input$methods == "Auto") {
        phe.cut <- survminer::surv_cutpoint(
          phe2,
          time = "RFS_time_Months",
          event = "RFS_Status",
          variables = variables,
          minprop = 0.0001
        )
        survminer::surv_categorize(phe.cut)
      } else if (input$methods == "Median") {
        median.cut(
          data = phe2,
          time = "RFS_time_Months",
          event = "RFS_Status",
          variables = variables
        )
      } else if (input$methods == "Tri") {
        tri.cut(
          data = phe2,
          time = "RFS_time_Months",
          event = "RFS_Status",
          variables = variables
        )
      }
    })
    
    tmp.cat2 <- reactive({
      tmp.cat <- tmp.cat()
      if (class(tmp.cat) == "list") {
        tmp.cat[[1]]
      } else {
        tmp.cat
      }
    })
    
    output$ss <- renderUI({
      shinyWidgets::pickerInput(
        inputId = ns("plotw"),
        label = "Quick search", 
        choices = colnames(tmp.cat2())[c(-1,-2)],
        options = list(
          `live-search` = TRUE)
      )
    })
    
    wr <- function(data) {
      tmp.cat <- tmp.cat()
      if (class(tmp.cat) == "list") {
        if (length(tmp.cat[[2]]) > 0) {
          b <- paste(tmp.cat[[2]], collapse="; ")
          paste0(b, " are not included for lacking of sufficient number of values; ", length(colnames(tmp.cat2())[c(-1,-2)]), " samples were grouped.")
        } else {
          paste0(length(colnames(tmp.cat2())[c(-1,-2)]), " samples were grouped.")
        }
      } else {
        paste0(length(colnames(tmp.cat2())[c(-1,-2)]), " samples were grouped.")
      }
    }
    
    
      output$fail <- renderText({
        print(wr())
      })
    
    fits <- reactive({
      tmp.cat2 <- tmp.cat2()
      # lapply
      formulae <- list()
      genes <- colnames(tmp.cat2)[c(-1,-2)]
      formulae <- lapply(genes, function(x) as.formula(paste0("survival::Surv(RFS_time_Months, RFS_Status) ~ ", x)))
      
      require(survminer)
      surv_fit(formulae, data = tmp.cat2)
    })
    
    output$table <- DT::renderDT(
      #output$preview3 <- reactable::renderReactable({
      DT::datatable( tmp.cat2(), escape = FALSE, selection="multiple",
                     rownames = FALSE,
                     style = "bootstrap4",
                     extensions = 'Buttons',
                     options=list(
                       #sDom  = '<"top">flrt<"bottom">ip',
                       #columnDefs = list(list(className = 'dt-center', targets = 5)),
                       pageLength = -1,
                       #lengthMenu = list(c(15, 50, 100, -1),c(15, 50, 100, "ALL")),
                       dom = 'Bfrtip',
                       # columnDefs = list(list(
                       #   targets = c(10:(tar()-1)), visible = FALSE
                       # )),
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
    
    plotsf <- function() {
      fits <- fits()
      tmp.cat2 <- tmp.cat2()
      survminer::ggsurvplot_list(fits, data = tmp.cat2,
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
    
    # output$tt <- renderUI({
    #   shinyWidgets::sliderTextInput(
    #     inputId = ns("plotw"),
    #     label = "Choose one to plot:",
    #     grid = TRUE,
    #     width = "100%",
    #     choices = rownames(mat())
    #   )
    # })
    
    # observeEvent(input$plotw,{
    #   output$ss <- renderUI({
    #     shinyWidgets::pickerInput(
    #       inputId = ns("picker"),
    #       label = "Quick search", 
    #       choices = rownames(mat()),
    #       selected = input$plotw,
    #       options = list(
    #         `live-search` = TRUE)
    #     )
    #   })
    # })
    # 
    # observeEvent(input$picker,{
    #   output$tt <- renderUI({
    #     shinyWidgets::sliderTextInput(
    #       inputId = ns("plotw"),
    #       label = "Choose one to plot:",
    #       grid = TRUE,
    #       width = "100%",
    #       selected = input$picker,
    #       choices = rownames(mat())
    #     )
    #   })
    # })
    
    output$splot <- renderPlot(
      width = 1200,
      height = 600,
      res = 100,
      {
      plots()[[paste0("tmp.cat2::",input$plotw)]]
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
