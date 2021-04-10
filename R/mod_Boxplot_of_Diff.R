#' Boxplot_of_Diff UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Boxplot_of_Diff_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    bs4Dash::tabsetPanel(
      id = NULL,
      tabPanel(
        "Plot and check one by one",
        icon = icon("spa"),
        fluidRow(
          column(12,hr()),
          column(12,uiOutput(outputId = ns('uu'))),
          column(12,uiOutput(outputId = ns('tt'))),
          column(12,hr()),
          column(6,plotOutput(ns("sinplot"))),
          column(6,DT::DTOutput(ns("table")))
        )
      ),
      tabPanel(
        "Plot All",
        icon = icon("autoprefixer"),
        hr(),
        downloadButton(ns('dallPlot'), 'Download plot as PDF'),
        downloadButton(ns('sep.plot'), 'Download Seperate Plots'),
        br(),
        hr(),
        shinyWidgets::actionBttn(
          inputId = ns("plotall"),
          label = "Plot all",
          style = "unite", 
          color = "success"
        ),
        hr(),
        plotOutput(ns("allplot"))
      )
    )
    
  )
}
    
#' Boxplot_of_Diff Server Functions
#'
#' @noRd 
#' @importFrom ggplot2 ggsave ggplot geom_boxplot aes ggtitle theme theme_bw scale_y_continuous scale_fill_manual element_text element_blank element_rect facet_wrap

mod_Boxplot_of_Diff_server <- function(id,matdata=""){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    a <- reactive({
      matdata$resdata()
    })
    
    allHeight <- reactive({
      a<-a()
      ceiling(dim(a)[1]/4)*300
    })
    
    output$uu <- renderUI({
      shinyWidgets::sliderTextInput(
        inputId = ns("choose"),
        label = "Choose one to plot:",
        grid = TRUE,
        width = "100%",
        choices = rownames(a())
      )
    })
    
    output$tt <- renderUI({
      shinyWidgets::pickerInput(
        inputId = "choosep",
        label = "Quick search", 
        choices = rownames(a()),
        options = list(
          `live-search` = TRUE)
      )
    })
    
    res <- reactive({
      a <- a()
      group_list=ifelse(grepl('PT',colnames(a)),'normal','tumor')
      ta <- t(a)
      ta <- tibble::as_tibble(ta)
      ta$group <- group_list
      a <- tibble::rownames_to_column(a, var = "group")
      res1 <- tidyr::gather(a, AA, value, 2:dim(a)[2], na.rm = T) #宽转长
      res <- res1[c(2,1,3)] #调换一列和二列的位置
      colnames(res) <- c("colname", "rowname", "value") #随意为三列命名
      res$group <- rep(group_list,each=dim(a)[1])
      data.table::setDT(res)
    })
    
    observeEvent(input$choose,{
      output$tt <- renderUI({
        shinyWidgets::pickerInput(
          inputId = ns("choosep"),
          label = "Quick search", 
          choices = rownames(a()),
          selected = input$choose,
          options = list(
            `live-search` = TRUE)
        )
      })
    })
    
    observeEvent(input$choosep,{
      output$uu <- renderUI({
        shinyWidgets::sliderTextInput(
          inputId = ns("choose"),
          label = "Choose one to plot:",
          grid = TRUE,
          width = "100%",
          selected = input$choosep,
          choices = rownames(a())
        )
      })
    })
    
    
    dat <- reactive({
      res <- res()
      res[which(res$rowname == input$choose),]
    })
    
    output$table <- DT::renderDT(
      #output$preview3 <- reactable::renderReactable({
      DT::datatable( dat(), escape = FALSE, selection="multiple",
                     rownames = TRUE,
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
    
    splot <- function() {
      dat <- dat()
      ggplot(dat, aes(x=group, y=value, fill = group)) +
        geom_boxplot(outlier.shape=19,lwd = 0.1,
                     outlier.size=0.3) + 
        scale_fill_manual(values = c("#00a6b3","#e3af00")) + 
        # # scale_color_manual(values = c("#00a6b3","#e3af00")) + 
        # facet_wrap( ~ rowname, ncol=4,scales = "free") +
        theme_bw() + 
        theme(text=element_text(family="Times"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.text.x = element_text(colour = "black"),
              panel.border = element_rect(colour = "black", fill = NA),
              strip.background = element_blank()) +
        ggtitle(paste0(input$choose))
    }
    
    output$sinplot <- renderPlot(width = 600,
                                 height = 600,
                                 res = 100,
                                 {
                                   splot()
                                 })
    
    allplot <- function() {
      res <- res()
      ggplot(res, aes(x=group, y=value, fill = group)) +
        geom_boxplot(outlier.shape=19,lwd = 0.1,
                     outlier.size=0.3) + 
        scale_fill_manual(values = c("#00a6b3","#e3af00")) + 
        # scale_color_manual(values = c("#00a6b3","#e3af00")) + 
        facet_wrap( ~ rowname, ncol=4,scales = "free") +
        theme_bw() + 
        theme(text=element_text(family="Times"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.text.x = element_text(colour = "black"),
              panel.border = element_rect(colour = "black", fill = NA),
              strip.background = element_blank())
    }
    
    observeEvent(input$plotall, {
      output$allplot <- renderPlot(width = function() 1200,
                 height = function() allHeight(),
                 res = 100,
                 {
                   allplot()
                 })
    })
      
    # Create the button to download the plot as PDF
    output$dallPlot <- downloadHandler(
      filename = function() {
        paste('allboxPlot_', Sys.Date(), '.pdf', sep='')
      },
      content = function(file) {
        ggplot2::ggsave(file, allplot(), width = 1200/100, height = allHeight()/100, dpi = 300, units = "in", limitsize = FALSE)
      }
    )
    
    # Create the button to download the plot as separately
    allsepplot <- function() {
      res <- res()
      doParallel::registerDoParallel()
      foreach::getDoParWorkers() 
      plyr::ddply(
        res, .variables = "rowname", .fun = function(x) {
          #do your plotting now 
          example_plot <- ggplot(res, aes(x=group, y=value, fill = group)) +
            geom_boxplot(outlier.shape=19,lwd = 0.1,
                         outlier.size=0.3) + 
            scale_fill_manual(values = c("#00a6b3","#e3af00")) + 
            # # scale_color_manual(values = c("#00a6b3","#e3af00")) + 
            # facet_wrap( ~ rowname, ncol=4,scales = "free") +
            theme_bw() + 
            theme(text=element_text(family="Times"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.text.x = element_text(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA),
                  strip.background = element_blank()) +
            ggtitle(paste0(x$rowname[1]))
          #save your plot
          ggsave(paste(x$rowname[1],".pdf",sep = ""), example_plot)
        },
        .parallel = TRUE
      )
    }
    
    # Download the plots
    output$sep.plot = downloadHandler(
      filename = 'plots.zip',
      content = function( file){
        
        # Set temporary working directory
        owd <- setwd( tempdir())
        on.exit( setwd( owd))
        
        # Save the histograms (a loop can be used here for a bunch of plots)
        allsepplot()
        
        # Zip them up
        zip( file, list.files(pattern = "\\.pdf$"))
      }
    )
    
  })
}
    
## To be copied in the UI
# mod_Boxplot_of_Diff_ui("Boxplot_of_Diff_ui_1")
    
## To be copied in the server
# mod_Boxplot_of_Diff_server("Boxplot_of_Diff_ui_1")
