# load in packages
library(shiny)
library(shinyWidgets)
library(dplyr)
library(titanic)
library(ggplot2)
library(ggforce)
library(RColorBrewer)
library(scales)
library(tidyverse)
library(dsmodules)


###================================================================================
### plot function to prepare data and create sankey plot
###================================================================================

prepare_data <- function(df, col_vars, fill_var = ""){
  
  if(fill_var == ""){
    fill_var <- NULL
    }
  
  if(is.null(fill_var)){
    groupby <- col_vars
  } else if(fill_var %in% col_vars){
    groupby <- col_vars
  } else {
    groupby <- c(col_vars, fill_var)
  }
  
  dat_prelim <- df %>% 
    select(groupby) %>% 
    replace(is.na(.), "missing") %>% 
    group_by_at(groupby) %>% 
    summarise(Freq = n()) %>%
    ungroup() %>% 
    mutate(id = row_number())
  
  if(is.null(fill_var)){
  dat_plot <- dat_prelim %>% 
    tidyr::gather(key = "x",
                  value = "stratum",
                  factor_key = TRUE,
                  col_vars)
  } else {
    dat_plot <- dat_prelim %>% 
      tidyr::gather(key = "x",
                    value = "stratum",
                    factor_key = TRUE,
                    col_vars) %>%
      left_join(dat_prelim %>%
                  mutate_(fill = fill_var) %>%
                  select(id, fill),
                by = "id")
  }
  return(dat_plot)
}

create_sankey_plot <- function(df, fill_var = "", palette = NULL, manualcols = NULL, labels = NULL, stratum_colour = "black"){
  
  if(fill_var == ""){
    fill_var <- NULL
  }
  
  
  if(stratum_colour == "black"){
    stratum_font_colour = "white"
  } else {
    stratum_font_colour = "black"
    
  }
  
  stratum_line_colour = "black"
  
  stratum_width <- 0.4
  stratum_angle <- 0
  legend_position <- "right"
  alpha <- 0.7
  
  gg <- ggplot(df, aes(x = x, id = id, split = stratum, value = Freq)) +
        geom_parallel_sets(alpha = alpha, axis.width = stratum_width) +
        geom_parallel_sets_axes(axis.width = stratum_width, 
                                fill = stratum_colour,
                                colour = stratum_line_colour) +
        geom_parallel_sets_labels(colour = stratum_font_colour, angle = stratum_angle) +
        theme_minimal() +
        theme(
          legend.position = legend_position,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 14, face = "bold"),
          axis.title.x  = element_blank()
          )
  
  if(!is.null(fill_var)){
    gg <-   gg <- ggplot(df, aes(x = x, id = id, split = stratum, value = Freq)) +
      geom_parallel_sets(aes(fill = fill), alpha = alpha, axis.width = stratum_width) +
      geom_parallel_sets_axes(axis.width = stratum_width, 
                              fill = stratum_colour,
                              colour = stratum_line_colour) +
      geom_parallel_sets_labels(colour = stratum_font_colour, angle = stratum_angle) +
      scale_fill_discrete(name = fill_var) +
      theme_minimal() +
      theme(
        legend.position = legend_position,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.title.x  = element_blank()
      )
  }
  
  if(!is.null(palette)){
    gg <- gg +
      scale_fill_brewer(name = fill_var, palette = palette) +
      scale_color_brewer(palette = palette)
  }
  
  if(!is.null(manualcols)){
    gg <- gg +
      scale_fill_manual(name = fill_var, values = manualcols) +
      scale_color_manual(values = manualcols)
  }
  
  if(!is.null(labels)){
    gg <- gg +
      scale_x_discrete(labels = labels)
  }
  
  return(gg)
}


###================================================================================
### app
###================================================================================

# Define UI for data download app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("App for Sankey plot"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      tableInputUI("dataInput", choices = list("Copy & paste" = "pasted",
                                               "File upload" = "fileUpload",
                                               "Example data" = "sampleData",
                                               "Google sheets" = "googleSheets"),
                   selected = "sampleData"),
      verbatimTextOutput("inputOptions"),
      # Input: Choose columns ----
      uiOutput("chooseColumnsInput"),
      uiOutput("chooseFillvalInput"),
      conditionalPanel(
        condition = "input.fillval != ''",
        selectizeInput(
          "fillcol",
          "Choose method for colour selection:",
          choices = list("Colour palette" = "colourpalette", "Custom" = "custom"),
          options = list(
            placeholder = 'Optionally select a colour method',
            onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ),
      conditionalPanel(
        condition = "input.fillcol == 'colourpalette'",
        selectInput(
          "palettes", 
          "Colour palettes",
          choices = list(
            "Categorical:" = list("Accent", "Dark2", "Paired", "Pastel1",
                                  "Pastel2", "Set1", "Set2", "Set3"),
            "Diverging" = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy",
                               "RdYlBu", "RdYlGn", "Spectral"),
            "Sequential:" = list("Blues", "BuGn", "BuPu", "GnBu", "Greens",
                                 "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples",
                                 "RdPu", "Reds", "YlGn", "YlGnBu", "GlOrBr", "YlOrRD")
            )
          )
        ),
      conditionalPanel(
        condition = "input.fillcol == 'custom'",
        uiOutput("customcolours")
      ),
      radioButtons("stratumColour", 
                   "Choose colour of stratum",
                   choices = list("Black" = "black", "White" = "white"), 
                   selected = "black"
                   ),
      
      
      # Button
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset", tableOutput("dataset")),
        tabPanel("Sankey plot", plotOutput("sankeyChart")),
        tabPanel("Data preview", tableOutput("formattedData"))
        )
      )
    )
)

# Define server logic to display and download selected file ----
server <- function(input, output) {
  
  inputData <- callModule(tableInput, 
                          "dataInput",
                          sampleFile =
                            list("Titanic"="./data/titanic_data.csv",
                                 "UK general election 2019"="./data/election_data.csv"))
  
  output$inputOptions <- renderPrint({
    inputData()
  })
  
  output$chooseColumnsInput <- renderUI({
    selectizeInput("chooseColumns", 
                   "Choose columns for axes:",
                   selected = names(inputData())[1:2],
                   choices = names(inputData()),
                   multiple = TRUE
    )
  })
  
  output$chooseFillvalInput <- renderUI({
    selectizeInput(
      "fillval", 
      "Choose column for fill:",
      choices = names(inputData()),
      options = list(
        placeholder = 'Optionally select a fill variable',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })
  
  colours <- reactive({
    if(input$fillcol == "colourpalette"){
    input$palettes
    } else {
      NULL
    }
  })
  
  categoriesFill <- reactive({
    inputData() %>% select(input$fillval) %>% distinct() %>% pull()
  })
  
  inputColours <- reactiveValues(x=NULL)
  colourVector <- reactive({
    if(input$fillcol == "custom"){
      lapply(1:length(categoriesFill()), function(i) {
        inputColours$x[[i]] <- input[[paste0("colour", i)]]
      })
    colours <- inputColours$x
    names(colours) <- categoriesFill()
    colours
    } else {
      NULL
    }
  })
  
  colourInputList <- reactive({
    req(input$fillval)
    ls <- list()
    for (i in 1:length(categoriesFill())){
      category <- categoriesFill()[i]
      si <- spectrumInput(
        inputId = paste0("colour", i),
        label = paste0("Pick a color for ",input$fillval," - ",category,":"),
        selected = "#000000",
        choices = list(
          list('black', 'white', 'blanchedalmond', 'steelblue', 'forestgreen'),
          as.list(brewer_pal(palette = "Blues")(9)),
          as.list(brewer_pal(palette = "Greens")(9)),
          as.list(brewer_pal(palette = "Spectral")(11)),
          as.list(brewer_pal(palette = "Dark2")(8))
        ),
        options = list(`toggle-palette-more-text` = "Show more")
      )
      ls[[i]] <- si
    }
    ls
  })

  
  output$customcolours <- renderUI({
    colourInputList()
    })
  
  plot_data <- reactive({
    prepare_data(df = inputData(), 
                 col_vars = input$chooseColumns, 
                 fill_var = input$fillval)
    })
  
  plot <- reactive({
    req(input$chooseColumns)
    colourVector <- NULL
    try({
      colourVector <- colourVector()
    })
    create_sankey_plot(df = plot_data(), 
                       stratum_colour = input$stratumColour,
                       fill_var = input$fillval,
                       palette = colours(),
                       manualcols = colourVector
                       )
    })

  output$sankeyChart <- renderPlot({
    plot()
  })

  output$dataset <- renderTable({
    head(inputData(), n = 10)
  })
    
  output$formattedData <- renderTable({
    head(plot_data() %>% arrange(id), n = 10)
  })
  
  # Downloadable png of selected plot type ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("sankey.png")
    },
    content = function(file) {
      plot()
      ggsave(file, device = "png")
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)