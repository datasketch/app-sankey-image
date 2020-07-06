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
library(V8)

# load in ds packages
library(dsmodules)
library(shinyinvoer)
library(shinypanels)
library(shi18ny)
library(hotr)
library(parmesan)

# load functions to prepare data and create plot
source("sankey_functions.R")

# Define UI for data download app ----
ui <- panelsPage(useShi18ny(),
                 langSelectorInput("lang", position = "fixed"),
                 panel(title = ui_("upload_data"), 
                       width = 200,
                       body = uiOutput("dataInput")),
                 panel(title = ui_("dataset"), 
                       width = 300,
                       body = uiOutput("dataset")),
                 panel(title = ui_("options"), 
                       width = 250,
                       color = "chardonnay",
                       body = uiOutput("controls")),
                 panel(title = ui_("viz"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(
                         langSelectorInput("lang", position = "fixed"),
                         plotOutput("sankeyChart"),
                         shinypanels::modal(id = "download",
                                            title = ui_("download_plot"),
                                            uiOutput("modal"))),
                       footer = shinypanels::modalButton(label = ui_("download_plot"), modal_id = "download")))



# Define server logic ----
server <- function(input, output) {
  
  inputData <- callModule(tableInput, 
                          "initial_data",
                          sampleFile =
                            list("Titanic"="./data/titanic_data.csv",
                                 "UK general election 2019"="./data/election_data.csv"))
  
  i18n <- list(defaultLang = "en", 
               availableLangs = c("en", "de"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = FALSE)
  
  observeEvent(lang(), {uiLangUpdate(input$shi18ny_ui_classes, lang())})
  
  output$dataInput <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload", "google"), lang = lang())
    tableInputUI("initial_data", 
                 choices = choices,
                 selected = ifelse(is.null(input$`initial_data-tableInput`), "sampleData", input$`initial_data-tableInput`))
  })
  
  labels <- reactive({
    
    sm_f <- paste0("data/", i_(c("sample_ch_0", "sample_ch_1"), lang()))
    names(sm_f) <- i_(c("sample_ch_nm_0", "sample_ch_nm_1"), lang())
    
    list(sampleLabel = i_("sample_lb", lang()), 
         sampleFiles = sm_f,
         
         pasteLabel = i_("paste", lang()), 
         pasteValue = "", 
         pastePlaceholder = i_("paste_pl", lang()), 
         pasteRows = 5,
         
         uploadLabel = i_("upload_lb", lang()),
         uploadButtonLabel = i_("upload_bt_lb", lang()), 
         uploadPlaceholder = i_("upload_pl", lang()),
         
         googleSheetLabel = i_("google_sh_lb", lang()),
         googleSheetValue = "",
         googleSheetPlaceholder = i_("google_sh_pl", lang()),
         googleSheetPageLabel = i_("google_sh_pg_lb", lang())
    )
  })
  
  output$dataset <- renderUI({
    if (is.null(inputData())) 
      return()
    suppressWarnings(hotr("hotr_input", data = inputData(), order = NULL, options = list(height = 470), enableCTypes = FALSE))
  })
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "choices", "text"))})
  output_parmesan("controls",
                  parmesan = parmesan_lang,
                  input = input,
                  output = output,
                  env = environment())
  
  
  datasetColumnChoices <- reactive({
    names(inputData())
  })
  
  datasetColumnSelected <- reactive({
    names(inputData())[1:2]
  })
  
  useFillValue <- reactive({
    if(!is.null(input$fillval)) TRUE else FALSE
  })
  
  colourMethodChoices <- reactive({
    colour_method_choices <- list("colourpalette" = "colourpalette", "custom" = "custom")
    names(colour_method_choices) <- i_(names(colour_method_choices), lang())
    colour_method_choices
  })

  stratumColourChoices <- reactive({
    stratum_method_choices <- list("black" = "black", "white" = "white")
    names(stratum_method_choices) <- i_(names(stratum_method_choices), lang())
    stratum_method_choices
  })
  
  colourPaletteChoices <- reactive({
    list(
      "Categorical:" = list("Accent", "Dark2", "Paired", "Pastel1",
                            "Pastel2", "Set1", "Set2", "Set3"),
      "Diverging" = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy",
                         "RdYlBu", "RdYlGn", "Spectral"),
      "Sequential:" = list("Blues", "BuGn", "BuPu", "GnBu", "Greens",
                           "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples",
                           "RdPu", "Reds", "YlGn", "YlGnBu", "GlOrBr", "YlOrRD")
    )
  })
  
  colourCustomChoices <- reactive({
    rep("#000000", length(categoriesFill()))
  }) 
  
  maxCustomChoices <- reactive({
    length(categoriesFill())
  })
  
  customColours <- reactive({
    colours <- paste0("#",input$colour_custom)
    names(colours) <- sort(categoriesFill())
    colours
  }) 
  
  categoriesFill <- reactive({
    inputData() %>% select(input$fillval) %>% distinct() %>% pull()
  })
  
  plot_data <- reactive({
    prepare_data(df = inputData(), 
                 col_vars = input$chooseColumns, 
                 fill_var = input$fillval)
    })
  
  plot <- reactive({
    req(input$chooseColumns)
    palette = input$palette
    if(input$colour_method == "colourpalette"){
      palette <- input$palette
      manualcols <- NULL
    } else if(input$colour_method == "custom"){
      palette <- NULL
      manualcols <- customColours()
    }
    create_sankey_plot(df = plot_data(), 
                       stratum_colour = input$stratumColour,
                       fill_var = input$fillval,
                       palette = palette,
                       manualcols = manualcols
                       )
    })

  output$sankeyChart <- renderPlot({
    plot()
  })


  output$modal <- renderUI({
    dw <- i_("download", lang())
    downloadImageUI("download_data_button", dw, formats = c("jpeg", "png"))
  })
  
  callModule(downloadImage, "download_data_button", graph = plot(), lib = "ggplot", formats = c("jpeg", "png"))
  
}

# Create Shiny app ----
shinyApp(ui, server)