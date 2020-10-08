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
library(shinydisconnect)

# load in ds packages
library(dspins)
library(dsmodules)
library(shinyinvoer)
library(shinypanels)
library(shi18ny)
library(hotr)
library(parmesan)
library(paletero)

# load functions to prepare data and create plot
source("sankey_functions.R")

# Define UI for data download app ----
ui <- panelsPage(useShi18ny(),
                 disconnectMessage(
                   text = "Oh no!, la sesión a finalizado, si estabas trabajando en la app, por favor contacta a soporte y cuentanos que ha sucedido//Oh no, the session has ended, if you were working on the app, please contact support and tell us what has happened",
                   refresh = "O, intenta de nuevo//Or try again",
                   background = "#385573",
                   colour = "white",
                   overlayColour = "grey",
                   overlayOpacity = 0.3,
                   refreshColour = "#FBC140"
                 ),
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
                       title_plugin = uiOutput("download"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = uiOutput("viz")))



# Define server logic ----
server <- function(input, output, session) {

  i18n <- list(defaultLang = "en",
               availableLangs = c("en", "de", "es", "pt"))

  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = FALSE)

  observeEvent(lang(),{
    uiLangUpdate(input$shi18ny_ui_classes, lang())
    })

  output$dataInput <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload", "google"), lang = lang())
    tableInputUI("initial_data",
                 "Input data",
                 choices = choices,
                 selected =  "sampleData")
  })

  sample_data <- reactive({
    sm_f <- list("data/titanic_data.csv",
                 "data/election_data.csv")
    names(sm_f) <- i_(c("sample_titanic_name", "sample_elections_name"), lang())
    sm_f
  })

  inputData <- tableInputServer("initial_data", 
                                sampleLabel = i_("sample_lb", lang()),
                                sampleFiles = sample_data(),
                                sampleSelected = names(sample_data())[1],
                                
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
                                googleSheetPageLabel = i_("google_sh_pg_lb", lang()))

  output$dataset <- renderUI({
    if (is.null(inputData()))
      return()
    suppressWarnings(hotr("hotr_input", data = inputData(), options = list(height = 470)))
  })

  data_fringe <- reactive({
    suppressWarnings( hotr::hotr_fringe(input$hotr_input))
  })

  dic_load <- reactive({
    data_fringe()$dic
  })

  data_load <- reactive({
    data <- data_fringe()$data
    names(data) <- dic_load()$label
    as.data.frame(data)
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
    dic_load()$label
  })

  moreDataInfo <- reactive({
    data_load() %>% map_df( ~ (data.frame(
      n_distinct = n_distinct(.x),
      class = class(.x)
    )),
    .id = "variable") %>% filter(!class == "vctrs_vctr")
  })
  
  datasetColumnSelected <- reactive({
    possible_columns <- moreDataInfo() %>% filter(n_distinct < 10) %>% distinct(variable) %>% pull()
    dic_cat <- dic_load() %>% filter(hdType == "Cat") %>% filter(label %in% possible_columns)
    dic_cat$label[1:2]
  })
  
  dic_draw <- reactive({
    moreDataInfo() %>% filter(variable %in% input$chooseColumns)
  })
  
  fillValueSelected <- reactive({
    datasetColumnSelected()[1]
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
    paletero::paletero_cat(categoriesFill(), palette = "Set1")
  }) 

  maxCustomChoices <- reactive({
    length(categoriesFill())
  })

  customColours <- reactive({
    colours <- input$colour_custom
    names(colours) <- sort(categoriesFill())
    colours
  })

  categoriesFill <- reactive({
    data_load() %>% select(input$fillval) %>% distinct() %>% pull()
  })

  plot_data <- reactive({
    if(!input$chooseColumns %in% names(data_load())) return()
    if(any(dic_draw()$class != "hd_Cat") | any(dic_draw()$n_distinct > 10)) return()
    data <- prepare_data(df = data_load(),
                 col_vars = input$chooseColumns,
                 fill_var = input$fillval)
    data <- data %>% mutate(fill = as.character(fill))
    data
    })

  gg_viz <- reactive({
    req(input$chooseColumns)
    if(is.null(plot_data()))return()
    palette <- input$palette
    manualcols <- NULL
    colour_method <- "colourpalette"
    if(!is.null(input$colour_method)){
      colour_method <- input$colour_method
    }
    if(colour_method == "colourpalette"){
      palette <- input$palette
      manualcols <- NULL
    } else if(colour_method == "custom"){
      palette <- NULL
      if(is.null(input$colour_custom)) return()
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
    if(is.null(gg_viz())) return()
    gg_viz()
  })
  
  output$viz <- renderUI({
    if(is.null(dic_draw()))return()
    if(any(dic_draw()$class != "hd_Cat") | any(dic_draw()$n_distinct > 10)){
      v <- div(shinypanels::infomessage(type = "warning" , i_("cannot_plot", lang())),
               shinypanels::infomessage(type = "info" , i_("data_advice", lang())))
    } else {
      v <- plotOutput("sankeyChart")
    }
    v
  })


  output$download <- renderUI({
    lb <- i_("download_viz", lang())
    dw <- i_("download", lang())
    gl <- i_("get_link", lang())
    mb <- list(textInput("name", i_("gl_name", lang())),
               textInput("description", i_("gl_description", lang())),
               selectInput("license", i_("gl_license", lang()), choices = c("CC0", "CC-BY")),
               selectizeInput("tags", i_("gl_tags", lang()), choices = list("No tag" = "no-tag"), multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop'))),
               selectizeInput("category", i_("gl_category", lang()), choices = list("No category" = "no-category")))
    downloadDsUI("download_data_button", dropdownLabel = lb, text = dw, formats = c("jpeg", "pdf", "png"),
                 display = "dropdown", dropdownWidth = 170, getLinkLabel = gl, modalTitle = gl, modalBody = mb,
                 modalButtonLabel = i_("gl_save", lang()), modalLinkLabel = i_("gl_url", lang()), modalIframeLabel = i_("gl_iframe", lang()),
                 modalFormatChoices = c("PNG" = "png"))
  })
  
  
  par <- list(user_name = "brandon")
  url_par <- reactive({
    url_params(par, session)
  })
  
  pin_ <- function(x, bkt, ...) {
    x <- dsmodules:::eval_reactives(x)
    bkt <- dsmodules:::eval_reactives(bkt)
    nm <- input$`download_data_button-modal_form-name`
    if (!nzchar(input$`download_data_button-modal_form-name`)) {
      nm <- paste0("saved", "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)))
      updateTextInput(session, "download_data_button-modal_form-name", value = nm)
    }
    dv <- dsviz(x,
                name = nm,
                description = input$`download_data_button-modal_form-description`,
                license = input$`download_data_button-modal_form-license`,
                tags = input$`download_data_button-modal_form-tags`,
                category = input$`download_data_button-modal_form-category`)
    dspins_user_board_connect(bkt)
    Sys.setlocale(locale = "en_US.UTF-8")
    pin(dv, bucket_id = bkt)
  }
  
  
  observe({
    req(gg_viz())
    if (is.null(url_par()$inputs$user_name)) return()
    
    downloadDsServer("download_data_button", element = reactive(gg_viz()),
                     formats = c("jpeg", "pdf", "png"),
                     errorMessage = i_("error_down", lang()),
                     modalFunction = pin_, reactive(gg_viz()),
                     bkt = url_par()$inputs$user_name)
  })

}

# Create Shiny app ----
shinyApp(ui, server)
