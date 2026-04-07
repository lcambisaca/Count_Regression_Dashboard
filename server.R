server <- (function(input, output, session){
  options(scipen = 4)
  #############################################################################################
  # DATA OBJECTS
  #############################################################################################
  globalVars <- reactiveValues() #Global Variables
  globalVars$sample <- FALSE
  globalVars$dataset <- NULL
  globalVars$dataset.original <- NULL
  globalVars$fcts <- c()                 # Tracking Categorical Variables
  globalVars$transform.type <- "none"
  globalVars$interactions = c()
  
  globalVars$equation <- NULL
  globalVars$response <- NULL
  globalVars$model <- NULL
  globalVars$modelsummary <- NULL
  globalVars$prepare_model_interp <- NULL        
  
  globalVars$anova <- NULL
  globalVars$prepare_anova_interp <- NULL
  
  globalVars$anova_fctcomp <- NULL
  globalVars$make_anovafctcomp_plot <- NULL
  globalVars$make_anovafctcomp_num <- NULL
  globalVars$prepare_anova_fctcompinterp <- NULL
  
  globalVars$emmeans <- NULL
  globalVars$mod.emmeanscontrast <- NULL
  globalVars$mod.emtrends <- NULL
  globalVars$mod.emtrendcontrast <- NULL
  
  globalVars$make_ggpairs_plot <- NULL
  globalVars$make_ggpairs_summary <- NULL
  globalVars$make_assumptions_plot <- NULL
  
  globalVars$make_vif_num <- NULL
  globalVars$make_check_plot <- NULL
  
  globalVars$make_anova_num <- NULL
  globalVars$make_model_summary <- NULL

  globalVars$mod.emtrendcontrast <- NULL
  globalVars$make_interaction_emtrendscontrast_table <- NULL              
  globalVars$prepare_interaction_emtrendscontrast_interp <- NULL              
  globalVars$make_interaction_emtrends_tab <- NULL
  globalVars$prepare_interaction_emtrends_interp <- NULL
  
  globalVars$make_interaction_emmeans_tab <- NULL
  globalVars$prepare_interaction_emmeans_interp <- NULL            
  globalVars$make_interaction_contrasts_tab <- NULL
  globalVars$prepare_interaction_emmeanscontrast_interp <- NULL
  globalVars$ggemmeansplot <- NULL
  
  globalVars$jnplot <- NULL
  globalVars$prepare_jn_int <- NULL
  
  globalVars$modelmargins <- NULL
  globalVars$make_margins_summary <- NULL
  globalVars$prepare_margins_interp <- NULL
  
  globalVars$changed.input <- TRUE    
  
  globalVars$scaleDataUpToDate <- FALSE
  
  shinyjs::hide("interaction_analysis")
  shinyjs::hide("var_inter")
  shinyjs::hide("var_moderator")
  shinyjs::hide("interaction.error")
  shinyjs::hide("ihs_button")
  shinyjs::hide("log_button")
  shinyjs::hide("lp1_button")
  
  hideTab(inputId="workPanel", target="summary")
  hideTab(inputId="workPanel", target="assumptions")
  hideTab(inputId="workPanel", target="checks")
  hideTab(inputId="workPanel", target="anova")
  hideTab(inputId="workPanel", target="interpretation")
  hideTab(inputId="workPanel", target="interaction")
  
  
  ##############################################
  # PROCESS UPLOADED DATA
  ##############################################
  upload_data<-reactive({
    req(input$file_upload) # this connects to file input in ui and makes it so we pause our code here until we add a file
    tryCatch({
      dat <- read.csv(input$file_upload$datapath)},
      error = function(e){
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    globalVars$dataset <- dat %>% mutate_if(is.character,as.factor)%>%
      mutate_if(is.integer,as.numeric)
    
    globalVars$dataset.original <- globalVars$dataset
    
    updateFactorsSelectize()
    emptyEquation()
  })
  
  
  ##############################################
  # DATASET PREVIEW
  ##############################################
  output$preview.data <- DT::renderDataTable({
    if(globalVars$sample){
      filename <- case_when(input$sample_data_choice=="Bracht et al. MFAP4"                             ~ "BrachtMFAP4Data",
                            input$sample_data_choice=="Palmer Penguins"                                 ~ "PalmerPenguin",
                            input$sample_data_choice=="U.S. News College Data"                          ~ "College",
                            input$sample_data_choice=="Cooley's Poor Beliefs Data"                      ~ "poorbeliefs"
      )
      
      DT::datatable(
        {globalVars$dataset},
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = list(list(extend="csv", text = "Download CSV", filename=filename,  exportOptions = list(modifier = list(page = "all"))))
        )
        , rownames = FALSE)
    }else{
      DT::datatable(globalVars$dataset, rownames = FALSE)
    }
    
  }, server = FALSE)
  
  #############################################################################################
  # When data is uploaded -- preview data and clear everything else
  #############################################################################################
  observeEvent(input$file_upload,{
    globalVars$changed.input <- TRUE
    #upload data and preview
    inFile <<-upload_data()
    #clear everything else
    hideInteractionInput()
    
    #UI resets
    updateTabsetPanel(session, "workPanel", selected = "Data Preview")
    globalVars$model <- NULL
    shinyjs::show("select_factors")
    uncheckAllAssumptions()
    globalVars$scaleDataUpToDate <- FALSE 
    updateCheckboxInput(session, "scalevars", value = FALSE)
    
  })
  
  read_data <- metaReactive2({
    if(globalVars$sample){
      if(input$sample_data_choice=="Bracht et al. MFAP4"){
        metaExpr({
          "# You can download the data on the Data Preview page"
          dat<-read_csv("BrachtMFAP4Data.csv")%>%
            mutate(Age=as.numeric(Age))
        })
      }else if(input$sample_data_choice=="Palmer Penguins"){
        metaExpr({
          "# You may need to install the palmerpenguins package: install.packages('palmerpenguins')"
          library(palmerpenguins)
          dat<-tibble(penguins)
        })
      }else if(input$sample_data_choice=="U.S. News College Data"){
        metaExpr({
          "# You may need to install the ISLR package: install.packages('ISLR')"
          library(ISLR)
          dat<-College
        })
      }else if(input$sample_data_choice=="Cooley's Poor Beliefs Data"){
        metaExpr({
          dat<-read_csv("poorbeliefs.csv") %>% mutate(Democrat = factor(Democrat))
        })
      }
    }else{
      metaExpr({
        quote(dat <- read_csv(..(input$file_upload$name)))
      }, quoted = T)
    }
  },inline=TRUE)
  
  
  updateFactorsSelectize<- function(){
    # req(globalVars$dataset)
    var.names = colnames(globalVars$dataset)
    fctvar.names = c(names(Filter(is.factor,globalVars$dataset)), names(Filter(is.character,globalVars$dataset)))
    globalVars$fcts <- fctvar.names
    updateSelectizeInput(session, "select_factors",
                         "Specify Categorical Variables in the Data:",
                         choices = var.names,
                         selected = fctvar.names) # updateSelectizeInput
  }
  
  emptyEquation <- function(){
    updateTextInput(session, "equation", value = "")
  }

  hideInteractionInput <- function(){
    shinyjs::hide("interaction_analysis")
    shinyjs::hide("var_inter")
    shinyjs::hide("var_moderator")
    shinyjs::hide("interaction.error")
    
    updateSelectizeInput(session, "var_inter",
                         "Select Interaction",
                         choices = c("None"),
                         selected = "None")
    
    updateSelectizeInput(session, "var_moderator",
                         "Select Moderator",
                         choices = c("Select..."),
                         selected = "Select...")
    
    updateCheckboxInput(session, "interaction.error", value =TRUE)
  }
  
  showInteractionInput <- function(){
    shinyjs::show("interaction_analysis")
    shinyjs::show("var_inter")
  }
  
  
  
  uncheckAllAssumptions <- function() {
    shinyjs::hide("asmp_1note")
    shinyjs::hide("asmp_2note")
    shinyjs::hide("asmp_3note")
    shinyjs::hide("asmp_4note")
    shinyjs::hide("all")
    shinyjs::hide("asmp_note")
    updateCheckboxInput(session, "asmp_1", value = FALSE)
    updateCheckboxInput(session, "asmp_2", value = FALSE)
    updateCheckboxInput(session, "asmp_3", value = FALSE)
    updateCheckboxInput(session, "asmp_4", value = FALSE)
    
    shinyjs::hide("ihs_button")
    shinyjs::hide("log_button")
    shinyjs::hide("lp1_button")
    
    shinyjs::hide("check_note")
    updateCheckboxInput(session, "check_1", value = FALSE)
    updateCheckboxInput(session, "check_2", value = FALSE)
    updateCheckboxInput(session, "check_3", value = FALSE)
    updateCheckboxInput(session, "check_4", value = FALSE)
    
    updateCheckboxInput(session, "interaction.error", value = TRUE)
  }
  
  
 }
)