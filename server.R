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
  
  # (NOTE PLOT 6) Make sure to instantiate plots
  globalVars$Pois_RQRPlot <- NULL
  globalVars$NB_RQRPlot <- NULL
  globalVars$RQRPlot <- NULL
  globalVars$Pearson_Residual <- NULL
  
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
  
  #These are tabs for dataset portion
  hideTab(inputId="workPanel", target="summary")
  hideTab(inputId="workPanel", target="assumptions")
  hideTab(inputId="workPanel", target="checks")
  hideTab(inputId="workPanel", target="anova")
  hideTab(inputId="workPanel", target="interpretation")
  hideTab(inputId="workPanel", target="interaction")
  
  
  ##############################################
  # PROCESS UPLOADED DATA
  ##############################################
  upload_data<-reactive({ # when function is called it now run
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
  output$preview.data <- DT::renderDataTable({ # this gets called from ui when we want to render dataset e.r call oreview
    if(globalVars$sample){ #Edit this for sample data
      filename <- case_when(input$sample_data_choice=="Bracht et al. MFAP4"                             ~ "BrachtMFAP4Data",
                            input$sample_data_choice=="Palmer Penguins"                                 ~ "PalmerPenguin",
                            input$sample_data_choice=="U.S. News College Data"                          ~ "College",
                            input$sample_data_choice=="Camera Data"                                     ~ "cs_replication_data",
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
  observeEvent(input$file_upload,{ # check to see if this specefic var gets changed this occurs first
    globalVars$changed.input <- TRUE
    #upload data and preview
    inFile <<- upload_data() # saved to global var
    #clear everything else
    hideInteractionInput()
    
    #UI resets
    updateTabsetPanel(session, "workPanel", selected = "Data Preview") # This function updates a specific tabset depending on id
    globalVars$model <- NULL
    shinyjs::show("select_factors")
    uncheckAllAssumptions()
    globalVars$scaleDataUpToDate <- FALSE 
    updateCheckboxInput(session, "scalevars", value = FALSE)
    
  })
  
  # metaExpr acts as a Script Recorder it allows us to remeber the line code we used to execute our script
  # in this case a specefic part. So with this were getting the actual data fra,e and code that implemented it
  # reactive focuses on output # meta reactive focus on process
  
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
      }else if(input$sample_data_choice=="Camera Data"){
        metaExpr({
          dat<-read_csv("cs_replication_data.csv")
        })
      }
      
    }else{
      metaExpr({
        quote(dat <- read_csv(..(input$file_upload$name)))
      }, quoted = T)
    }
  },inline=TRUE)
  
  
  #############################################################################################
  # ADD VARIABLES TO FACTOR SELECTIZE INPUT
  #############################################################################################
  updateFactorsSelectize<- function(){
    req(globalVars$dataset)
    var.names = colnames(globalVars$dataset)
    fctvar.names = c(names(Filter(is.factor,globalVars$dataset)), names(Filter(is.character,globalVars$dataset)))
    globalVars$fcts <- fctvar.names
    updateSelectizeInput(session, "select_factors",
                         "Specify Categorical Variables in the Data:",
                         choices = var.names,
                         selected = fctvar.names)
  }
  
  #############################################################################################
  # Helper functions
  #############################################################################################
  # convert scientific notation to 10^
  changeSciNot <- function(n) {
    output <- format(n, scientific = TRUE) #Transforms the number into scientific notation even if small
    output <- sub("e", "*10^", output) #Replace e with 10^
    output <- sub("\\+0?", "", output) #Remove + symbol and leading zeros on expoent, if > 1
    output <- sub("-0?", "-", output) #Leaves - symbol but removes leading zeros on expoent, if < 1
    output
  }
  
  
  
  hideAllTabs <- function(){
    updateTabsetPanel(session, "workPanel", selected="data")
    
    hideTab(inputId="workPanel", target="summary")
    hideTab(inputId="workPanel", target="assumptions")
    hideTab(inputId="workPanel", target="checks")
    hideTab(inputId="workPanel", target="anova")
    hideTab(inputId="workPanel", target="interpretation")
    hideTab(inputId="workPanel", target="interaction")
  }
  
  showAllTabs <- function(){
    showTab(inputId="workPanel", target="summary")
    showTab(inputId="workPanel", target="assumptions")
    showTab(inputId="workPanel", target="checks")
    showTab(inputId="workPanel", target="anova")
    showTab(inputId="workPanel", target="interpretation")
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
  
  
  emptyEquation <- function(){
    updateTextInput(session, "equation", value = "")
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
  
  ##############################################################
  # When sample data is loaded (Tom look of sample is good or bad and make adjustements as needed)
  ##############################################################
  # Tom edit below
  
  observeEvent(input$sample,{
    globalVars$changed.input <- TRUE
    updateTabsetPanel(session, "workPanel", selected = "data")
    globalVars$model <- NULL
    hideAllTabs()
    hideInteractionInput()
    uncheckAllAssumptions()
    emptyEquation()
    
    
    if(!globalVars$sample){
      globalVars$sample <- TRUE
      globalVars$dataset <- NULL
      shinyjs::hide("file_upload")
      shinyjs::show("choose_sample") # looks for id in ui
      shinyjs::show("select_factors")
      
      if(input$sample_data_choice=="Palmer Penguins"){
        library(palmerpenguins)
        dat<-data.frame(penguins)
      }else if(input$sample_data_choice=="Bracht et al. MFAP4" ){
        dat<-read.csv("www/BrachtMFAP4Data.csv")%>%
          mutate(Age=as.numeric(Age))
      }else if(input$sample_data_choice=="U.S. News College Data"){
        library(ISLR)
        dat<- College
      }else if(input$sample_data_choice=="Camera Data"){
        dat<-read.csv("www/cs_replication_data.csv")
      }
      # basic data fix stuff
      globalVars$dataset <- dat %>% mutate_if(is.character,as.factor)%>%
        mutate_if(is.integer,as.numeric)    
      globalVars$dataset.original <- dat %>% mutate_if(is.character,as.factor)%>%
        mutate_if(is.integer,as.numeric)
      
      updateActionButton(session, "sample", label = "<- Back")
      updateFactorsSelectize()
      
    } else {
      globalVars$sample <- FALSE
      globalVars$dataset <- NULL
      shinyjs::show("file_upload")
      shinyjs::hide("choose_sample")
      
      if(!is.null(input$file_upload)){
        inFile <<- upload_data() # Weird this is used
        shinyjs::show("select_factors")
        updateFactorsSelectize()
        
      }else{
        shinyjs::hide("select_factors")
        globalVars$dataset <- NULL
        globalVars$dataset.original <- NULL
        globalVars$fcts <- NULL
        updateSelectizeInput(session, "select_factors",
                             "Specify Categorical Variables in the Data:",
                             choices = c(""),
                             selected = NULL)
      }
      updateActionButton(session, "sample", label = "Sample Data")
    }
    
    
  })
  
  
  
  #############################################################################################
  # When factors are selected -- update dataset
  #############################################################################################
  
  can.be.numeric <- function(x) {
    stopifnot(is.atomic(x) || is.list(x)) # check if x is a vector
    numNAs <- sum(is.na(x))
    numNAs_new <- suppressWarnings(sum(is.na(as.numeric(as.character(x))))) # if already a factor as.numeric works
    return(numNAs_new == numNAs) 
    # If the number of NAs stayed the same, it means everything in that column was successfully turned into a number. The function returns TRUE.
    # overall checks if col can be classified as a numerical number
  }
  
  
  observeEvent(input$select_factors,{ # Whole point of this is to update data to speciefy which ones are what, I want to also add fact and numerical ontop of each header in future
    globalVars$changed.input <- TRUE
    req(globalVars$dataset)
    updateTabsetPanel(session, "workPanel", selected = "data") # we just go back to data
    # So we reset
    globalVars$model <- NULL
    hideAllTabs()
    hideInteractionInput()
    uncheckAllAssumptions()
    
    #set factors in data
    factors<-input$select_factors
    dat<-globalVars$dataset.original
    
    factors.to.check <- setdiff(factors, globalVars$fcts)
    # are there factors to erase?
    factors.to.revert <- setdiff(globalVars$fcts, factors)
    
    for(var in union(factors, globalVars$fcts)){
      # Check if the factor is new
      if(var %in% factors.to.check){
        unique.categories <- unique(dat[[var]])
        if(can.be.numeric(unique.categories) & (length(unique.categories) >= 10)){ 
          shinyalert("Warning!", 
                     text = paste("You have specified", var, "as a categorical variable. The variable is numerically coded and has", length(unique.categories), "unique observations. If this was a mistake, simply remove", var, "from the list of categorical variables."), 
                     type = "warning")
        } 
      }
      
      # Change the variable type in the dataset
      if(var %in% factors.to.revert){
        if(can.be.numeric(dat[[var]])){
          dat[[var]] = as.numeric(as.character(dat[[var]]))
        }else{
          shinyalert("Warning!", 
                     text = paste("You have removed", var, "from the list of categorical variable(s), but it can not be converted to a numeric variable."), 
                     type = "warning")
          updateSelectizeInput(session, "select_factors",
                               "Specify Categorical Variables:",
                               choices = colnames(dat),
                               selected = c(factors, var))
        }
      }else{
        dat[[var]] = factor(dat[[var]])  
      }
      
    }
    
    #update dataset
    globalVars$dataset <- dat
    globalVars$fcts <- input$select_factors
    # CHECK THIS -- IF I CREATE THE REGRESSION FUNCTION AND THEN SELECT A NEW
    #               FACTOR IT APPEARS THIS WILL CHANGE THE SCALE INPUT
    globalVars$scaleDataUpToDate <- FALSE
    updateCheckboxInput(session, "scalevars", value = FALSE)
    
  }, ignoreNULL=FALSE)
  
  observeEvent(input$equation,{
    globalVars$changed.input <- TRUE
    updateTabsetPanel(session, "workPanel", selected = "data")
    globalVars$model <- NULL
    hideAllTabs()
    hideInteractionInput()
    uncheckAllAssumptions()
  })
  
  ## Will look at transforming later for now wanted to scale in server 276- 387
  
  
  #scale_section  ------------------------------------------------------------
  
  ##############################################################################
  # HANDLE SCALE VARIABLES AND TRANSFORMATION COLLISION
  ##############################################################################
  remove_response_scaling <- function(equation){
    
    # Try to parse variables safely
    vars <- try(all.vars(as.formula(equation)), silent = TRUE)
    
    # If equation is malformed:
    if (inherits(vars, "try-error")) return(NULL) #Exit early, can't process equation. ADD ERROR MODAL??
    
    suffix <- ".scaled"
    scaled <- equation
    
    # Only strip suffix from exact variable names
    pattern <- paste0("\\b", vars[1], "\\b")
    unsuffixed <- sub(paste0(suffix, "$"), "", vars[1])
    # won't change anything if pattern isn’t found
    scaled <- gsub(pattern, unsuffixed, scaled)
    
    globalVars$equation <- scaled
    return(scaled)
  }
  
  ##############################################################################
  # HANDLE SCALE VARIABLES DATSET CHANGE
  ##############################################################################
  prepare_scaled_data <- metaReactive2({
    dat <- globalVars$dataset
    if(input$scalevars==TRUE){
      metaExpr({
        "####################################"
        "# Create Scaled Data"
        "####################################"
        dat <- dat %>%
          mutate(across(where(is.numeric), 
                        ~ as.numeric(scale(.x)),
                        .names = "{.col}.scaled"))
      })
    }
  }, inline=TRUE)
  
  ##############################################################################
  # SCALE VARIABLES CHECKBOX STATE CHANGED
  ##############################################################################
  observeEvent(input$scalevars, {
    
    ### Kick the user back to the start
    globalVars$changed.input <- TRUE
    hideInteractionInput()
    updateTabsetPanel(session, "main", selected = "data")
    hideAllTabs()
    uncheckAllAssumptions()
    
    ### update dataset if needed
    if(!globalVars$scaleDataUpToDate){ # if Scaled Data isn't up to date
      if (!is.null(globalVars$dataset)){ # dataset exists
        globalVars$dataset <- prepare_scaled_data()
        globalVars$scaleDataUpToDate = TRUE
      }
    }
    
    
    ### UPDATE EQUATION
    equation <- input$equation
    
    # Try to parse variables safely
    vars <- try(all.vars(as.formula(equation)), silent = TRUE)
    
    # If equation is malformed:
    if (inherits(vars, "try-error")) return(NULL) #Exit early, can't process equation. ADD ERROR MODAL??
    
    suffix <- ".scaled"
    scaled <- equation
    
    checkEquationValidity()
    #We check the validity to get the error modals if something is wrong,
    #but we still scale things as normal because the code in this func is robust to malformed equations
    #Below we do not change variables that don't exist in the dataset
    
    if(input$scalevars){ ### user has checked the box
      
      for (v in vars){
        # if its not in the dataset, is categorical, already has a scaled tag,
        # or is a transformed variable, DO NOT ADD THE SCALED TAG
        if (is.null(globalVars$dataset[[v]]) || is.factor(globalVars$dataset[[v]]) || endsWith(v, suffix) || startsWith(v, "ihs.transformed.") || startsWith(v, "log.transformed.") || startsWith(v, "lp1.transformed.")){
          # Don't scale this variable, put it in without the suffix
          scaled <- gsub(paste0("\\b", v, "\\b"), v, scaled)
        } else {
          # Add suffix to the variable and put it in the scaled equation
          scaled <- gsub(paste0("\\b", v, "\\b"), paste0(v, suffix), scaled)
        }
      }
      
    } else { ### user has un-checked the box
      
      for (v in vars) {
        # Only strip suffix from exact variable names
        pattern <- paste0("\\b", v, "\\b")
        unsuffixed <- sub(paste0(suffix, "$"), "", v)
        
        # won't change anything if pattern isn’t found
        scaled <- gsub(pattern, unsuffixed, scaled)
      }
      
    }
    
    globalVars$equation <- scaled
    updateTextInput(session, "equation", value = globalVars$equation)
  })
  
  
  # ------------------------------------------------------------
  # Interaction
  
  #############################################################################################
  # Add variables to interaction select input
  #############################################################################################
  updateInteractionSelect <- function(){
    req(input$equation)
    updateSelectizeInput(session, "var_inter",
                         "Select Interaction",
                         choices = c("None", globalVars$interactions),
                         selected = "None")
    
  }
  
  fitmodel<-metaReactive2({
    dat<-globalVars$data
    metaExpr({
      "####################################"
      "# Fit Model"
      "####################################"
      model<-glm(..(as.formula(globalVars$equation)), data=dat)
    })
  }, inline=TRUE)
  
  #############################################################################################
  # When interaction is selected -- update interaction selectize
  #############################################################################################
  observeEvent(input$var_inter,{
    interacts <- input$var_inter
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    if(input$var_inter == "None" | is.null(input$var_inter)){
      shinyjs::hide("var_moderator")
      shinyjs::hide("interaction.error")
      if((input$var_inter=="None" | input$var_moderator=="Select...")){
        hideTab(inputId="workPanel", target="interaction")
      }
    }
    else{
      #######################################
      #Check for missing observations for interaction variables (both are categorical)
      ########################################
      var_inter1 <- str_split_1(input$var_inter, "\\:")[1]
      var_inter2 <- str_split_1(input$var_inter, "\\:")[2]
      stop <- FALSE
      if (!is.numeric(globalVars$dataset[[var_inter1]]) && !is.numeric(globalVars$dataset[[var_inter2]])){
        subbed <- str_replace_all(input$equation,fixed("+"),"~")
        subbed <- str_replace_all(subbed,fixed("*"),"~")
        subbed <- str_replace_all(subbed,fixed(":"),"~")
        variables <- trimws(str_split(string = subbed, pattern = "~", simplify = T))
        for(i in levels(globalVars$dataset[[var_inter1]])){
          for(j in levels(globalVars$dataset[[var_inter2]])){
            data.points <- globalVars$dataset %>%
              dplyr::filter(!!sym(var_inter1) == i & !!sym(var_inter2) == j & !is.na(!!sym(variables[1])))
            if(nrow(data.points) == 0){
              shinyalert("Error!", text = "There are missing combinations of the two interaction variables selected.", type = "error")
              return(NULL)
              stop <- TRUE
              break
            }
          }
          if(stop){break}
        }
      }
      shinyjs::show("var_moderator")
      updateSelectizeInput(session, "var_moderator",
                           "Select Moderator",
                           choices = c("Select...", interact.vars),
                           selected = "Select...")
      
      if((input$var_inter=="None" | input$var_moderator=="Select...")){
        hideTab(inputId="workPanel", target="interaction")
      }
    }
    
  })
  
  observeEvent(input$var_moderator,{
    req(globalVars$model)
    shinyjs::hide("interaction.error")
    updateCheckboxInput(session, "interaction.error", value =TRUE)
    
    if(input$var_moderator == "Select..."){
      hideTab(inputId="workPanel", target="interaction")   
    }else{
      showModal(modalDialog("Things are happening in the background!", footer=NULL))
      globalVars$emmeans <- prepare_interaction_emmeans()
      globalVars$mod.emmeanscontrast <- prepare_interaction_emmeanscontrasts()
      globalVars$mod.emtrends <- prepare_interaction_emtrends()
      if(!is.null(globalVars$mod.emtrends)){
        globalVars$mod.emtrendcontrast <- prepare_interaction_emtcontrast()
        globalVars$make_interaction_emtrendscontrast_table <- make_interaction_emtrendscontrast_table()
        globalVars$prepare_interaction_emtrendscontrast_interp <- prepare_interaction_emtrendscontrast_interp()
        globalVars$make_interaction_emtrends_tab <- make_interaction_emtrends_tab()
        globalVars$prepare_interaction_emtrends_interp <- prepare_interaction_emtrends_interp()
      }
      globalVars$make_interaction_emmeans_tab <- make_interaction_emmeans_tab()
      globalVars$prepare_interaction_emmeans_interp <- prepare_interaction_emmeans_interp()
      globalVars$make_interaction_contrasts_tab <- make_interaction_contrasts_tab()
      globalVars$prepare_interaction_emmeanscontrast_interp <- prepare_interaction_emmeanscontrast_interp()
      globalVars$ggemmeansplot <- ggemmeansplot()
      globalVars$jnplot <- jnplot()
      globalVars$prepare_jn_int <- prepare_jn_int()
      removeModal()
      showTab(inputId="workPanel", target="interaction")
    }
  })
  
  
    observeEvent(input$interaction.error,{
    if(!globalVars$changed.input & (input$var_inter!="None" & input$var_moderator!="Select...")){
      globalVars$ggemmeansplot <- ggemmeansplot()
    }
  })
  
  
  
  #############################################################################################
  # When a new sample selected
  #############################################################################################
  observeEvent(input$sample_data_choice,{
    globalVars$changed.input <- TRUE
    if(globalVars$sample){
      if(input$sample_data_choice=="Palmer Penguins"){
        library(palmerpenguins)
        dat<-data.frame(penguins)
      }else if(input$sample_data_choice=="Bracht et al. MFAP4" ){
        dat<-read.csv("www/BrachtMFAP4Data.csv")%>%
          mutate(Age=as.numeric(Age))
      }else if(input$sample_data_choice=="U.S. News College Data"){
        library(ISLR)
        dat<-College
      }else if(input$sample_data_choice=="Camera Data" ){
        dat<-read.csv("www/cs_replication_data.csv")
      }
      shinyjs::show("select_factors")
      globalVars$dataset <- dat %>% mutate_if(is.character,as.factor)%>%
        mutate_if(is.integer,as.numeric)
      globalVars$dataset.original <- globalVars$dataset
      
      updateFactorsSelectize()
      hideInteractionInput()
      emptyEquation()
      uncheckAllAssumptions()
      hideAllTabs()
    }
  })
  
  ########################################
  # Check Equation Input
  ########################################
  
  checkEquationValidity <- function(){
    
    run <- TRUE
    dat <- globalVars$dataset
    
    if (run & grepl("~", input$equation)) {
      subbed <- str_replace_all(input$equation,fixed("+"),"~")
      subbed <- str_replace_all(subbed,fixed("*"),"~")
      subbed <- str_replace_all(subbed,fixed(":"),"~")
      variables <- trimws(str_split(string = subbed, pattern = "~", simplify = T))
      
      if(length(variables)>=2 && variables[2]!=""){
        globalVars$response <- variables[1]
        
        if(!(substr(globalVars$response, start=0, stop=16) %in% c("ihs.transformed.", "log.transformed.", "lp1.transformed."))){
          globalVars$transform.type <- "none"
          
          globalVars$dataset <- globalVars$dataset %>%
            dplyr::select(-starts_with("ihs.transformed.")) %>%
            dplyr::select(-starts_with("log.transformed.")) %>%
            dplyr::select(-starts_with("lp1.transformed."))
        }
        
        globalVars$equation <- input$equation
        
        #Check for misspellings 
        if(!all(variables %in% colnames(globalVars$dataset))){
          run <- FALSE
          shinyalert("Error!", text = "One of the variables in your equation does not exist in the data set. Please check your equation again for spelling or other errors.", type = "error")          
          # check for correct response
        }else if(!(is.numeric(globalVars$dataset[[variables[,1]]]))){
          run <- FALSE
          shinyalert("Error!", text = "Please make sure to choose a numeric response variable.", type = "error")
          # check for bad categorical variables
        }else{
          # Check for too many levels on explanatory variables
          for (i in 2:length(variables)){
            if(!is.numeric(globalVars$dataset[[variables[i]]])){
              if (length(unique(as.character(globalVars$dataset[[variables[i]]]))) > 12){
                shinyalert("Error!", text = "One of your categorical predictor variables has more than the maximum supported number of unique levels (12). Please check that the predictor is indeed categorical or adjust your regression equation.", type = "error")
                run <- FALSE
                break
              }
            }
          } 
        }
      }else{
        run <- FALSE
        shinyalert("Error!", text="Your regression equation is not correctly specified, please rewrite it.", type = "error")
      }
    }else{
      run <- FALSE
      shinyalert("Error!", text="Your regression equation is not correctly specified, please rewrite it.", type = "error")
    }
    return(run)
  }
  
  
  #############################################################################################
  # When a new sample selected
  #############################################################################################
  observeEvent(input$DoCompute,{
    if(is.null(globalVars$dataset)){
      shinyalert("Error!", text = "Please choose a dataset.", type = "error")
    }
    req(globalVars$dataset)
    
    ########################################
    # Significance/confidence
    ########################################
    if(((!is.numeric(input$alpha))|is.null(input$alpha))){
      run <- FALSE
    }else{
      if((input$alpha<=0)|(input$alpha>=1)){
        run <- FALSE
        updateNumericInput(session, "alpha", value = 0.05)
        shinyalert("Error!", text = "The significance level must be a number between 0 and 1. It has been reset to the default of 0.05.", type = "error")
      }
    }
    
    ########################################
    # We changed the input, so try again
    ########################################
    if(globalVars$changed.input){
      run <- checkEquationValidity()
      
      
      
      if(run){
       # print(head(globalVars$dataset)) # Shows the first 6 rows
        
        
        # Try to fit model
        model <- tryCatch({
          
          model<-glm((as.formula(globalVars$equation)), data=globalVars$dataset)
          
          # Change the call to have the full formula -- this is imporant for later. It is usually unnecessary but it is important
          # for the emmeans code -- this is how it checks whether a log transformation was used.
          model$call <- str2lang(paste0("lm(formula=",globalVars$equation, ",data=globalVars$dataset)"))

          model
        }, error = function(e){
          #
          if(grepl(pattern = "Error in eval(predvars, data, env): object", x = e, fixed = T)){
            # run <- FALSE
            shinyalert("Error!", text="You may have misspelled one of the variables. Please rewrite the regression equation.", type = "error")
          }else if(grepl(pattern = "Error in str2lang(x)", x = e, fixed=T)){  
            # run <- FALSE
            shinyalert("Error!", text="You may have misspelled one of the variables. Please rewrite the regression equation.", type = "error")
          }else if(grepl(pattern = "singular fits are not implemented", x = e, fixed=T)){  
            # run <- FALSE
            shinyalert("Error!", text="Your specified model has singular fit. This typically occurs when there is multicollinearity among predictor variables or too many predictors in the model. Please rewrite the regression equation, removing variables that cause the singularity.", type = "error")
          }else{
            # run <- FALSE
            shinyalert("Error!", text="There was an unanticipated error in fitting your regression model. Please report the issue and/or try another regression equation.", type = "error")
            safeError(e)
          }
        }, warning = function(w){
          #
          # catch when user has a response as a predictor
          if(as.character(w)=="simpleWarning in model.matrix.default(mt, mf, contrasts): the response appeared on the right-hand side and was dropped\n"){
            # run <- FALSE
            shinyalert("Error!", text="The response has been mistakenly referenced as a predictor. Please rewrite the regression equation.", type = "error")
          }else if(grepl(pattern = 'Warning in model.response(mf, "numeric"): using type = "numeric" with a factor response will be ignored', x = w, fixed = T)){
            # run <- FALSE
            shinyalert("Error!", text="You may have specified a non-numeric response variable. Please rewrite the regression equation.", type = "error")
          }else if(grepl(pattern = '<simpleError in model.frame.default(formula = (as.formula(globalVars$equation))', x = w, fixed = T)){
            # run <- FALSE
            shinyalert("Error!", text="You may have misspelled one of the variables. Please rewrite the regression equation.", type = "error")
          }else{
            # run <- FALSE
            shinyalert("Error!", text="There was an unanticipated error in fitting your regression model. Please report the issue and/or try another regression equation.", type = "error")          }
        })
        
        
        
        # 1. Check if model is NULL first
        if (is.null(model)) {
          print("DEBUG: Model object is NULL!")
          return(NULL)
        }
        
        # 2. Check if it actually has 'terms' (what model.matrix needs)
        if (is.null(terms(model))) {
          print("DEBUG: Object exists but has no 'terms'. It might not be a fitted model.")
          return(NULL)
        }
        
        model_matrix <- model.matrix(model)
        
        
        # Check the rank of the model matrix < number of predictors?
        model_matrix <- model.matrix(model)
        if(qr(model_matrix)$rank < (ncol(model_matrix))){
          shinyalert("Error!", text="Coefficients in your model are undefined due to signular fit. This can happen when there are missing combinations of observed categorical variables in a model with an interaction.", type = "error")
          return(NULL)
        }
        
        if(!("lm" %in% class(model))){
          run <- FALSE
        }
        
      }
      
      #         globalVars$modelsummary <- prepare_model_summary()

      
      
      
      if(run){ # NOTE THIS IS WHERE ALL PLOTS AND THINGS HAPPEN
        showModal(modalDialog("Things are happening in the background!", footer=NULL))
        globalVars$model <- model
        
        globalVars$modelsummary <- prepare_model_summary()
        globalVars$make_model_summary <- make_model_summary()
        
        
        #NOTE Should plot stuff
        
        globalVars$RQRPlot <- tryCatch({ # (NOTE PLOT 5 need to call this btw) 
          RQRPlot()
        }, error = function(e) {
          shinyalert("Error!", text = "There was an issue making the RQR Plot.", type = "error")
          NULL
        })
        
        
        
    
        
        showAllTabs()
        updateTabsetPanel(session, "workPanel", selected = "plot")
        removeModal()
        # Hide tabs and do nothing further if model is not fit
        globalVars$changed.input <-FALSE
        
      }else{
        globalVars$model <- NULL
        hideAllTabs()
      }
      

    }
    else{
      shinyalert("Warning!", 
                 text = "You have not changed the input. Your results are up to date.", 
                 type = "warning")
    }
    
  })
  
  # From here we might be by ourselves
  
  
  
  #############################################################################################
  # Model Summary
  #############################################################################################
  prepare_model_summary <- metaReactive2({ 
    req(globalVars$model)
    
    dat <- globalVars$dataset
    model <- globalVars$model
    metaExpr({
      "####################################"
      "#Summarize Model"
      "####################################" 
      mod.sum <- summary(model)
      
      "####################################"
      "# Add Confidence Intervals"
      "####################################"  
      mod.table<-cbind(mod.sum$coefficients, na.omit(confint(model)))
      
      "####################################"
      "# Clean Up Labels for Printing"
      "####################################"
      mod.classes<-sapply(X = model$model, FUN = class)[-1]
      mod.classes<-mod.classes[which(mod.classes=="factor")]
      for(vname in names(mod.classes)){
        ind <- grepl(x=rownames(mod.table), pattern = vname)
        ind.int <- grepl(x=rownames(mod.table), pattern = ":")
        
        indexes.int<- ind & ind.int
        indexes <- xor(ind, indexes.int)
        
        varval  <- str_remove_all(rownames(mod.table)[indexes], vname)
        rownames(mod.table)[indexes]<-paste(vname, " = ", varval, sep="")
        
        if(any(indexes.int)){
          for(i in which(indexes.int)){
            intname <- rownames(mod.table)[i]
            vname.unique <- str_replace(string = intname, pattern = paste(rownames(mod.table)[(!indexes)&(!ind.int)],collapse ="|"), replacement = "")
            varval  <- str_remove_all(vname.unique, pattern = vname)
            varval  <- str_remove_all(varval, pattern = "[:]")
            
            rname<-str_remove_all(string = rownames(mod.table)[i], pattern = vname)
            rname<-str_remove_all(string = rname, pattern = varval)
            rname<-str_remove_all(string = rname, pattern = "[:]")
            
            rownames(mod.table)[i]<- paste(rname," : (",vname, " = ", varval,")", sep="")
          }
        }
      }
      mod.table <-  data.frame(mod.table) %>% 
        mutate("Term" = rownames(.)) %>%  # make terms part of table
        relocate(Term) %>%                # put Term in first column
        set_rownames(NULL) %>%
        set_colnames(c("Term", "Estimate", "SE", "t", "p-value", "Lower CI", "Upper CI"))
    })
  }, inline=TRUE)
  
  make_model_summary <- metaReactive2({
    
    req(globalVars$modelsummary)
    mod.table <- globalVars$modelsummary
    metaExpr({
      "####################################"
      "# Print Summary"
      "####################################" 
      mod.table %>% 
        mutate_if(is.numeric, round, 4) %>%
        mutate(`p-value` = ifelse(`p-value` < 0.0001, "<0.0001", `p-value`))
      
    })
  }, inline=TRUE)
  
  # Render table to UI  
  output$modsumTab <- DT::renderDataTable({ # Where we named table Tom
    globalVars$make_model_summary
  },
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = 
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend="csv", filename="model-summary"),
          list(extend="excel", filename="model-summary"),
          list(extend="pdf", filename="model-summary")
        ),
        text = "Download",
        filename = "model-summary"
      ))
  ), rownames = FALSE)
  
  # Download button for summary (LaTeX version)----
  output$downloadmodsumLatex <- downloadHandler(
    filename = function() {
      paste("model-summary.tex", sep="")
    },
    content = function(file) {
      base::print(xtable(globalVars$make_model_summary, digits = 4), file, type = "latex")
    }
  )
  
  ##########################################################
  # RQR Plots
  ##########################################################
  
RQRPlot <- metaReactive2({ # For pois (NOTE PLOT 1) 
  req(globalVars$model)
  dat <- globalVars$dataset
  model <- globalVars$model
  
  library('patchwork')
  library('emmeans')
  
  metaExpr({
    "####################################"
    "# Create Data for PDF of Residuals Plot"
    "####################################"
  counts <- model$y
  lambdas <- fitted(model)
  rqr <- rep(NA, length(lambdas))
  
  
  for(i in 1:length(lambdas)){
    ai <- ppois(counts[i]-1,lambda=lambdas[i])
    bi <- ppois(counts[i], lambda=lambdas[i])
    ui <- ai+runif(1)* (bi-ai)
    ui <- max(min(ui,1-10^(-6)),10^(-6))
    rqr[i] <- qnorm(ui)
  }
  
  pearson.ratio <- sum(residuals(model, type= "pearson")^2)/model$df.residual
  p1 <- ggplot(data=tibble(lambda=lambdas,e=rqr)) +
    geom_hline(yintercept=0,linetype="dotted")+
    geom_point(aes(x=lambda, y=e))+
    theme_bw()+
    xlab(bquote(lambda))+
    ylab("RandomizedQuantileResiduals")
  
  p2 <- ggplot(data=tibble(e=rqr)) +
    stat_qq(aes(sample=e)) +
    stat_qq_line(aes(sample=e))+
    theme_bw()+
    xlab("Theoretical")+
    ylab("Observed")+
    ggtitle(paste("DispersionRatio=",round(pearson.ratio,4)))
  
  p1+p2
  
  })
  
#  browser() we use this to pause app at a part to help debug
  
    
})
  

# Render plot to UI
output$RQR_plot <- renderPlot({ # This part and bellow is specific to UI (NOTE PLOT 2) 
  globalVars$RQRPlot
})


# Download button for plots (call reactive function here to get plot object) ---- (NOTE PLOT 3) 
output$downloadRQRPlot <- downloadHandler(  
  filename = function() { paste('RQRplot.', input$RQR_plot_format, sep='') },
  content = function(file) {
    ggsave(file, plot = globalVars$RQRPlot, device = input$RQR_plot_format, 
           height = as.numeric(input$RQR_plot_height), width = as.numeric(input$RQR_plot_width), 
           units = input$RQR_plot_units)
  }
)


# Display code for ci visualization plot ---- (NOTE PLOT 4) 
observeEvent(input$code_RQR, {
  code <- expandChain(
    "# Ensure to load your data as an object called dat.",
    quote({
      library(tidyverse) 
      library(ggeffects)
    }),
    "####################################",
    "# Load Data",
    "####################################",
    read_data(),
    refactor_data(),
   # prepare_transformed_data(), still gotta think abt this
  #  prepare_scaled_data(),
    fitmodel(),
    RQRPlot()
  )
  
  displayCodeModal(
    code, 
    title = "Interaction Marginal Effects Plot",
    size = "l", 
    fontSize = 16,
    clip=NULL
  )
})


refactor_data <- metaReactive2({
  if(length(input$select_factors) > 0){
    metaExpr({
      "####################################"
      "# Specify factor variables"
      "####################################"
      dat <- dat %>%
        mutate(across(..(input$select_factors), as.factor))
    })
  }
},inline=TRUE)

}
)
#NOTE: HI LEO!
#Alright leo, just wanna add this here cuz I spent about 2 hrs cross-referencing his code with ours
# 1. He appears to have an input dataframe/tibble that I can't seem to find the source for. 
# 2. We have code for unchecking the assumption boxes, but those don't seem to exist. I wanna run that by ya first
# because that's gonna be a massive code update. Tmr, I'll see if I can make a branch to experiment with some stuff, but that's
# a whole can of worms that I'll need about 2-3 hours to start deciphering ngl
# 3. In his model, there's several things in UI that check and uncheck the boxes, which are then used as boolean values in server (which is weird)
# Look for anything that uses the "CheckboxInput()" function in his code. We do have a few instances in our code too.
# 4. In his code, there is one confusing bit with the checkbox inputs, and that's that I never see it updated to true. Im gonna ask him on Monday abt it.
