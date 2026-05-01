
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
  
  
  globalVars$model_choice <- "Poisson" # New
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
  
  #---------------------------------------------
  # (NOTE PLOT) 1 Make sure to instantiate plots
  #---------------------------------------------
  
  globalVars$RQRPlot <- NULL
  globalVars$Pearson_Residual <- NULL
  globalVars$ZeroInflated <- NULL
  
  
  
  globalVars$jnplot <- NULL
  globalVars$prepare_jn_int <- NULL
  
  globalVars$modelmargins <- NULL
  globalVars$make_margins_summary <- NULL
  globalVars$prepare_margins_interp <- NULL
  
  globalVars$changed.input <- TRUE  
  globalVars$changed.model.input <- TRUE #New
  
  
  globalVars$scaleDataUpToDate <- FALSE
  
  
  observe({
    session$onFlushed(function() {
      shinyjs::runjs("
      $('#equation')
        .attr('data-original-title', 'Example: response ~ x1 + x2')
        .tooltip({
          trigger: 'hover',
          placement: 'right'
        });
    ")
    }, once = TRUE)
  })
  
  
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
  hideTab(inputId="workPanel", target="plot")
  
  
  
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
    globalVars$dataset <- dat %>% 
      mutate_if(is.character, as.factor) %>%
      mutate_if(is.integer, as.numeric) %>%
      tidyr::drop_na()
    
    globalVars$dataset.original <- globalVars$dataset
    
    updateFactorsSelectize()
    emptyEquation()
  })
  
  
  ##############################################
  # DATASET PREVIEW
  ##############################################
  output$preview.data <- DT::renderDataTable({ # this gets called from ui when we want to render dataset e.r call oreview
    if(globalVars$sample){ #Edit this for sample data
      filename <- case_when(input$sample_data_choice=="Kitsberg et al. Nucleus"                             ~ "BrachtMFAP4Data",
                            input$sample_data_choice=="Palmer Penguins"                                 ~ "PalmerPenguin",
                            input$sample_data_choice=="U.S. News College Data"                          ~ "College",
                            input$sample_data_choice=="Camera Data"                                     ~ "cs_replication_data",
                            input$sample_data_choice=="Ache Monkey"                                     ~ "Ache Monkey",
                            input$sample_data_choice=="Tweedie Simulated Data"                          ~ "tweedie_regression_data"
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
    inFile <- upload_data() # saved to global var
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
      if(input$sample_data_choice=="Kitsberg et al. Nucleus"){
        metaExpr({
          "# You can download the data on the Data Preview page"
          dat<-read_csv("www/Kitsberg25-nucleus.csv")
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
          dat<-read_csv("www/cs_replication_data.csv")
        })
      }else if(input$sample_data_choice=="Ache Monkey"){
        metaExpr({
          dat<-read_csv("www/McMillanAcheMonkey.csv")
        })
      } else if (input$sample_data_choice=="Tweedie Simulated Data"){
        metaExpr({
          dat<-read_csv("www/tweedie_regression_data.csv")
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
  dznbinom <- function(y, mu, theta, pi) {
    probs <- numeric(length(y))
    probs[y == 0] <- pi[y == 0] + (1 - pi[y == 0]) * (theta / (mu[y == 0] + theta))^theta
    probs[y > 0] <- (1 - pi[y > 0]) * dnbinom(y[y > 0], size = theta, mu = mu[y > 0])
    return(probs)
  }
  
  pznbinom <- function(q, mu, theta, pi) {
    probs <- numeric(length(q))
    if(length(pi) == 1) pi <- rep(pi, length(q))
    if(length(mu) == 1) mu <- rep(mu, length(q))
    probs[q < 0] <- 0
    idx <- q >= 0
    probs[idx] <- pi[idx] + (1 - pi[idx]) * pnbinom(q[idx], size = theta, mu = mu[idx])
    
    return(probs)
  }
  
  dzpois <- function(y, lambda, pi) {
    probs <- numeric(length(y))
    n <- length(y)
    probs[y == 0] <- pi[y == 0] + (1 - pi[y == 0]) * exp(-lambda[y == 0])
    probs[y > 0] <- (1 - pi[y > 0]) * dpois(y[y > 0], lambda[y > 0])
    return(probs)
  }
  
  pzpois <- function(q, lambda, pi) {
    probs <- numeric(length(q))
    probs[q < 0] <- 0
    idx <- q >= 0
    probs[idx] <- pi[idx] + (1 - pi[idx]) * ppois(q[idx], lambda[idx])
    
    return(probs)
  }
  
  
   # convert scientific notation to 10^
  changeSciNot <- function(n) {
    output <- format(n, scientific = TRUE) #Transforms the number into scientific notation even if small
    output <- sub("e", "*10^", output) #Replace e with 10^
    output <- sub("\\+0?", "", output) #Remove + symbol and leading zeros on expoent, if > 1
    output <- sub("-0?", "-", output) #Leaves - symbol but removes leading zeros on expoent, if < 1
    output
  }
  
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
  
  hideAllTabs <- function(){
    updateTabsetPanel(session, "workPanel", selected="data")
    
    hideTab(inputId="workPanel", target="summary")
    hideTab(inputId="workPanel", target="assumptions")
    hideTab(inputId="workPanel", target="checks")
    hideTab(inputId="workPanel", target="anova")
    hideTab(inputId="workPanel", target="interpretation")
    hideTab(inputId="workPanel", target="interaction")
    hideTab(inputId="workPanel", target="plot")
    
  }
  
  showAllTabs <- function(){
    showTab(inputId="workPanel", target="summary")
    showTab(inputId="workPanel", target="assumptions")
    showTab(inputId="workPanel", target="checks")
    showTab(inputId="workPanel", target="anova")
    showTab(inputId="workPanel", target="interpretation")
    showTab(inputId="workPanel", target="plot")
    
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
    shinyjs::hide("asmp_5Anote")
    shinyjs::hide("asmp_5Bnote")
    shinyjs::hide("asmp_6note")
    shinyjs::hide("all")
    shinyjs::hide("asmp_note")
    updateCheckboxInput(session, "asmp_1", value = FALSE)
    updateCheckboxInput(session, "asmp_2", value = FALSE)
    updateCheckboxInput(session, "asmp_3", value = FALSE)
    updateCheckboxInput(session, "asmp_4", value = FALSE)
    updateCheckboxInput(session, "asmp_5A", value = FALSE)
    updateCheckboxInput(session, "asmp_5B", value = FALSE)
    updateCheckboxInput(session, "asmp_6", value = FALSE)
    
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
      }else if(input$sample_data_choice=="Kitsberg et al. Nucleus" ){
        dat<-read.csv("www/Kitsberg25-nucleus.csv")
      }else if(input$sample_data_choice=="U.S. News College Data"){
        library(ISLR)
        dat<- College
      }else if(input$sample_data_choice=="Camera Data"){
        dat<-read.csv("www/cs_replication_data.csv")
      }else if (input$sample_data_choice=="Ache Monkey"){
        dat<-read.csv("www/McMillanAcheMonkey.")
      }else if (input$sample_data_choice=="Tweedie Simulated Data"){
        dat<-read.csv("www/tweedie_regression_data.csv")
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
        inFile <- upload_data() # Weird this is used
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
  
  
  observeEvent(input$select_factors,{ # Whole point of this is to update data to specify which ones are what, I want to also add fact and numerical ontop of each header in future
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
  
  ############################################################################################
  # All models 
  ############################################################################################
  fitmodel <-metaReactive2({
    req(globalVars$model_choice, globalVars$equation)
    
    dat<-globalVars$data
    formula <- as.formula(globalVars$equation)
    mod_type <- globalVars$model_choice
    print(mod_type)
    print(formula)
    metaExpr({
      "####################################"
       paste("# Fit Model:", ..(mod_type))
      "####################################"

      model <- ..(
        if (mod_type == "Poisson") {
          # Standard Poisson
          quote(glm(formula, data = dat, family = poisson(link = "log")))
        }
        else if (mod_type == "Negative Binomial") {
          # Requires library(MASS)
          quote(MASS::glm.nb(formula, data = dat))
        } else if (mod_type == "Quasi-Poisson") {
          # Overdispersion adjustment
          quote(glm(formula, data = dat, family = quasipoisson(link = "log")))
          
        } else if (mod_type == "Zero-Inflated Poisson") {
          # Requires library(glmmTMB)
          quote(glmmTMB(formula, ziformula,data = dat, dist = "poisson")) #Might change formula remove pscl
          
        } else if (mod_type == "Zero Inflated Negative Binomial") {
          # Requires library(glmmTMB)
          quote(glmmTMB(formula, ziformula, data = dat, family = "nbinom2"))
          
        } else if (mod_type == "Tweedie") {
          # Requires library(tweedie)
          # Usually family = tweedie(var.power=1.5, link.power=0)
          quote(glm(formula, data = dat, family = statmod::tweedie(var.power = 1.5, link.power = 0)))
        })
      
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
   
#############################################################################
  # Start of Pain   #AHHH
  #########################################################################
  observeEvent(input$var_moderator,{
    req(globalVars$model)
    shinyjs::hide("interaction.error")
    updateCheckboxInput(session, "interaction.error", value =TRUE)
    
    if(input$var_moderator == "Select..."){
      hideTab(inputId="workPanel", target="interaction")   
    }else{
      showModal(modalDialog("Things are happening in the background!", footer=NULL))
      globalVars$emmeans <- prepare_interaction_emmeans() # Good
      globalVars$mod.emmeanscontrast <- prepare_interaction_emmeanscontrasts() # Good
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
      globalVars$jnplot <- jnplot() # Here where it fails for ZIP TOm FIX
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
  # ggemmeans plot
  #############################################################################################
  # Code to create ggemmeanss plot
  ggemmeansplot <- metaReactive2({
    
    req(globalVars$model)
    model <- globalVars$model
    
    dat <- globalVars$dataset
    
    se.on = ifelse(input$interaction.error==TRUE, 0.40, 0)
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    
    if(globalVars$transform.type == "none"){
      y.label <- paste("Predicted ", names(model.frame(model))[1],sep="")
    }else if(globalVars$transform.type == "log"){
      y.label <- paste("Predicted log(", substring(names(model.frame(model))[1], first=17), ")", sep="")  
    }else if(globalVars$transform.type == "lp1"){
      y.label <- paste("Predicted log(", substring(names(model.frame(model))[1], first=17), " + 1)", sep="")
    }else if(globalVars$transform.type == "ihs"){
      y.label <- paste("Predicted IHS(", substring(names(model.frame(model))[1], first=17), ")", sep="")
    }
    
    if(all(int.vars.classes=="numeric")){ #Both Continuous
      shinyjs::show("emtrendsdiv")
      shinyjs::show("emtrendscontrastdiv")
      shinyjs::show("jndiv")
      shinyjs::show("interaction.error")
      metaExpr({
        "####################################"
        "# Effects of Interest"
        "####################################"
        m.mod<-mean(unlist(model.frame(model)[..(moderator)]), na.rm=T)
        s.mod<-sd(unlist(model.frame(model)[..(moderator)]), na.rm=T)
        meffectsfor <- c(..(int.var), paste(..(moderator), "[",round(m.mod-s.mod,2), ",", round(m.mod+s.mod,2),"]", sep=""))
        mod.emmeans<-ggemmeans(model = model, terms = meffectsfor , interval = "confidence") # checkbox
        mod.emmeans <- ggemmeans(model = model, terms = meffectsfor, interval = "confidence")
        ggdat <- data.frame(mod.emmeans) %>%
          mutate(group = case_when(group == round(m.mod - s.mod, 2) ~ paste("Low (Mean - 1SD = ", round(m.mod - s.mod, 2), ")", sep = ""),
                                   TRUE                             ~ paste("High (Mean + 1SD = ", round(m.mod + s.mod, 2), ")", sep = "")))
        "####################################"
        "# Plot"
        "####################################"
        ggplot(data = ggdat, aes(x = x, y = predicted, color = group, fill = group)) +
          geom_line() +
          geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                      linetype="dotted",
                      alpha = ifelse(..(se.on), 0.4, 0)) +
          xlab(..(int.var))+
          ylab(..(y.label))+
          scale_color_brewer(..(moderator), palette = "Pastel1")+
          scale_fill_brewer(..(moderator), palette = "Pastel1")+
          theme_bw()
      })
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      shinyjs::hide("emtrendsdiv")
      shinyjs::hide("emtrendscontrastdiv")
      shinyjs::hide("jndiv")
      shinyjs::hide("interaction.error")
      metaExpr({
        "####################################"
        "# Effects of Interest"
        "####################################"
        #mod.emmeans<-ggemmeans(model = model, terms = c(..(int.var), ..(moderator)) , interval = "prediction") # checkbox
        mod.emmeans<-ggemmeans(model = model, terms = c(..(int.var), ..(moderator)) , interval = "confidence") # checkbox
        
        "####################################"
        "# Plot"
        "####################################"
        ggdat <- data.frame(mod.emmeans)
        ggplot(data=ggdat, aes(x=x, y=predicted, color=group))+
          geom_point(position = position_dodge(.25))+
          geom_errorbar(aes(ymin=conf.low, ymax=conf.high), position = position_dodge(.25), width=0.1)+
          xlab(..(int.var))+
          ylab(..(y.label))+
          scale_color_brewer(..(moderator), palette = "Pastel1")+
          scale_fill_brewer(..(moderator), palette = "Pastel1")+
          theme_bw()
      })
    }else{ #One of Each
      if(int.vars.classes[moderator]=="factor"){
        shinyjs::show("emtrendsdiv")
        shinyjs::show("emtrendscontrastdiv")
        shinyjs::hide("jndiv")
        shinyjs::show("interaction.error")
        metaExpr({
          "####################################"
          "# Effects of Interest"
          "####################################"
          #mod.emmeans<-ggemmeans(model = model, terms = meffectsfor , interval = "prediction") # to select
          mod.emmeans<-ggemmeans(model = model, terms = c(..(int.var),..(moderator)) , interval = "confidence") # checkbox
          
          "####################################"
          "# Plot"
          "####################################"
          ggdat <- data.frame(mod.emmeans)
          ggplot(data = ggdat, aes(x = x, y = predicted, color = group, fill = group)) +
            geom_line() +
            geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                        linetype="dotted",
                        alpha = ifelse(..(se.on), 0.4, 0)) +
            xlab(..(int.var))+
            ylab(..(y.label))+
            scale_color_brewer(..(moderator), palette = "Pastel1")+
            scale_fill_brewer(..(moderator), palette = "Pastel1")+
            theme_bw()
        })
      }else{
        shinyjs::hide("emtrendsdiv")
        shinyjs::hide("emtrendscontrastdiv")
        shinyjs::hide("jndiv")
        shinyjs::hide("interaction.error")
        metaExpr({
          "####################################"
          "# Effects of Interest"
          "####################################"
          m.mod<-mean(unlist(model.frame(model)[..(moderator)]), na.rm=T)
          s.mod<-sd(unlist(model.frame(model)[..(moderator)]), na.rm=T)
          
          meffectsfor <- c(..(int.var), paste(..(moderator), "[",round(m.mod-s.mod,2), ",", round(m.mod+s.mod,2),"]", sep=""))
          #mod.emmeans<-ggemmeans(model = model, terms = meffectsfor , interval = "prediction") # to select
          mod.emmeans<-ggemmeans(model = model, terms = meffectsfor , interval = "confidence") # checkbox
          
          mod.emmeans <- mod.emmeans %>% 
            mutate(group=ifelse(group == round(m.mod-s.mod,2), paste("Low (Mean - 1SD = ", round(m.mod-s.mod,2), ")", sep=""),
                                paste("High (Mean + 1SD = ", round(m.mod+s.mod,2), ")", sep="")))
          "####################################"
          "# Plot"
          "####################################"
          ggdat <- data.frame(mod.emmeans)
          ggplot(data=ggdat, aes(x=x, y=predicted, color=group))+
            geom_point(position = position_dodge(.25))+
            geom_errorbar(aes(ymin=conf.low, ymax=conf.high), position = position_dodge(.25), width=0.1)+
            xlab(..(int.var))+
            ylab(..(y.label))+
            scale_color_brewer(..(moderator), palette = "Pastel1")+
            scale_fill_brewer(..(moderator), palette = "Pastel1")+
            theme_bw()
        })
      }
    }
  },inline=TRUE)
  
  # Render plot to UI
  output$ggemmeans_plot <- renderPlot({
    globalVars$ggemmeansplot
  })
  
  # Download button for plots (call reactive function here to get plot object) ----
  output$downloadggemmeansPlot <- downloadHandler(
    filename = function() { paste('ggemmeansplot.', input$ggemmeans_plot_format, sep='') },
    content = function(file) {
      ggsave(file, plot = globalVars$ggemmeansplot, device = input$ggemmeans_plot_format, 
             height = as.numeric(input$ggemmeans_plot_height), width = as.numeric(input$ggemmeans_plot_width), 
             units = input$ggemmeans_plot_units)
    }
  )
  
  
  # Rcode 
  # Display code for ci visualization plot ----
  observeEvent(input$code_ggemmeans, {
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
      prepare_scaled_data(),
      fitmodel(),
      ggemmeansplot()
    )
    
    displayCodeModal(
      code, 
      title = "Interaction Marginal Effects Plot",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  #############################################################################################
  # emmeans table
  #############################################################################################
  prepare_interaction_emmeans <- metaReactive2({
    req(input$var_inter)
    req(input$var_moderator)
    
    model<-globalVars$model
    
    dat <- globalVars$dataset
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    if(all(int.vars.classes=="numeric")){ #Both Continuous
      metaExpr({
        "####################################"
        "# Calculate Marginal Effects"
        "####################################"
        m.mod<-mean(unlist(model.frame(model)[..(moderator)]), na.rm=T)
        s.mod<-sd(unlist(model.frame(model)[..(moderator)]), na.rm=T)
        m.var<-mean(unlist(model.frame(model)[..(int.var)]), na.rm=T)
        s.var<-sd(unlist(model.frame(model)[..(int.var)]), na.rm=T)
        modvarat<-list(c(round(m.mod-s.mod,2), round(m.mod+s.mod,2)),
                       c(round(m.var-s.var,2), round(m.var+s.var,2)))
        names(modvarat)<-c((..(moderator)), ..(int.var))
        
        mod.emmeans<-data.frame(emmeans(object = model, spec=c(..(int.var),..(moderator)), var=..(int.var), at=modvarat, type="response")) 
        
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        mod.emmeans[,..(int.var)]<- ifelse(mod.emmeans[,..(int.var)]==round(m.var-s.var,2),     paste("Low (Mean - 1SD = ", round(m.var-s.var,2), ")", sep=""),paste("High (Mean + 1SD = ", round(m.var+s.var,2), ")", sep=""))
        mod.emmeans[,..(moderator)]<- ifelse(mod.emmeans[,..(moderator)]==round(m.mod-s.mod,2), paste("Low (Mean - 1SD = ", round(m.mod-s.mod,2), ")", sep=""),paste("High (Mean + 1SD = ", round(m.mod+s.mod,2), ")", sep=""))
        
        mod.emmeans <- mod.emmeans %>%
          set_rownames(NULL) %>%
          set_colnames(c(..(moderator),..(int.var), "Estimated Marginal Mean", "SE", "df", "Lower CI", "Upper CI"))
      })
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      metaExpr({
        "####################################"
        "# Calculate Marginal Effects"
        "####################################"
        mod.emmeans<-data.frame(emmeans(object = model, spec=c(..(int.var),..(moderator)), var=..(int.var), type="response")) 
        
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        mod.emmeans <- mod.emmeans %>%
          set_rownames(NULL) %>%
          set_colnames(c(..(moderator),..(int.var),"Estimated Marginal Mean", "SE", "df", "Lower CI", "Upper CI"))
      })
    }else{ 
      if(int.vars.classes[moderator]=="factor"){
        metaExpr({
          "####################################"
          "# Calculate Marginal Effects"
          "####################################"  
          m.var<-mean(unlist(model.frame(model)[..(int.var)]), na.rm=T)
          s.var<-sd(unlist(model.frame(model)[..(int.var)]), na.rm=T)
          modvarat<-list(c(round(m.var-s.var,2), round(m.var+s.var,2)))
          names(modvarat)<-..(int.var)
          
          mod.emmeans<-data.frame(emmeans(model, specs = c(moderator, int.var), var = int.var, at=modvarat, type="response")) 
          
          
          "####################################"
          "# Clean Up Labels for Printing"
          "####################################"
          mod.emmeans[,..(int.var)]<- ifelse(mod.emmeans[,..(int.var)]==round(m.var-s.var,2),paste("Low (Mean - 1SD = ", round(m.var-s.var,2), ")", sep=""),paste("High (Mean + 1SD = ", round(m.var+s.var,2), ")", sep=""))
          
          mod.emmeans <- mod.emmeans %>%
            set_rownames(NULL) %>%
            set_colnames(c(..(moderator),..(int.var),"Estimated Marginal Mean", "SE", "df", "Lower CI", "Upper CI"))
        })
      }else{
        model<-globalVars$model
        metaExpr({
          "####################################"
          "# Calculate Marginal Effects"
          "####################################"
          m.mod<-mean(unlist(model.frame(model)[..(moderator)]), na.rm=T)
          s.mod<-sd(unlist(model.frame(model)[..(moderator)]), na.rm=T)
          
          modvarat<-list(c(round(m.mod-s.mod,2), round(m.mod+s.mod,2)))
          names(modvarat)<-c(..(moderator))
          
          mod.emmeans<-data.frame(emmeans(model, specs = c(..(moderator),..(int.var)), var=..(moderator), at=modvarat, type="response")) 
          
          
          "####################################"
          "# Clean Up Labels for Printing"
          "####################################"
          mod.emmeans[,..(moderator)]<- ifelse(mod.emmeans[,..(moderator)]==round(m.mod-s.mod,2),paste("Low (Mean - 1SD = ", round(m.mod-s.mod,2), ")", sep=""),paste("High (Mean + 1SD = ", round(m.mod+s.mod,2), ")", sep=""))
          
          mod.emmeans <- mod.emmeans %>%
            set_rownames(NULL) %>%
            set_colnames(c(..(moderator),..(int.var),"Estimated Marginal Mean", "SE", "df", "Lower CI", "Upper CI"))
        })
      }
    }
  }, inline=TRUE)
  
  make_interaction_emmeans_tab <- metaReactive2({
    mod.emmeans <- globalVars$emmeans
    metaExpr({
      mod.emmeans %>%
        mutate_if(is.numeric, round, 4) 
    })
    
  }, inline=TRUE)
  
  ### Model Summary
  output$interaction_emmeans_tab <- DT::renderDataTable({
    globalVars$make_interaction_emmeans_tab
  },
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = 
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend="csv", filename="model-interaction-marginalmeans"),
          list(extend="excel", filename="model-interaction-marginalmeans"),
          list(extend="pdf", filename="model-interaction-marginalmeans")
        ),
        text = "Download",
        filename = "model-interaction-marginalmeans"
      ))
  ), rownames = FALSE)
  
  # Download button for summary (LaTeX version)----
  output$download_interaction_emmeansLatex <- downloadHandler(
    filename = function() {
      "model-interaction-marginalmeans.tex"
    },
    content = function(file) {
      base::print(xtable(globalVars$make_interaction_emmeans_tab, digits = 4), file, type = "latex")
    }
  )
  
  # R Code
  observeEvent(input$code_interaction_emmeanstab, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse)
        library(magrittr)
        library(emmeans)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_scaled_data(),
      fitmodel(),
      prepare_interaction_emmeans(),
      make_interaction_emmeans_tab()
    )
    
    displayCodeModal(
      code,
      title = "Interaction Estimated Marginal Means Table",
      size = "l",
      fontSize = 16,
      clip=NULL
    )
  })
  
  #Interpretation
  output$interaction_emmeans_interp <- renderUI({
    globalVars$prepare_interaction_emmeans_interp
  })
  
  prepare_interaction_emmeans_interp <- function(){
    mod.emmeans <- globalVars$emmeans %>%
      mutate_if(is.numeric, round, 4) 
    model<-globalVars$model
    dat<-globalVars$dataset
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    
    if(globalVars$transform.type == "none"){
      response <- names(model.frame(model))[1]
    }else if(globalVars$transform.type == "log"){
      response <- paste("log(", substring(names(model.frame(model))[1], first=17), ")", sep="")  
    }else if(globalVars$transform.type == "lp1"){
      response <- paste("log(", substring(names(model.frame(model))[1], first=17), " + 1)", sep="")
    }else if(globalVars$transform.type == "ihs"){
      response <- paste("inverse hyperbolic sine transformed ", substring(names(model.frame(model))[1], first=17), sep="")
    }
    
    
    emmeans.text<-""
    if(all(int.vars.classes=="numeric")){ #Both Continuous
      for(i in 1:nrow(mod.emmeans)){
        emmeans.text <- paste(emmeans.text,"\U2022 The estimated marginal mean of ", sub("\\.scaled$", "", names(model.frame(model))[1]), " for ", sub("\\.scaled$", "", int.var), " = ", mod.emmeans[i,int.var] ," and ", sub("\\.scaled$", "", colnames(mod.emmeans[moderator])), " = " , as.character(mod.emmeans[i,moderator]), " is ", round(mod.emmeans[i,"Estimated Marginal Mean"],2),
                              " (95% CI: ", round(mod.emmeans[i,"Lower CI"],2), ", ", round(mod.emmeans[i,"Upper CI"],2), "). <br/>",
                              sep="")
      }
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      for(i in 1:nrow(mod.emmeans)){
        emmeans.text <- paste(emmeans.text,"\U2022 The estimated marginal mean of ", sub("\\.scaled$", "", response), " for ", sub("\\.scaled$", "", int.var), " = ", as.character(mod.emmeans[i,int.var]) ," and ", sub("\\.scaled$", "", colnames(mod.emmeans[moderator])), " = " , as.character(mod.emmeans[i,moderator]), " is ", round(mod.emmeans[i,"Estimated Marginal Mean"],2),
                              " (95% CI: ", round(mod.emmeans[i,"Lower CI"],2), ", ", round(mod.emmeans[i,"Upper CI"],2), "). <br/>",
                              sep="")
      }
    }else{
      if(int.vars.classes[moderator]=="factor"){
        for(i in 1:nrow(mod.emmeans)){
          emmeans.text <- paste(emmeans.text, "\U2022 The estimated marginal mean of ", sub("\\.scaled$", "", response), " for ", sub("\\.scaled$", "", int.var), " = ", mod.emmeans[i,int.var] ," and ", sub("\\.scaled$", "", colnames(mod.emmeans[moderator])), " = " , as.character(mod.emmeans[i,moderator]), " is ", round(mod.emmeans[i,"Estimated Marginal Mean"],2),
                                " (95% CI: ", round(mod.emmeans[i,"Lower CI"],2), ", ", round(mod.emmeans[i,"Upper CI"],2), "). <br/>",
                                sep="")
        }
      }else{
        for(i in 1:nrow(mod.emmeans)){
          emmeans.text <- paste(emmeans.text,"\U2022 The estimated marginal mean of ", sub("\\.scaled$", "", response), " for ", sub("\\.scaled$", "", int.var), " = ", as.character(mod.emmeans[i,int.var]) ," and ", sub("\\.scaled$", "", colnames(mod.emmeans[moderator])), " = " , as.character(mod.emmeans[i,moderator]), " is ", round(mod.emmeans[i,"Estimated Marginal Mean"],2),
                                " (95% CI: ", round(mod.emmeans[i,"Lower CI"],2), ", ", round(mod.emmeans[i,"Upper CI"],2), "). <br/>",
                                sep="")
        }
      }
    }
    emmeans.text<-paste(emmeans.text, "<br/><strong>Note:</strong> This approach contrasts the estimated marginal means with a Tukey adjustment for multiple comparisons. These values are calculated at the 'average' of the other variables in the model.",
                        "You may want to set the other variables to specific values, which is supported in R but not currently supported in this application.")
    HTML(emmeans.text)
  }
  
  #############################################################################################
  # contrast table
  #############################################################################################
  prepare_interaction_emmeanscontrasts <- metaReactive2({
    dat <- globalVars$dataset
    model <- globalVars$model
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    
    if(all(int.vars.classes=="numeric")){ #Both Continuous
      metaExpr({
        m.mod<-mean(unlist(model.frame(model)[..(moderator)]), na.rm=T)
        s.mod<-sd(unlist(model.frame(model)[..(moderator)]), na.rm=T)
        m.var<-mean(unlist(model.frame(model)[..(int.var)]), na.rm=T)
        s.var<-sd(unlist(model.frame(model)[..(int.var)]), na.rm=T)
        modvarat<-list(c(round(m.mod-s.mod,2), round(m.mod+s.mod,2)),
                       c(round(m.var-s.var,2), round(m.var+s.var,2)))
        names(modvarat)<-c((..(moderator)), ..(int.var))
        
        mod.emmeans<-emmeans(object = model, spec=c(..(int.var),..(moderator)), var=..(int.var), at=modvarat, type="response")
        mod.emmeans <- add_grouping(mod.emmeans, "int.var", ..(int.var) , c(paste("(Low ", ..(int.var),")", sep=""), paste("(High ", ..(int.var),")", sep="")))
        mod.emmeans <- add_grouping(mod.emmeans, "moderator", ..(moderator) , c(paste("(Low ", ..(moderator),")", sep=""), paste("(High ", ..(moderator),")", sep="")))
        
        mod.emmeanscontrast <-data.frame(pairs(emmeans(mod.emmeans, c("int.var","moderator"), type="response")))
        mod.emmeanscontrastci<-data.frame(confint(pairs(mod.emmeans)))
        mod.emmeanscontrast$asymp.LCL<-mod.emmeanscontrastci$asymp.LCL
        mod.emmeanscontrast$asymp.UCL<-mod.emmeanscontrastci$asymp.UCL
        
        mod.emmeanscontrast$contrast <- gsub("\\.scaled", "", mod.emmeanscontrast$contrast)
        
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        mod.emmeanscontrast <- mod.emmeanscontrast %>%
          dplyr::select(-any_of("null"))%>%
          set_rownames(NULL) %>%
          set_colnames(c("Contrast", "Estimate", "SE", "df", "z ratio", "p-value", "Lower CI", "Upper CI"))
      }) 
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      metaExpr({
        "####################################"
        "# Calculate Marginal Effects"
        "####################################"
        mod.emmeans<-emmeans(model, specs = c(..(int.var),..(moderator)), var=moderator, type="response")
        mod.emmeanscontrast <-data.frame(pairs(mod.emmeans))
        mod.emmeanscontrastci<-data.frame(confint(pairs(mod.emmeans)))
        mod.emmeanscontrast$asymp.LCL<-mod.emmeanscontrastci$asymp.LCL
        mod.emmeanscontrast$asymp.UCL<-mod.emmeanscontrastci$asymp.UCL
        
        mod.emmeanscontrast$contrast <- gsub("\\.scaled", "", mod.emmeanscontrast$contrast)
        
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        mod.emmeanscontrast <- mod.emmeanscontrast %>%
          dplyr::select(-any_of("null"))%>%
          set_rownames(NULL) %>%
          set_colnames(c("Contrast", "Estimate", "SE", "df", "z ratio", "p-value", "Lower CI", "Upper CI"))
      })    
    }else{ 
      if(int.vars.classes[moderator]=="factor"){
        metaExpr({
          "####################################"
          "# Calculate Marginal Effects"
          "####################################"
          m<-mean(unlist(model.frame(model)[..(int.var)]), na.rm=T)
          s<-sd(unlist(model.frame(model)[..(int.var)]), na.rm=T)
          modvarat<-list(c(round(m-s,2), round(m+s,2)))
          names(modvarat)<-c(..(int.var))
          
          mod.emmeans <- emmeans(model, specs = c(..(moderator),..(int.var)), var=..(moderator), at=modvarat, type="response")
          mod.emmeans <- add_grouping(mod.emmeans, "int.var", ..(int.var) ,  c(paste("(Low ",..(int.var),")", sep=""), paste("(High ",..(int.var),")", sep="")))
          mod.emmeanscontrast <-data.frame(pairs(mod.emmeans))
          mod.emmeanscontrastci<-data.frame(confint(pairs(mod.emmeans)))
          mod.emmeanscontrast$asymp.LCL<-mod.emmeanscontrastci$asymp.LCL
          mod.emmeanscontrast$asymp.UCL<-mod.emmeanscontrastci$asymp.UCL
          
          mod.emmeanscontrast$contrast <- gsub("\\.scaled", "", mod.emmeanscontrast$contrast)
          
          "####################################"
          "# Clean Up Labels for Printing"
          "####################################"
          mod.emmeanscontrast <- mod.emmeanscontrast %>%
            dplyr::select(-any_of("null"))%>%
            set_rownames(NULL) %>%
            set_colnames(c("Contrast", "Estimate", "SE", "df", "z ratio", "p-value", "Lower CI", "Upper CI"))
        })
      }else{
        metaExpr({
          "####################################"
          "# Calculate Marginal Effects" # NOTE
          "####################################"
          m<-mean(unlist(model.frame(model)[..(moderator)]), na.rm=T)
          s<-sd(unlist(model.frame(model)[..(moderator)]), na.rm=T)
          
          modvarat<-list(c(round(m-s,2), round(m+s,2)))
          names(modvarat)<-c(..(moderator))
          
          mod.emmeans <- emmeans(model, specs = c(..(moderator), ..(int.var)), var=..(moderator), at=modvarat, type="response")
          mod.emmeans <- add_grouping(mod.emmeans, "moderator", ..(moderator) ,  c(paste("(Low ",..(moderator),")", sep=""), paste("(High ",..(moderator),")")))
          mod.emmeanscontrast <-data.frame(pairs(mod.emmeans))
          
          mod.emmeanscontrastci<-data.frame(confint(pairs(mod.emmeans)))
          mod.emmeanscontrast$asymp.LCL<-mod.emmeanscontrastci$asymp.LCL
          mod.emmeanscontrast$asymp.UCL<-mod.emmeanscontrastci$asymp.UCL
          
          mod.emmeanscontrast$contrast <- gsub("\\.scaled", "", mod.emmeanscontrast$contrast)
          
          "####################################"
          "# Clean Up Labels for Printing"
          "####################################"
          mod.emmeanscontrast <- mod.emmeanscontrast %>%
            dplyr::select(-any_of("null"))%>%
            set_rownames(NULL) %>%
            set_colnames(c("Contrast", "Estimate", "SE", "df", "z ratio", "p-value", "Lower CI", "Upper CI"))
        })
      }
    }
  }, inline=TRUE)
  
  # Code to create contrast table
  make_interaction_contrasts_tab<- metaReactive2({
    
    mod.emmeanscontrast <- globalVars$mod.emmeanscontrast
    metaExpr({
      "####################################"
      "# Print Table"
      "####################################"
      mod.emmeanscontrast %>%
        mutate_if(is.numeric, round, 4) %>%
        mutate(`p-value` = ifelse(`p-value` < 0.0001, "<0.0001", `p-value`))
    })
  }, inline=TRUE)
  
  # Render Table to UI
  output$interaction_emmeanscontrast_tab <- DT::renderDataTable({
    globalVars$make_interaction_contrasts_tab
  },
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = 
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend="csv", filename="model-interaction-contrasts"),
          list(extend="excel", filename="model-interaction-contrasts"),
          list(extend="pdf", filename="model-interaction-contrasts")
        ),
        text = "Download",
        filename = "model-interaction-contrasts"
      ))
  ), rownames = FALSE)
  
  # Download button for summary (LaTeX version)----
  output$download_interaction_emmeanscontrastLatex <- downloadHandler(
    filename = function() {
      "model-interaction-contrasts.tex"
    },
    content = function(file) {
      base::print(xtable(globalVars$make_interaction_contrasts_tab, digits = 4), file, type = "latex")
    }
  )
  
  # R Code
  observeEvent(input$code_interaction_emmeans_contrast, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse)
        library(magrittr)
        library(emmeans)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_scaled_data(),
      fitmodel(),
      prepare_interaction_emmeanscontrasts(),
      make_interaction_contrasts_tab()
    )
    
    displayCodeModal(
      code,
      title = "Interaction Estimated Marginal Means Contrasts Table",
      size = "l",
      fontSize = 16,
      clip=NULL
    )
  })
  
  # Interpretation
  output$interaction_emmeanscontrast_interp <- renderPrint({
    globalVars$prepare_interaction_emmeanscontrast_interp
  })
  
  prepare_interaction_emmeanscontrast_interp <- function(){
    mod.emmeanscontrast <- globalVars$mod.emmeanscontrast
    model <- globalVars$model
    dat <- globalVars$dataset
    alpha<-input$alpha
    
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    
    if(globalVars$transform.type == "none"){
      response <- names(model.frame(model))[1]
    }else if(globalVars$transform.type == "log"){
      response <- paste("log(", substring(names(model.frame(model))[1], first=17), ")", sep="")  
    }else if(globalVars$transform.type == "lp1"){
      response <- paste("log(", substring(names(model.frame(model))[1], first=17), " + 1)", sep="")
    }else if(globalVars$transform.type == "ihs"){
      response <- paste("inverse hyperbolic sine transformed ", substring(names(model.frame(model))[1], first=17), sep="")
    }
    
    if(all(int.vars.classes=="numeric")){ #Both Continuous
      text <- ""
      for(i in 1:nrow(mod.emmeanscontrast)){
        adding.text <- paste("\U2022 The contrast of estimated marginal means of ", sub("\\.scaled$", "", response), " for", " (", gsub("\\.scaled", "", mod.emmeanscontrast$Contrast[i]),") is",
                             ifelse(mod.emmeanscontrast[i,"p-value"]<alpha, " statistically discernible ", " not significant "), "(t = ", round(mod.emmeanscontrast[i,"z ratio"],2),
                             ", df = ", round(mod.emmeanscontrast[i,"df"]), ", p-value ", ifelse(mod.emmeanscontrast[i,"p-value"]<0.0001,"< 0.0001", paste("= ",round(mod.emmeanscontrast[i,"p-value"],4), sep="")),
                             "; 95% CI: ", round(mod.emmeanscontrast[i,"Lower CI"],2), ", ", round(mod.emmeanscontrast[i,"Upper CI"],2), ").",
                             sep="")
        text <- paste(text, adding.text, sep="<br>")
      }
      HTML(text)
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      text <- ""
      for(i in 1:nrow(mod.emmeanscontrast)){
        adding.text <- paste("\U2022 The contrast of estimated marginal means of ", sub("\\.scaled$", "", response), " for", " (", gsub("\\.scaled", "", mod.emmeanscontrast$Contrast[i]),") is",
                             ifelse(mod.emmeanscontrast[i,"p-value"]<alpha, " statistically discernible ", " not significant "), "(t = ", round(mod.emmeanscontrast[i,"z ratio"],2),
                             ", df = ", round(mod.emmeanscontrast[i,"df"]), ", p-value ", ifelse(mod.emmeanscontrast[i,"p-value"]<0.0001,"< 0.0001", paste("= ",round(mod.emmeanscontrast[i,"p-value"],4), sep="")),
                             "; 95% CI: ", round(mod.emmeanscontrast[i,"Lower CI"],2), ", ", round(mod.emmeanscontrast[i,"Upper CI"],2), ").",
                             sep="")
        text <- paste(text, adding.text, sep="<br>")
      }
      HTML(text)
    }else{
      if(int.vars.classes[moderator]=="factor"){
        text <- ""
        for(i in 1:nrow(mod.emmeanscontrast)){
          adding.text <- paste("\U2022 The contrast of estimated marginal means of ", sub("\\.scaled$", "", response), " for", " (", gsub("\\.scaled", "", as.character(mod.emmeanscontrast$Contrast[i])),") when (", int.var, " = ", mod.emmeanscontrast[i,int.var], ") is",
                               ifelse(mod.emmeanscontrast[i,"p-value"]<alpha, " statistically discernible ", " not significant "), "(t = ", round(mod.emmeanscontrast[i,"z ratio"],2),
                               ", df = ", round(mod.emmeanscontrast[i,"df"]), ", p-value ", ifelse(mod.emmeanscontrast[i,"p-value"]<0.0001,"< 0.0001", paste("= ",round(mod.emmeanscontrast[i,"p-value"],4), sep="")),
                               "; 95% CI: ", round(mod.emmeanscontrast[i,"Lower CI"],2), ", ", round(mod.emmeanscontrast[i,"Upper CI"],2), ").",
                               sep="")
          text <- paste(text, adding.text, sep="<br>")
        }
        HTML(text)
      }else{
        text <- ""
        for(i in 1:nrow(mod.emmeanscontrast)){
          adding.text <- paste("\U2022 The contrast of estimated marginal means of ", sub("\\.scaled$", "", response), " for", " (", gsub("\\.scaled", "", mod.emmeanscontrast$Contrast[i]),") is",
                               ifelse(mod.emmeanscontrast[i,"p-value"]<alpha, " statistically discernible ", " not significant "), "(t = ", round(mod.emmeanscontrast[i,"z ratio"],2),
                               ", df = ", round(mod.emmeanscontrast[i,"df"]), ", p-value ", ifelse(mod.emmeanscontrast[i,"p-value"]<0.0001,"< 0.0001", paste("= ",round(mod.emmeanscontrast[i,"p-value"],4), sep="")),
                               "; 95% CI: ", round(mod.emmeanscontrast[i,"Lower CI"],2), ", ", round(mod.emmeanscontrast[i,"Upper CI"],2), ").",
                               sep="")
          text <- paste(text, adding.text, sep="<br>")
        }
        HTML(text)
      }
    }
  }
  
  #############################################################################################
  # marginal effects table 
  #############################################################################################
  prepare_interaction_emtrends<- metaReactive2({
    
    model <-globalVars$model
    dat <- globalVars$dataset
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)

    if(all(int.vars.classes=="numeric")){ #Both Continuous
      metaExpr({
        "####################################"
        "# Calculate Marginal Effects"
        "####################################"
        
        
        m<-mean(unlist(model.frame(model)[..(moderator)]), na.rm=T)
        s<-sd(unlist(model.frame(model)[..(moderator)]), na.rm=T)
        modvarat<-list(c(round(m-s,2), round(m+s,2)))
        names(modvarat)<-c(..(moderator))
        
        mod.emtrends<-data.frame(emtrends(object = model, spec=..(moderator), var=..(int.var), at=modvarat))
        mod.emtrends[,..(moderator)]<- ifelse(mod.emtrends[,..(moderator)]==round(m-s,2),"Low (Mean - 1SD)", "High (Mean + 1SD)")
        
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        colnames(mod.emtrends)[-(1)]<-c(paste("Slope of", ..(int.var)), "SE", "df", "Lower CI", "Upper CI")
        emtrend.test <-test(emtrends(object = model, spec=..(moderator), var=..(int.var), at=modvarat))
        mod.emtrends<-mod.emtrends %>% mutate("p-value"= emtrend.test$p.value,
                                              "z ratio"= emtrend.test$z.ratio) %>%
          relocate(c(`z ratio`, `p-value`), .after=df)
        
      })
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      # HIDE emtrends_tab, emtrends_int
    }else{
      if(int.vars.classes[moderator]=="factor"){
        metaExpr({
          "####################################"
          "# Calculate Marginal Effects"
          "####################################"
          mod.emtrends<-data.frame(emtrends(model, specs = ..(moderator), var = ..(int.var)))
          
          "####################################"
          "# Clean Up Labels for Printing"
          "####################################"
          colnames(mod.emtrends)[-(1)]<-c(paste("Slope of", ..(int.var)), "SE", "df", "Lower CI", "Upper CI")
          emtrend.test <-test(emtrends(model, specs = ..(moderator), var = ..(int.var)))
          mod.emtrends<-mod.emtrends %>% mutate("p-value"= emtrend.test$p.value,
                                                "z ratio"= emtrend.test$z.ratio)%>%
            relocate(c(`z ratio`, `p-value`), .after=df)
        })
      }else{
        # HIDE emtrends_tab, emtrends_int
      }
    }
  }, inline=TRUE)
  
  make_interaction_emtrends_tab <- metaReactive2({
    
    mod.emtrends<-globalVars$mod.emtrends
    
    metaExpr({
      "####################################"
      "# Print Table"
      "####################################"
      mod.emtrends %>%
        mutate_if(is.numeric, round, 4) %>%
        mutate(`p-value` = ifelse(`p-value` < 0.0001, "<0.0001", `p-value`))
    })
  }, inline=TRUE)
  
  ### Render Model Summary to UI
  output$interaction_emtrends_tab <- DT::renderDataTable({
    globalVars$make_interaction_emtrends_tab
  },
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = 
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend="csv", filename="model-interaction-marginaleffects"),
          list(extend="excel", filename="model-interaction-marginaleffects"),
          list(extend="pdf", filename="model-interaction-marginaleffects")
        ),
        text = "Download",
        filename = "model-interaction-marginaleffects"
      ))
  ), rownames = FALSE)
  
  # Download button for summary (LaTeX version)----
  output$download_interaction_emtrendsLatex <- downloadHandler(
    filename = function() {
      paste("model-interaction-marginaleffects.tex", sep="")
    },
    content = function(file) {
      base::print(xtable(globalVars$make_interaction_emtrends_tab, digits = 4), file, type = "latex")
    }
  )
  
  # R Code
  # Display code for ci visualization plot ----
  observeEvent(input$code_interaction_emtrendstab, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse) 
        library(magrittr)
        library(emmeans)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_scaled_data(),
      fitmodel(),
      prepare_interaction_emtrends(),
      make_interaction_emtrends_tab()
    )
    
    displayCodeModal(
      code, 
      title = "Interaction Marginal Effects Table",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  
  output$interaction_emtrends_interp <- renderUI({  
    globalVars$prepare_interaction_emtrends_interp
  })
  
  prepare_interaction_emtrends_interp <- function(){
    mod.emtrends<-globalVars$mod.emtrends
    model<-globalVars$model
    dat = globalVars$dataset
    alpha <- input$alpha
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    
    emtrends.interp<-""
    for(i in 1:nrow(mod.emtrends)){
      if(class(mod.emtrends[,1])=="factor" | class(mod.emtrends[,1])=="character"){
        emtrends.interp <- paste(emtrends.interp,"\U2022 The effect of ", gsub("\\.scaled", "", (int.var)),  " is ", ifelse(mod.emtrends[i,"p-value"]<alpha, "statistically discernible ", "not significant "),
                                 "when ", gsub("\\.scaled", "", (moderator)), " is ", mod.emtrends[i,1],
                                 " (Slope = ", round(mod.emtrends[i,2],4),
                                 ", z ratio = ", round(mod.emtrends[i,"z ratio"],4),
                                 ", p-value ",  ifelse(mod.emtrends[i,"p-value"]<0.0001,"< 0.0001", paste("= ", round(mod.emtrends[i,"p-value"],4), sep="")), ").<br/>",
                                 sep="")
      }else{
        emtrends.interp <- paste(emtrends.interp,"\U2022 The effect of ", gsub("\\.scaled", "", (int.var)),  " is ", ifelse(mod.emtrends[i,"p-value"]<alpha, "statistically discernible ", "not significant "),
                                 "when ", gsub("\\.scaled", "", (moderator)), " is ", round(mod.emtrends[i,1],4),
                                 " (Slope = ", round(mod.emtrends[i,2],4),
                                 ", z ratio = ", round(mod.emtrends[i,"z ratio"],4),
                                 ", p-value ",  ifelse(mod.emtrends[i,"p-value"]<0.0001,"< 0.0001", paste("= ", round(mod.emtrends[i,"p-value"],4), sep="")), ").<br/>",
                                 sep="")
      }
    }
    
    if(globalVars$transform.type == "none"){
      emtrends.interp<-paste(emtrends.interp, "<br/><strong>Note:</strong> These marginal effects are calculated at the 'average' of the other variables in the model.",
                             "You may want to set the other variables to specific values, which is supported in R but not currently supported in this application.")      
    }else if(globalVars$transform.type == "log"){
      emtrends.interp<-paste(emtrends.interp, "<br/><strong>Note:</strong> These effects are on the original log-transformed response. Further, note that these marginal effects are calculated at the 'average' of the other variables in the model.",
                             "You may want to set the other variables to specific values, which is supported in R but not currently supported in this application.")
    }else if(globalVars$transform.type == "lp1"){
      emtrends.interp<-paste(emtrends.interp, "<br/><strong>Note:</strong> These effects are on the original log-plus-1-transformed response. Further, note that these marginal effects are calculated at the 'average' of the other variables in the model.",
                             "You may want to set the other variables to specific values, which is supported in R but not currently supported in this application.")
    }else if(globalVars$transform.type == "ihs"){
      emtrends.interp<-paste(emtrends.interp, "<br/><strong>Note:</strong> These effects are on the original inverse hyperbolic sine transformed response. Further, note that these marginal effects are calculated at the 'average' of the other variables in the model.",
                             "You may want to set the other variables to specific values, which is supported in R but not currently supported in this application.")
    }
    
    HTML(emtrends.interp)
  }
  
  #############################################################################################
  # marginal effects contrast table 
  #############################################################################################
  prepare_interaction_emtcontrast<- metaReactive2({
    
    model <-globalVars$model
    dat <- globalVars$dataset
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    if(all(int.vars.classes=="numeric")){ #Both Continuous
      metaExpr({
        "####################################"
        "# Calculate Marginal Effects Contrasts"
        "####################################"
        m.mod<-mean(unlist(model.frame(model)[..(moderator)]), na.rm=T)
        s.mod<-sd(unlist(model.frame(model)[..(moderator)]), na.rm=T)
        m.var<-mean(unlist(model.frame(model)[..(int.var)]), na.rm=T)
        s.var<-sd(unlist(model.frame(model)[..(int.var)]), na.rm=T)
        modvarat<-list(c(round(m.mod-s.mod,2), round(m.mod+s.mod,2)),
                       c(round(m.var-s.var,2), round(m.var+s.var,2)))
        names(modvarat)<-c((..(moderator)), ..(int.var))
        mod.emtrends <- emtrends(object = model, spec=..(moderator), var=..(int.var), at=modvarat)
        mod.emtrends <- add_grouping(mod.emtrends, "moderator", ..(moderator) , c(paste("(Low ", ..(moderator),")", sep=""), paste("(High ", ..(moderator),")", sep="")))
        mod.emtrends <- emmeans(mod.emtrends, spec="moderator", var=(int.var), at=modvarat)
        
        mod.emtrendcontrast <-data.frame(pairs(mod.emtrends))
        mod.emtrendcontrastci<-data.frame(confint(pairs(mod.emtrends)))
        mod.emtrendcontrast$asymp.LCL<-mod.emtrendcontrastci$asymp.LCL
        mod.emtrendcontrast$asymp.UCL<-mod.emtrendcontrastci$asymp.UCL
        
        mod.emtrendcontrast$contrast <- gsub("\\.scaled", "", mod.emtrendcontrast$contrast)
        
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        mod.emtrendcontrast <- mod.emtrendcontrast %>%
          set_rownames(NULL) %>%
          set_colnames(c("Contrast", "Estimate", "SE", "df", "z ratio", "p-value", "Lower CI", "Upper CI"))
      })
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      # HIDE emtrends_tab, emtrends_int
    }else{
      if(int.vars.classes[moderator]=="factor"){
        metaExpr({
          "####################################"
          "# Calculate Marginal Effects Contrasts"
          "####################################"
          mod.emtrends <- emtrends(object = model, spec=..(moderator), var=..(int.var))
          mod.emtrendcontrast <-data.frame(pairs(mod.emtrends))
          
          mod.emtrendcontrastci<-data.frame(confint(pairs(mod.emtrends)))
          mod.emtrendcontrast$asymp.LCL<-mod.emtrendcontrastci$asymp.LCL
          mod.emtrendcontrast$asymp.UCL<-mod.emtrendcontrastci$asymp.UCL
          
          mod.emtrendcontrast$contrast <- gsub("\\.scaled", "", mod.emtrendcontrast$contrast)
          
          "####################################"
          "# Clean Up Labels for Printing"
          "####################################"
          mod.emtrendcontrast <- mod.emtrendcontrast %>%
            set_rownames(NULL) %>%
            set_colnames(c("Contrast", "Estimate", "SE", "df", "z ratio", "p-value", "Lower CI", "Upper CI"))
          
        })
      }else{
        # HIDE emtrends_tab, emtrends_int
      }
    }
  }, inline=TRUE)
  
  make_interaction_emtrendscontrast_table <- metaReactive2({
    
    mod.emtrendcontrast<-globalVars$mod.emtrendcontrast
    
    metaExpr({
      "####################################"
      "# Print Table"
      "####################################"
      mod.emtrendcontrast %>%
        mutate_if(is.numeric, round, 4) %>%
        mutate(`p-value` = ifelse(`p-value` < 0.0001, "<0.0001", `p-value`))
    })
  }, inline=TRUE)
  
  ### Render Model Summary to UI
  output$interaction_emtrendscontrast_tab <- DT::renderDataTable({
    globalVars$make_interaction_emtrendscontrast_table
  },
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = 
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend="csv", filename="model-interaction-marginaleffectscontrast"),
          list(extend="excel", filename="model-interaction-marginaleffectscontrast"),
          list(extend="pdf", filename="model-interaction-marginaleffectscontrast")
        ),
        text = "Download",
        filename = "model-interaction-marginaleffectscontrast"
      ))
  ), rownames = FALSE)
  
  # Download button for summary (LaTeX version)----
  output$download_interaction_emtrendscontrastLatex <- downloadHandler(
    filename = function() {
      paste("model-interaction-marginaleffectscontrast.tex", sep="")
    },
    content = function(file) {
      base::print(xtable(globalVars$make_interaction_emtrendscontrast_table, digits = 4), file, type = "latex")
    }
  )
  
  # R Code
  # Display code for ci visualization plot ----
  observeEvent(input$code_interaction_emtrendscontrasttab, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse) 
        library(magrittr)
        library(emmeans)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_scaled_data(),
      fitmodel(),
      prepare_interaction_emtcontrast(),
      make_interaction_emtrendscontrast_table()
    )
    
    displayCodeModal(
      code, 
      title = "Interaction Marginal Effects Contrasts Table",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  output$interaction_emtrendscontrast_interp <- renderUI({  
    globalVars$prepare_interaction_emtrendscontrast_interp
  })
  
  prepare_interaction_emtrendscontrast_interp <- function(){
    mod.emtrendcontrast<-globalVars$mod.emtrendcontrast
    model<-globalVars$model
    
    alpha <- input$alpha
    dat = globalVars$dataset
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    
    emtrends.interp<-""
    for(i in 1:nrow(mod.emtrendcontrast)){
      emtrends.interp <- paste(emtrends.interp,"\U2022 The effect of ", gsub("\\.scaled", "", (int.var)),  " is ", ifelse(mod.emtrendcontrast[i,"p-value"]<alpha, "statistically discernibly ", "not significantly "),
                               "different by ", gsub("\\.scaled", "", (moderator)), 
                               " (Contrast = ", mod.emtrendcontrast[i,"Contrast"],
                               ", Estimate = ", round(mod.emtrendcontrast[i,"Estimate"],4),
                               ", z ratio = ", round(mod.emtrendcontrast[i,"z ratio"],4),
                               ", p-value ",  ifelse(mod.emtrendcontrast[i,"p-value"]<0.0001,"< 0.0001", paste("= ", round(mod.emtrendcontrast[i,"p-value"],4), sep="")), ").<br/>",
                               sep="")
    }
    
    HTML(emtrends.interp)
  }
  
  #############################################################################################
  # Johnson Neyman Plot
  #############################################################################################
  # Code for Johnson Neyman Plot
  jnplot <- metaReactive2({
    req(globalVars$model)
    
    dat <- globalVars$dataset
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    if(all(int.vars.classes=="numeric")){ #Both Continuous
      model<-globalVars$model
      
      metaExpr({
        "####################################"
        "# Johnson Neyman"
        "####################################"
        jn<-interactions::johnson_neyman(model = model, pred =  !!sym(..(int.var)), 
                                         modx = !!sym(..(moderator)),
                                         control.fdr = TRUE, plot = FALSE)
        
        "####################################"
        "# Plot"
        "####################################"
        p<-jn$plot + 
          xlab(..(moderator))+
          ylab(paste("Slope of ", ..(int.var),sep=""))+
          ggtitle("Johnson-Neyman Plot")+
          scale_color_brewer(paste("Slope of ", ..(int.var),sep=""), palette = "Pastel1")+
          scale_fill_brewer(paste("Slope of ", ..(int.var),sep=""), palette = "Pastel1")+
          theme_bw()
        # If greyscale, this bit is needed
        # p$layers[[9]]  <- geom_vline(xintercept = jnplot$bounds[1], color="black", linetype="dashed")
        # p$layers[[10]] <- geom_vline(xintercept = jnplot$bounds[2], color="black", linetype="dashed")
        p
      })
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      #not appropriate
      cat("Johnson Neyman plots do not support categorical predictors.")
    }else{ #One of Each
      #not appropriate
      cat("Johnson Neyman plots do not support categorical predictors.")
    }
  },inline=TRUE)
  
  # Render plot to UI
  output$jn_plot <- renderPlot({
    globalVars$jnplot
  })
  
  # R Code
  observeEvent(input$code_jn, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse) 
        library(interactions)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_scaled_data(),
      fitmodel(),
      jnplot()
    )
    
    displayCodeModal(
      code, 
      title = "Johnson Neyman Plot",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  # Interpret Johnson Neyman Plot
  output$jn_int <- renderPrint({
    globalVars$prepare_jn_int
  })
  
  prepare_jn_int <- function(){
    
    dat <- globalVars$dataset
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    
    model<-globalVars$model
    
    if(all(int.vars.classes=="numeric")){ #Both Continuous
      jn<-interactions::johnson_neyman(model = model, pred = !!sym(moderator), modx = !!sym(int.var),
                                       control.fdr = TRUE, plot = FALSE)
      jn<-paste(capture.output(jn), collapse=" ")
      jn<-crayon::strip_style(jn)
      jn<-str_remove(string = jn, pattern = "JOHNSON-NEYMAN INTERVAL   ")
      jn<-str_replace(string = jn, pattern = "slope", replacement = "effect")
      jn<-str_replace(string = jn, pattern = "Interval calculated using", replacement = "and the interval was calculated using")
      jn<-strsplit(jn, "adjusted")[[1]][1]
      jn<-paste(jn, "adjustment.", sep="")
      jn<-str_replace(string = jn, pattern = "Note:", replacement = "<br><strong>Note:</strong>")
      jn <- paste("\U2022 ", jn, sep="")
      jn <- gsub("\\.scaled", "", jn)
      
      HTML(jn)
      
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      #not appropriate
      HTML("Johnson Neyman plots do not support categorical predictors.")
    }else{ #One of Each
      #not appropriate
      HTML("Johnson Neyman plots do not support categorical predictors.")
    }
  }
  
  # Download button for plots (call reactive function here to get plot object) ----
  output$downloadjnPlot <- downloadHandler(
    filename = function() { paste('johnsonneymanplot.', input$jn_plot_format, sep='') },
    content = function(file) {
      ggsave(file, plot = globalVars$jnplot, device = input$jn_plot_format, 
             height = as.numeric(input$jn_plot_height), width = as.numeric(input$jn_plot_width), 
             units = input$jn_plot_units)
    }
  )
  
  #############################################################################################
  # When a new sample selected
  #############################################################################################
  observeEvent(input$sample_data_choice,{
    globalVars$changed.input <- TRUE
    if(globalVars$sample){
      if(input$sample_data_choice=="Palmer Penguins"){
        library(palmerpenguins)
        dat<-data.frame(penguins)
      }else if(input$sample_data_choice=="Kitsberg et al. Nucleus" ){
        dat<-read.csv("www/Kitsberg25-nucleus.csv")
      }else if(input$sample_data_choice=="U.S. News College Data"){
        library(ISLR)
        dat<-College
      }else if(input$sample_data_choice=="Camera Data" ){
        dat<-read.csv("www/cs_replication_data.csv")
      }
      else if(input$sample_data_choice=="Ache Monkey" ){
        dat<-read_csv("www/McMillanAcheMonkey.csv")
      }else if(input$sample_data_choice=="Tweedie Simulated Data"){
        dat<-read.csv("www/tweedie_regression_data.csv")
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
  
  #############################################################################################
  # When a new model selected
  #############################################################################################  
  observe({
    session$onFlushed(function() {
      shinyjs::runjs("
      $('#equation').tooltip({
        trigger: 'hover',
        placement: 'right'
      });
    ")
    }, once = TRUE)
  })
 
    
    observeEvent(input$model_choice,{ #Okay this is set up to get the user choice and save it in server, now once they select one it should clear
      req(input$model_choice)
      
      globalVars$changed.model.input <- TRUE
      globalVars$model_choice <- input$model_choice
      print(paste("Selected Model:", globalVars$model_choice))
      
      updateFactorsSelectize()
      hideInteractionInput()
      emptyEquation()
      uncheckAllAssumptions()
      hideAllTabs()
      

      
      if(input$model_choice %in% c("Poisson", "Negative Binomial")){
        shinyjs::show("asmp_5A")
        shinyjs::hide("asmp_5B")
      } else if(input$model_choice %in% c("Zero-Inflated Poisson", "Zero Inflated Negative Binomial")) {
        shinyjs::hide("asmp_5A")
        shinyjs::show("asmp_5B")
      } else {
        shinyjs::hide("asmp_5A")
        shinyjs::hide("asmp_5B")
      }
      
      
      
    
    # Define the message based on the model selection
    tip_message <- case_when(
      
      # Standard Poisson
      input$model_choice == "Poisson" ~ 
        "Example: response ~ x_1 + x_2 + ... + x_k",
      
      
      # Models for Over dispersion
      input$model_choice %in% c("Negative Binomial", "Quasi-Poisson", "Tweedie") ~ 
        "Format: response ~ x_1 + x_2 + ... + x_k (Adjusted for overdispersion)",
      
      # Zero-Inflated Models (typically using the 'pscl' or 'glmmTMB' package syntax)
      input$model_choice %in% c("Zero-Inflated Poisson", "Zero Inflated Negative Binomial") ~ 
        "Format: response ~ count_vars | zero_vars (e.g., y ~ x1 | z1)",      

      TRUE ~ "Format: response ~ explanatory_1 + explanatory_2 + ... + explanatory_k"
    )

    # Remove the old tooltip and add the new one ask prof this is SHAWWWW
    
    shinyjs::runjs(sprintf("
  $('#equation').attr('data-original-title', '%s').tooltip('update');
", tip_message))
    
  })
  
  
  ########################################
  # Check Equation Input
  ########################################
  
    checkEquationValidity <- function() {
      run <- TRUE
      dat <- globalVars$dataset
      
      # 1. Syntax Check
      if (!grepl("~", input$equation)) {
        shinyalert("Error!", text = "Your regression equation must contain a '~'.", type = "error")
        return(FALSE) # Exit immediately
      }
      
      # 2. Variable/Logic Check
      # We wrap the whole thing in tryCatch and assign the result to 'run'
      run <- tryCatch({
        f <- as.formula(input$equation)
        variables <- all.vars(f)
        
        if (length(variables) >= 2 && variables[2] != "") {
          globalVars$response <- variables[1] 
          globalVars$equation <- input$equation
          
          # Check misspellings
          if (!all(variables %in% colnames(dat))) {
            shinyalert("Error!", text = "Variable does not exist in data set.", type = "error")
            return(FALSE) 
            
            # Check Numeric Response
          } else if (!is.numeric(dat[[globalVars$response]])) {
            shinyalert("Error!", text = "Response variable must be numeric.", type = "error")
            return(FALSE)
            
          } else {
            # Check Categorical Levels
            for (i in 2:length(variables)) {
              var_name <- variables[i]
              column_data <- dat[[var_name]]
              if (!is.numeric(column_data)) {
                if (length(unique(as.character(column_data))) > 12) {
                  shinyalert("Error!", text = paste0("Variable '", var_name, "' has > 12 levels."), type = "error")
                  return(FALSE)
                }
              }
            }
          }
          return(TRUE) # If we got here, everything is valid
          
        } else {
          shinyalert("Error!", text = "Missing predictors.", type = "error")
          return(FALSE)
        }
        
      }, error = function(e) {
        # If as.formula() or all.vars() explodes, this triggers
        shinyalert("Error!", text = "Invalid formula syntax.", type = "error")
        return(FALSE) 
      })
      
      return(run) # This now returns the result of the tryCatch
    }
  
  
  #############################################################################################
  # When a new sample selected
  #############################################################################################
  observeEvent(input$DoCompute,{
    #Handles Edge case in which user stats on the Poisson Model. A lot of other stuff was built on Poisson being default supposedly. In any other case, this does nothing and it works as normal.
    if(globalVars$model_choice == "Poisson"){
      shinyjs::show("asmp_5A")
      shinyjs::hide("asmp_5B")
    }
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
        # Try to fit model
        model <- tryCatch({
          withCallingHandlers({
            
        #  browser()
          choice <- globalVars$model_choice
          form <- globalVars$equation
          dat <- globalVars$dataset
          
          
          model <- switch(choice,
                        "Poisson" = glm(form, data = dat, family = poisson(link = "log")),
                        "Quasi-Poisson" = glm(form, data = dat, family = quasipoisson(link = "log")),
                        "Negative Binomial" = MASS::glm.nb(form, data = dat),
                        "Tweedie" = glm(form, data = dat, family = statmod::tweedie(1.5, 0)),
                        NULL
          )
          
          if (grepl("\\|", form)) {
            parts <- strsplit(form, "\\|")[[1]]
            count_formula <- as.formula(parts[1])
            zi_formula    <- as.formula(paste("~", trimws(parts[2])))
            
          } else {
            count_formula <- as.formula(form)
            zi_formula    <- ~ 1
          }
          

          
          if (choice == "Zero-Inflated Poisson"){
            # For some reason doesnt work with some interaction maybe due to sensitivity in poisson need to set up a try catch
              model <- glmmTMB(
                formula   = count_formula,
                ziformula = zi_formula,
                data      = dat,
                family    = 'poisson'
              )
          }
          
          else if(choice == "Zero Inflated Negative Binomial" ){
            model <- glmmTMB(
              formula   = count_formula,
              ziformula = zi_formula,
              data      = dat,
              family    = 'nbinom2'
            )
            
          }
            
          if (choice %in% c("Zero-Inflated Poisson", "Zero Inflated Negative Binomial")) {
            # Call for glmmTMB
            model$call <- substitute(
              glmmTMB(formula = F_VAL, ziformula = Z_VAL, data = D_VAL, family = FAM_VAL),
              list(
                F_VAL   = count_formula,
                Z_VAL   = zi_formula,
                D_VAL   = quote(dat), # Make sure this matches your actual data object name
                FAM_VAL = if(choice == "Zero-Inflated Poisson") "poisson" else "nbinom2"
              )
            )
          } else {
            # Call for standard GLM/NB
            model$call <- substitute(
              FUNC(formula = F_VAL, data = D_VAL, family = FAM_VAL),
              list(
                FUNC    = as.name(if(choice == "Negative Binomial") "glm.nb" else "glm"),
                F_VAL   = count_formula,
                D_VAL   = quote(dat),
                FAM_VAL = if(choice == "Poisson") quote(poisson(link="log")) else 
                          if(choice == "Quasi-Poisson") quote(quasipoisson(link="log")) else
                          if(choice == "Tweedie") quote(statmod::tweedie(1.5, 0)) else NULL
              )
            )
          }
          
          if(is.null(model$call$family)) model$call$family <- NULL
          model
          },
          warning = function(w) {
            # 2. Check for the iteration warning specifically
            if (grepl("iteration limit reached", w$message)) {
              shinyalert("Warning", "Iteration limit reached. Results may be unstable.", type = "warning")
              invokeRestart("muffleWarning") # MAGIC: This sends R back to finish the model!
            }
            
            else if(grepl(pattern = "alternation limit reached", x = w, fixed = T)){
              shinyalert("Warning", text="The Negative Binomial model reached its iteration limit without converging. Please try another regression equation.", type = "warning")   
              invokeRestart("muffleWarning") # MAGIC: This sends R back to finish the model!
              
            }
            else if (grepl("non-positive-definite Hessian", x = w, fixed = TRUE)) {
              shinyalert(
                "Warning", 
                text = "The model 'converged' but the results are unstable (Non-positive-definite Hessian). Standard errors and p-values may be missing or incorrect. Try Scaling Data to fix",
                type = "warning"
              )
              invokeRestart("muffleWarning")
            }
          })
          
          
        }, error = function(e){
          if (grepl(pattern = "could not find function", x = e, fixed = T)) {
            shinyalert("Error!", text = "One of your function names is spelled incorrectly!", type = 'error')
          }else if (grepl(pattern = "minimum count is not zero", x = e, fixed = T)) {
            shinyalert("Error!", "Zero-inflated models require data with zeros. Choose Poisson instead.", type = "error")
          }else if(grepl(pattern = "Error in eval(predvars, data, env): object", x = e, fixed = T)){
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
          return(NULL)
          
        }, warning = function(w){
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
            shinyalert("Error!", text="(In Warninig) There was an unanticipated error in fitting your regression model. Please report the issue and/or try another regression equation.", type = "error")         
            }
          
          return(NULL)
          
        })
        

        if (is.null(model)) {
          print("Error Message Appeared or Model is NULL!")
          return(NULL)
        }
        
        model_matrix <- tryCatch({
          if (choice == "Zero-Inflated Poisson" || choice == "Zero Inflated Negative Binomial"){
            model.matrix(model, model = "count")
          } else {
            model.matrix(model)
          }
        }, error = function(e) {
          print(paste("DEBUG: model.matrix failed -", e$message))
          return(NULL)
        })
        
        # 3. Final safety check
        if (is.null(model_matrix)) return(NULL)
      
        
        # Check the rank of the model matrix < number of predictors?
        if(qr(model_matrix)$rank < (ncol(model_matrix))){
          shinyalert("Error!", text="Coefficients in your model are undefined due to signular fit. This can happen when there are missing combinations of observed categorical variables in a model with an interaction.", type = "error")
          return(NULL)
        }
        
        
        if (!(inherits(model, "glm") | inherits(model, "glmmTMB"))) {
          run <- FALSE
        }
        
      }
      
      
      if(run){ # NOTE THIS IS WHERE ALL PLOTS AND THINGS HAPPEN
        showModal(modalDialog("Things are happening in the background!", footer=NULL))
        globalVars$model <- model
        globalVars$modelsummary <- prepare_model_summary() 
        globalVars$prepare_model_interp <- prepare_model_interp()
        
        globalVars$anova <- prepare_anova()
        globalVars$prepare_anova_interp <- prepare_anova_interp()
        
        
        if (choice == "Zero-Inflated Poisson" || choice == "Zero Inflated Negative Binomial" ){
          all_vars <- all.vars(formula(model))
          fct.vars <- all_vars[sapply(dat[all_vars], is.factor)]
        }
        else{
          fct.vars <- names(which(attr(model$terms, "dataClasses")=="factor"))
        }
        ### Show factor outputs, if necessary
        if(length(fct.vars)>=1){
          
          globalVars$anova_fctcomp <- prepare_anova_fctcomp()
          globalVars$make_anovafctcomp_plot <- make_anovafctcomp_plot()
          globalVars$make_anovafctcomp_num <- make_anovafctcomp_num()
          globalVars$prepare_anova_fctcompinterp <- prepare_anova_fctcompinterp()
          shinyjs::show("anova_fctcomp")
        }else{
          shinyjs::hide("anova_fctcomp")
        }
        

        globalVars$make_ggpairs_plot <- make_ggpairs_plot()
        globalVars$make_ggpairs_summary <- make_ggpairs_summary()
        

        globalVars$RQRPlot <- tryCatch({ # make_assumptions_plot
          RQRPlot()
        }, error = function(e) {
          shinyalert("Error!", text = "There was an issue making the RQR Plot.", type = "error")
          NULL
        })
        
        globalVars$Pearson_Residual <- tryCatch({
          Pearson_Residual()


        }, error = function(e) {
          shinyalert("Error!", text = "There was an issue making the Pearson Plot.", type = "error")
          NULL
        })
        
        globalVars$ZeroInflated <- tryCatch({ # HAve to set up for ZIP and Tweezer whatever doesnt use it
         ZeroInflated()

        }, error = function(e) {
          shinyalert("Error!", text = "There was an issue making the ZeroInflated Plot.", type = "error")
          NULL
        })
        
        all_vars <- all.vars(formula(model))
        len <- length(all_vars) - 1
        

        if(len >= 2){
          globalVars$make_vif_num <- make_vif_num()  
        }
        else{
          shinyjs::hide("vifdiv")
        }
        
        
        globalVars$make_check_plot <- make_check_plot() #Checks Outliers crashing here
        globalVars$make_anova_num <- make_anova_num()
        globalVars$make_model_summary <- make_model_summary()

        
        showAllTabs()
        updateTabsetPanel(session, "workPanel", selected = "summary")
        
        
        
        if((grepl("[*]", globalVars$equation))){
          # for any model with an interaction
          shinyjs::show("marginaleffectsdiv")
          globalVars$modelmargins <- prepare_model_margins()
          globalVars$make_margins_summary <- make_margins_summary()
          globalVars$prepare_margins_interp <- prepare_margins_interp()
          
          # Extract Regression Terms
          terms <- attr(terms(model), "term.labels")
          
          
          # What are the two-way interactions?
          possible.ints <- terms[str_count(string=terms, pattern=":")==1]
          # What are the 3+-way interactions?
          bigger.ints <- terms[str_count(string=terms, pattern=":") > 1]
          
          if(length(possible.ints)==0){
            ints.to.select <- c("None")
          } else if(length(bigger.ints)==0){
            ints.to.select <- c(possible.ints)
          }else{
            ints.to.select <- c()
            # For each two-way
            for(int in possible.ints){
              # get the variables in the two way interaction
              curr.vars <- c(str_split(string=int, pattern=":", simplify = T))
              # now, check if it is part of a higher-order interaction
              include=TRUE
              for(bigger.int in bigger.ints){
                # get the variables in the bigger interaction
                curr.big.vars <- c(str_split(string=bigger.int, pattern=":", simplify = T))  
                # are all two-way interaction variables in the higher-order interaction?
                if(all(curr.vars %in% curr.big.vars)){
                  # if not, it stops at a two-way interaction and we can deal with it.
                  include=FALSE
                }
              }
              if(include){
                ints.to.select <- c(ints.to.select, int)
              }
            }  
          }
          if(length(ints.to.select)>0){
            globalVars$interactions = ints.to.select
            showInteractionInput()
            updateInteractionSelect()
          }
          else{
            globalVars$interactions = ints.to.select
            hideInteractionInput()
          }
        }else{# no interaction
          shinyjs::hide("marginaleffectsdiv")
          hideTab(inputId="workPanel", target="interaction")
        }
        
        
        # Now should ad interactions
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

  #############################################################################################
  # Model Summary
  #############################################################################################
  prepare_model_summary <- metaReactive2({ 
    req(globalVars$model)
    mod_type <- globalVars$model_choice
    

    dat <- globalVars$dataset
    model <- globalVars$model
    

    if (mod_type == "Zero-Inflated Poisson" || mod_type == "Zero Inflated Negative Binomial"){
      metaExpr({
        
        zero.mod.sum <- summary(model)$coefficient$zi # has city things
        count.mod.sum <- summary(model)$coefficient$cond 
        
        ci <- as.data.frame(confint(model))
        
        ci <- ci[, -ncol(ci)]
        
        ci.zero <- ci[grep("^zi\\.", rownames(ci)), ]
        rownames(ci.zero) <- gsub("^zi\\.", "", rownames(ci.zero))
        ci.zero_to_keep <- ci.zero[, c("2.5 %", "97.5 %")]
        
        
        ci.count <- ci[grep("^cond\\.", rownames(ci)), ]
        rownames(ci.count) <- gsub("^cond\\.", "", rownames(ci.count))
        ci.count_to_keep <- ci.count[, c("2.5 %", "97.5 %")]
        
        
        mod.table_zero <- cbind(zero.mod.sum, ci.zero)
        mod.table_count <- cbind(count.mod.sum, ci.count_to_keep)
        
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        mod.classes<-sapply(X = model.frame(model), FUN = class)[-1]
        mod.classes<-mod.classes[which(mod.classes=="factor")]
        for(vname in names(mod.classes)){
          ind <- grepl(x=rownames(mod.table_count), pattern = vname)
          ind.int <- grepl(x=rownames(mod.table_count), pattern = ":")
          
          indexes.int<- ind & ind.int
          indexes <- xor(ind, indexes.int)
          
          varval  <- str_remove_all(rownames(mod.table_count)[indexes], vname)
          rownames(mod.table_count)[indexes]<-paste(vname, " = ", varval, sep="")
          
          if(any(indexes.int)){
            for(i in which(indexes.int)){
              intname <- rownames(mod.table_count)[i]
              vname.unique <- str_replace(string = intname, pattern = paste(rownames(mod.table_count)[(!indexes)&(!ind.int)],collapse ="|"), replacement = "")
              varval  <- str_remove_all(vname.unique, pattern = vname)
              varval  <- str_remove_all(varval, pattern = "[:]")
              
              rname<-str_remove_all(string = rownames(mod.table_count)[i], pattern = vname)
              rname<-str_remove_all(string = rname, pattern = varval)
              rname<-str_remove_all(string = rname, pattern = "[:]")
              
              rownames(mod.table_count)[i]<- paste(rname," : (",vname, " = ", varval,")", sep="")
            }
          }
          
        }
        
        
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        mod.classes<-sapply(X = model.frame(model), FUN = class)[-1]
        mod.classes<-mod.classes[which(mod.classes=="factor")]
        for(vname in names(mod.classes)){
          ind <- grepl(x=rownames(mod.table_zero), pattern = vname)
          ind.int <- grepl(x=rownames(mod.table_zero), pattern = ":")
          
          indexes.int<- ind & ind.int
          indexes <- xor(ind, indexes.int)
          
          varval  <- str_remove_all(rownames(mod.table_zero)[indexes], vname)
          rownames(mod.table_zero)[indexes]<-paste(vname, " = ", varval, sep="")
          
          if(any(indexes.int)){
            for(i in which(indexes.int)){
              intname <- rownames(mod.table_zero)[i]
              vname.unique <- str_replace(string = intname, pattern = paste(rownames(mod.table_zero)[(!indexes)&(!ind.int)],collapse ="|"), replacement = "")
              varval  <- str_remove_all(vname.unique, pattern = vname)
              varval  <- str_remove_all(varval, pattern = "[:]")
              
              rname<-str_remove_all(string = rownames(mod.table_zero)[i], pattern = vname)
              rname<-str_remove_all(string = rname, pattern = varval)
              rname<-str_remove_all(string = rname, pattern = "[:]")
              
              rownames(mod.table_zero)[i]<- paste(rname," : (",vname, " = ", varval,")", sep="")
            }
          }
          
        }

        
        mod.table_count <- data.frame(mod.table_count) %>% 
          mutate("Term" = rownames(.), "Model Part" = "Count Model") %>% 
          relocate(Term, `Model Part`) %>%  
          set_rownames(NULL) %>%
          set_colnames(c("Term", "Model Part", "Estimate", "SE", "z", "p-value", "Lower CI", "Upper CI"))
        

        
        
        # 2. FORMAT Zero Table
        mod.table_zero <- data.frame(mod.table_zero) %>% 
          mutate("Term" = rownames(.), "Model Part" = "Zero-Inflation") %>% 
          relocate(Term, `Model Part`) %>%  
          set_rownames(NULL) %>%
          set_colnames(c("Term", "Model Part", "Estimate", "SE", "z", "p-value", "Lower CI", "Upper CI"))
        

        
        mod.table_final <- rbind(mod.table_count, mod.table_zero)
        
        mod.table_final
   
       
      })
      
    }
    else{
      
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
      mod.classes<-sapply(X = model.frame(model), FUN = class)[-1]
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
        set_colnames(c("Term", "Estimate", "SE", "z", "p-value", "Lower CI", "Upper CI"))
      })
    }
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
  
  output$modsumTab2 <- DT::renderDataTable({ # Where we named table Tom
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
  
  
  
  
  #############################################################################################
  # Summary of model and effects
  #############################################################################################
  # Code to interpret model
  output$modelinterp <- renderUI({
    globalVars$prepare_model_interp
  })
  
  
  prepare_model_interp <- function(){
    req(globalVars$model)
    req(globalVars$modelsummary)
    
    dat <- globalVars$dataset
    model <- globalVars$model
    mod.sum <- summary(model)
    mod.table <- globalVars$modelsummary
    choice <- globalVars$model_choice
    
    alpha<-input$alpha
    
    # 1. Calculate the Global P-value (Likelihood Ratio Test)
    
    
    if (choice == "Zero-Inflated Poisson" || choice == "Zero Inflated Negative Binomial" ){
      
      mod_null <- update(model, . ~ 1)
      
      ll_full <- as.numeric(logLik(model))
      ll_null <- as.numeric(logLik(mod_null))
      LR_stat <- 2 * (ll_full - ll_null)
      df_diff <- attr(logLik(model), "df") - attr(logLik(mod_null), "df")
      p_val_global <- pchisq(LR_stat, df_diff, lower.tail = FALSE)
      mcfadden_r2 <- 1 - (ll_full / ll_null)
      
      firstPart <- paste(
        "\U2022 The model ",
        ifelse(p_val_global < alpha, "statistically discernibly predicts ", "does not significantly predict "),
        "the response (Chi-square = ", round(LR_stat, 4),
        ", df = ", df_diff,
        ", p-value ", ifelse(p_val_global < 0.0001, " < 0.0001", paste(" =", round(p_val_global, 4))),
        ").<br/>",
        
        "\U2022 The McFadden's Pseudo R-squared is ", round(mcfadden_r2, 4),
        ". This represents the proportion of total deviance explained by the predictors. For different datasets and within different contexts, the threshold for what is considered 'good'
      is subject to change. In general, if the value is higher than the odds of randomly guessing, then the model can be considered strong.<br/>",
        
        "\U2022 The Log-Likelihood is ", round(ll_full, 4),
        ". This value measures how well the model supports the observed data. The higher this value, the better the model is for the data. This is best used for comparing one model to another.<br/>",
        sep=""
      )
      
    }
    else {
    ll_full <- as.numeric(logLik(model))
    diff_deviance <- model$null.deviance - model$deviance
    df_diff <- model$df.null - model$df.residual
    p_val_global <- pchisq(diff_deviance, df_diff, lower.tail = FALSE)
    mcfadden_r2 <- 1 - (model$deviance / model$null.deviance)
    
    firstPart <- paste(
      "\U2022 The model ",
      ifelse(p_val_global < alpha, "statistically discernibly predicts ", "does not significantly predict "),
      "the response (Chi-square = ", round(diff_deviance, 4),
      ", df = ", df_diff,
      ", p-value ", ifelse(p_val_global < 0.0001, " < 0.0001", paste(" =", round(p_val_global, 4))),
      ").<br/>",
      
      "\U2022 The McFadden's Pseudo R-squared is ", round(mcfadden_r2, 4),
      ". This represents the proportion of total deviance explained by the predictors. For different datasets and within different contexts, the threshold for what is considered 'good'
      is subject to change. In general, if the value is higher than the odds of randomly guessing, then the model can be considered strong. <br/>",
      
      "\U2022 The negative Log-Likelihood is ", round(ll_full, 4),
      ". This value measures how well the model supports the observed data. The higher this value, the better the model is for the data. This is best used for comparing one model to another. <br/>",
      sep=""
    )
    
    }
    
    ind <- which(grepl(pattern = "[:]", x = mod.table$Term))
    secondPart<-""
    
    if (choice == "Zero-Inflated Poisson" || choice == "Zero Inflated Negative Binomial" ){
      
      if(length(ind)==0){ # NO INTERACTION
        numeric.vars <- setdiff(mod.table$Term[!grepl(pattern = " = ", x = mod.table$Term)], "(Intercept)")
        
        
        for(i in 1:nrow(mod.table)){
          curr.row <- mod.table$Term[i]
          if(curr.row=="(Intercept)"){next} #skip intercept
          effect.value <- mod.table$Estimate[i]
          z.value <- round(mod.table$z[i], 4)
          pval <- round(mod.table$`p-value`[i],4)
          pval_num <- round(mod.table$`p-value`[i],4)
          pval <- ifelse(pval<0.0001, " < 0.0001", paste(" =", round(pval, 4)))
          
          sig.text <- ifelse(pval > input$alpha, 
                             paste(". This effect did not reach statistical significance", " (z = ", round(mod.table$z[i], 4), ", p", pval,").", sep=""),
                             paste(" (z=", round(mod.table$z[i], 4), ", p", pval,").", sep=""))
          
          if(curr.row %in% numeric.vars){ # numeric
            if(endsWith(curr.row, ".scaled")){
              unit <- "standard deviation"
              unit2 <- " standard deviation"
              curr.row <- sub("\\.scaled$", "", curr.row) #.scaled $ (only from end)
            } else{
              unit <- "unit"
              unit2 <- ""
            }
            if (mod.table$`Model Part`[i] == "Count Model"){
              adding.text <- paste("\U2022 For every ", unit," increase in ", curr.row, ", we expect a ", round(abs(effect.value),4) , " unit", unit2, ifelse(effect.value<0, " decrease ", " increase "),
                                     "in ", sub("\\.scaled$", "", names(model.frame(model))[1]), ", on average.", sig.text, " This term is considered ", ifelse(pval_num < alpha, "statistically discernible.", "not statistically significant.") , "\n", sep="")
            }
            else{
                adding.text <- paste("\U2022 For every ", unit," increase in ", curr.row, ", we expect a ", round(abs(effect.value),4) , " unit", unit2, ifelse(effect.value<0, " decrease ", " increase "),
                                     "in ", sub("\\.scaled$", "", names(model.frame(model))[1]), ", on average.", sig.text, " This term is considered ", ifelse(pval_num < alpha, "statistically discernible.", "not statistically significant.") , "\n", sep="")
              
            }
            
            secondPart <- paste(secondPart, adding.text, sep="<br/>")
          }
          else{ #factor
            findequal <- str_locate(string=curr.row, pattern = " = ")
            varname <- substr(curr.row, start = 1, stop = findequal[1]-1)
            baselevel<-paste(varname, "=", levels(model.frame(model)[,varname])[1])
            
            adding.text <- paste("\U2022 We expect ", gsub("\\.scaled", "", names(model.frame(model))[1]), " to be ", round(abs(effect.value),4), ifelse(effect.value<0, " lower ", " higher "), "when ",
                                   curr.row, " compared to ", baselevel, ", on average", sig.text, "\n", sep="")
           
            secondPart <- paste(secondPart, adding.text, sep="<br/>")
          }
        }
      }else{
        for(i in ind){
          secondPart <- (paste(secondPart, "\U2022 The interactive effect of ", gsub("\\.scaled", "", mod.table$Term[i]), " is ", ifelse(mod.table$`p-value`[i]<alpha, "statistically discernible ", "not significant "),
                               "(", "\U03B2 = ", round(mod.table$Estimate[i],4),
                               ", z = ",  round(mod.table$z[i],4),
                               ", p-value ",  ifelse(mod.table$`p-value`[i]<0.0001,"< 0.0001", paste("= ", round(mod.table$`p-value`[i],4), sep="")), ").<br/>",
                               sep=""))
        }
      }
    }
    
    else{ # For other models
      if(length(ind)==0){ # NO INTERACTION
        numeric.vars <- setdiff(mod.table$Term[!grepl(pattern = " = ", x = mod.table$Term)], "(Intercept)")
        
        for(i in 1:nrow(mod.table)){
          curr.row <- mod.table$Term[i]
          if(curr.row=="(Intercept)"){next} #skip intercept
          effect.value <- mod.table$Estimate[i]
          z.value <- round(mod.table$z[i], 4)
          pval <- round(mod.table$`p-value`[i],4)
          pval <- ifelse(pval<0.0001, " < 0.0001", paste(" =", round(pval, 4)))
          sig.text <- ifelse(pval > input$alpha, 
                             paste(". This effect did not reach statistical significance", " (z = ", round(mod.table$z[i], 4), " p", pval,").", sep=""),
                             paste(" (z=", round(mod.table$z[i], 4), " p", pval,").", sep=""))
          
          if(curr.row %in% numeric.vars){ # numeric
            if(endsWith(curr.row, ".scaled")){
              unit <- "standard deviation"
              unit2 <- " standard deviation"
              curr.row <- sub("\\.scaled$", "", curr.row) #.scaled $ (only from end)
            } else{
              unit <- "unit"
              unit2 <- ""
            }
            
            adding.text <- paste("\U2022 For every ", unit," increase in ", curr.row, ", we expect a ", round(abs(effect.value),4) , unit2, ifelse(effect.value<0, " decrease ", " increase "),
                                 "in ", sub("\\.scaled$", "", names(model.frame(model))[1]), ", on average.", sig.text, "\n", sep="")
            
            secondPart <- paste(secondPart, adding.text, sep="<br/>")
          }
          else{ #factor
            findequal <- str_locate(string=curr.row, pattern = " = ")
            varname <- substr(curr.row, start = 1, stop = findequal[1]-1)
            baselevel<-paste(varname, "=", levels(model.frame(model)[,varname])[1])
            
            adding.text <- paste("\U2022 We expect ", gsub("\\.scaled", "", names(model.frame(model))[1]), " to be ", round(abs(effect.value),4), ifelse(effect.value<0, " lower ", " higher "), "when ",
                                 curr.row, " compared to ", baselevel, ", on average", sig.text, "\n", sep="")
            
            secondPart <- paste(secondPart, adding.text, sep="<br/>")
          }
        }
      }else{
        for(i in ind){
          secondPart <- (paste(secondPart, "\U2022 The interactive effect of ", gsub("\\.scaled", "", mod.table$Term[i]), " is ", ifelse(mod.table$`p-value`[i]<alpha, "statistically discernible ", "not significant "),
                               "(", "\U03B2 = ", round(mod.table$Estimate[i],4),
                               ", z = ",  round(mod.table$z[i],4),
                               ", p-value ",  ifelse(mod.table$`p-value`[i]<0.0001,"< 0.0001", paste("= ", round(mod.table$`p-value`[i],4), sep="")), ").<br/>",
                               sep=""))
        }
      }
      
    }
    
    
    
    
    HTML(paste(firstPart, secondPart))
    
  }
  
  
  #############################################################################################
  # Coefficient/Marginal Effect Interpretations BOO
  #############################################################################################
  prepare_model_margins <- metaReactive2({
    req(globalVars$model)
    dat <- globalVars$dataset
    model <- globalVars$model
    metaExpr({
      "####################################"
      "# Summarize Model"
      "####################################"
      m_frame <- model.frame(model)
      m_effects <- avg_slopes(model)
      margins.table <- as.data.frame(m_effects)
      
      "####################################"
      "# Clean Up Labels for Printing"
      "####################################"

      marg.classes<-sapply(X = m_frame, FUN = class)[-1]
      if(any(marg.classes=="factor")){
        marg.classes<-marg.classes[which(marg.classes=="factor")]
        for(vname in names(marg.classes)){
          varname <- vname
          indexes <- grepl(x=margins.table$term, pattern = vname)
          levels <- str_split(margins.table$contrast[indexes], " - ")
          clean_val <- sapply(levels, function(x) x[1])
          margins.table$term[indexes] <- paste(vname, "=", clean_val)
        }
      }
      margins.table <- margins.table %>%
        set_rownames(NULL) %>%
        set_colnames(c("Term", "Contrast","Average Marginal Effect", "SE", "z", "p-value", "s-value","Lower CI", "Upper CI"))
      
      margins.table <- margins.table[, !(colnames(margins.table) %in% "Contrast")]
    })
  }, inline=TRUE)
  
  
  make_margins_summary <- metaReactive2({
    
    margins.table <- globalVars$modelmargins
    metaExpr({
      "####################################"
      "# Print Table"
      "####################################"
      margins.table %>% 
        mutate_if(is.numeric, round, 4) %>%
        mutate(`p-value` = ifelse(`p-value` < 0.0001, "<0.0001", `p-value`))
    })
  }, inline=TRUE)
  
  ### Render Model Summary to UI
  output$marginsTab <- DT::renderDataTable({
    globalVars$make_margins_summary
  },
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = 
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend="csv", filename="model-marginaleffects"),
          list(extend="excel", filename="model-marginaleffects"),
          list(extend="pdf", filename="model-marginaleffects")
        ),
        text = "Download",
        filename = "model-marginaleffects"
      ))
  ), rownames = FALSE)
  
  # Download button for summary (LaTeX version)----
  output$downloadmarginsLatex <- downloadHandler(
    filename = function() {
      paste("model-marginaleffects.tex", sep="")
    },
    content = function(file) {
      base::print(xtable(globalVars$make_margins_summary, digits = 4), file, type = "latex")
    }
  )
  
  # R Code
  # Display code for ci visualization plot ----
  observeEvent(input$code_margins, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse) 
        library(magrittr)
        library(margins)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_scaled_data(),
      fitmodel(),
      prepare_model_margins(),
      make_margins_summary()
    )
    
    displayCodeModal(
      code, 
      title = "Regression Marginal Effects Table",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  # Code to interpret marginal effects
  output$marginsinterp <- renderUI({
    globalVars$prepare_margins_interp
  })
  
  prepare_margins_interp <- function(){ # TOM figure this out porfavor
    model <- globalVars$model
    margins.table <- globalVars$modelmargins
    
    mod.classes<-sapply(X = model.frame(model), FUN = class)[-1]
    mod.classes<-mod.classes[which(mod.classes=="numeric")]

    
    first.part <- ""
    for(i in 1:nrow(margins.table)){
      curr.row <- margins.table$Term[i]
      effect.value <- margins.table$`Average Marginal Effect`[i]
      z.value <- round(margins.table$z[i], 4)
      p.value <- ifelse(margins.table$`p-value`[i]<0.0001, "<0.0001", round(margins.table$`p-value`[i], 4))
      sig.text <- ifelse(p.value > input$alpha, " This effect did not reach statistical significance", "")
      
      if(curr.row %in% names(mod.classes)){ # numeric
        if(endsWith(curr.row, ".scaled")){
          unit <- "standard deviation"
          unit2 <- " standard deviation"
          curr.row <- sub("\\.scaled$", "", curr.row) #.scaled $ (only from end)
        } else{
          unit <- "unit"
          unit2 <- ""
        }
        
        adding.text <- paste("\U2022 For every ", unit," increase in ", curr.row, ", we expect a ", round(abs(effect.value),4) , unit2, ifelse(effect.value<0, " decrease ", " increase "),
                               "in ", sub("\\.scaled$", "", names(model.frame(model))[1]), ", on average.", sig.text, " (z=", round(z.value, 4), " p=",  p.value,  ").", "\n", sep="")
  
        first.part <- paste(first.part, adding.text, sep="<br/>")
      }
      else{ #factor
        findequal <- str_locate(string=curr.row, pattern = " = ")
        varname <- substr(curr.row, start = 1, stop = findequal[1]-1)
        baselevel<-paste(varname, "=", levels(model.frame(model)[,varname])[1])
        
      
        adding.text <-paste("\U2022 We expect ", names(model.frame(model))[1], " to be ", round(abs(effect.value),4), ifelse(effect.value<0, " lower ", " higher "), "when ",
                              curr.row, " compared to ", baselevel, ", on average.", sig.text, " (z=", round(z.value, 4), " p=",  p.value,  ").", "\n", sep="")
        
        first.part <- paste(first.part, adding.text, sep="<br/>")
      }
    }
    
    
    if(any(grepl(x = model$call$formula, pattern = "*"))|any(grepl(x = model$call$formula, pattern = ":"))){
      second.part <- paste("<br/><strong>Note:</strong> This model has interactions which are based on marginal effects. Marginal effects are partial 
          derivatives of the regression equation with respect to each variable, calculated as 
          the average change across observations. Calculating marginal effects at representative prespecified values is 
          supported in R, but not currently supported in this application.")
    }
    HTML(paste(first.part, second.part, sep="<br/>"))
  }

  ########################################################
  # Zero Inflated Test NEED SERIOUS FIXING TOMM NOTE only for tweezerAno
  ########################################################
  
  ZeroInflated <-metaReactive2({
    req(globalVars$model)
    dat <- globalVars$dataset
    model <- globalVars$model
    mod_type <- globalVars$model_choice


    
    if(mod_type == "Zero Inflated Negative Binomial" || mod_type == "Zero-Inflated Poisson"){
      

      metaExpr({
      # response
      obs_zeros <- sum(model$frame[1] == 0)
      numZeros <-rep(NA,1000)
      
      for (i in 1:1000){
        numZeros[i] <- sum(simulate(model) == 0)
      }
      
      mean(numZeros <= obs_zeros)
      
      
      sim_data <- data.frame(zeros = numZeros)
      
      ggplot(sim_data, aes(x = zeros)) +
        geom_histogram(aes(y = after_stat(density)), 
                       binwidth = 1, 
                       fill = "#5b5b5b", 
                       color = "blue") +
      
      geom_vline(xintercept = obs_zeros, 
                 color = "red", 
                 linetype = "dotted", 
                 linewidth = 1) +
      
      labs(title = "Assessing Zero Inflation",
           x = "Observed Zeros",
           y = "Density") +
      
      theme_bw()
    
    })
    }
    else{
      metaExpr({
        obs_zeros <- sum(model$y == 0)
        
        numZeros <-rep(NA,1000)
        
        if(mod_type == "Tweedie"){
          
          for (i in 1:1000){
            mu <- predict(model, type = "response")
            p <- 1.5
            phi <- summary(model)$dispersion
            numZeros[i] <- sum(rtweedie(n = length(mu), mu = mu, phi = phi, power = p) == 0)
          }
         
          
        }
        else if( mod_type == "Quasi-Poisson"){
          for (i in 1:1000){
            mu <- predict(model, type = "response")
            numZeros[i] <- sum(rpois(n = length(mu), lambda = mu) == 0)
          }
          
          
        }
        else{
          for (i in 1:1000){
            numZeros[i] <- sum(simulate(model) == 0)
          }
        }
        
        mean(numZeros <= obs_zeros)
        
        
        sim_data <- data.frame(zeros = numZeros)
        
        ggplot(sim_data, aes(x = zeros)) +
          geom_histogram(aes(y = after_stat(density)), 
                         binwidth = 1, 
                         fill = "#5b5b5b", 
                         color = "blue") +
          
          geom_vline(xintercept = obs_zeros, 
                     color = "red", 
                     linetype = "dotted", 
                     linewidth = 1) +
          
          labs(title = "Assessing Zero Inflation",
               x = "Observed Zeros",
               y = "Density") +
          
          theme_bw()
        
      })
      
      
    }
  
  })
  
  
  # Render plot to UI
  output$ZeroInflated_Plot <- renderPlot({ # This part and bellow is specific to UI 
    globalVars$ZeroInflated
  })
  
  # Download button for plots (call reactive function here to get plot object) ---- 
  output$downloadZeroInflated_Plot <- downloadHandler(  
    filename = function() { paste('ZeroInflated', input$RQR_plot_format, sep='') },
    content = function(file) {
      ggsave(file, plot = globalVars$ZeroInflated_Plot, device = input$ZeroInflated_Plot_format, 
             height = as.numeric(input$ZeroInflated_Plot_height), width = as.numeric(input$ZeroInflated_Plot_width), 
             units = input$ZeroInflated_Plot_units)
    }
  )
  
  observeEvent(input$code_ZeroInflated, {
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
      prepare_scaled_data(),
      fitmodel(),
      ZeroInflated()
    )
    
    displayCodeModal(
      code, 
      title = "Zero Inflation Test",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  ##########################################################
  # Pearson Plots
  ##########################################################
  Pearson_Residual <- metaReactive2({
    req(globalVars$model)
    dat <- globalVars$dataset
    model <- globalVars$model
    
    
    metaExpr({
      
    ggdat <- tibble(r2= resid(model, type = "pearson")^2,
                    lambdas = fitted(model))
    ggplot(ggdat) +
      geom_point(aes(x=lambdas, y=r2)) +
      geom_hline(yintercept = 1, linetype="dotted", color="red") +
      geom_smooth(aes(x=lambdas, y=r2), color="black") +
      theme_bw() +
      xlab(bquote(lambda)) +
      ylab(bquote(r^2))
    })
  
  })
  
  
  # Render plot to UI
  output$Pearson_Residual_Plot <- renderPlot({ # This part and bellow is specific to UI 
    globalVars$Pearson_Residual
  })
  
  # Download button for plots (call reactive function here to get plot object) ---- 
  output$downloadRQRPlot <- downloadHandler(  
    filename = function() { paste('Pearson_Residual', input$RQR_plot_format, sep='') },
    content = function(file) {
      ggsave(file, plot = globalVars$Pearson_Residual_Plot, device = input$Pearson_Residual_Plot_format, 
             height = as.numeric(input$Pearson_Residual_Plot_height), width = as.numeric(input$Pearson_Residual_Plot_width), 
             units = input$Pearson_Residual_Plot_units)
    }
  )
  
  observeEvent(input$code_Pearson_Residual, {
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
      prepare_scaled_data(),
      fitmodel(),
      Pearson_Residual()
    )
    
    displayCodeModal(
      code, 
      title = "Interaction Marginal Effects Plot",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  
  ##########################################################
  # RQR Plots HERE1
  ##########################################################
  
  RQRPlot <- metaReactive2({ 
      
    req(globalVars$model)
    dat <- globalVars$dataset
    model <- globalVars$model
    model_type <- globalVars$model_choice
    

    if (model_type == "Poisson") {
      metaExpr({

        counts <- model$y
        lambdas <- fitted(model)
        
        rqr <- rep(NA, length(lambdas))
        for(i in 1:length(lambdas)){
          ai <-ppois(counts[i]-1,lambda=lambdas[i])
          bi <-ppois(counts[i], lambda=lambdas[i])
          ui <-ai+runif(1)* (bi-ai)
          ui <-max(min(ui,1-10^(-6)),10^(-6))
          rqr[i] <-qnorm(ui)
        }
        pearson.ratio <-sum(residuals(model, type= "pearson")^2)/model$df.residual
        
        p1<-ggplot(data=tibble(lambda=lambdas,e=rqr)) +
          geom_hline(yintercept=0,linetype="dotted")+
          geom_point(aes(x=lambda, y=e))+
          theme_bw()+
          xlab(bquote(lambda))+
          ylab("RandomizedQuantileResiduals")
        p2<-ggplot(data=tibble(e=rqr)) +
          stat_qq(aes(sample=e)) +
          stat_qq_line(aes(sample=e))+
          theme_bw()+
          xlab("Theoretical")+
          ylab("Observed")+
          ggtitle(paste("DispersionRatio=",round(pearson.ratio,4)))
        p1+p2
      })
    }else if (model_type == "Negative Binomial") {
        metaExpr({
      
          counts <- model$y
          mus<- predict(model,type= "response")
          rqr<- rep(NA, length(mus))
          theta <- model$theta
          for(i in 1:length(mus)){
            ai <-pnbinom(counts[i]-1,size= theta, mu= mus[i])
            
            bi <-pnbinom(counts[i],size= theta,mu= mus[i])
            ui <-ai+runif(1)* (bi-ai)
            ui <-max(min(ui,1-10^(-6)),10^(-6))
            rqr[i] <-qnorm(ui)
          }
          pearson.ratio <-sum(residuals(model,type= "pearson")^2) /model$df.residual
          
          p1<-ggplot(data=tibble(mu=mus, e=rqr)) +
            geom_hline(yintercept=0,linetype="dotted")+
            geom_point(aes(x=mu,y=e))+
            theme_bw()+
            xlab(bquote(mu[y|x]))+
            ylab("Randomized QuantileResiduals")
          p2<-ggplot(data=tibble(e=rqr)) +
            stat_qq(aes(sample=e)) +
            stat_qq_line(aes(sample=e))+
            theme_bw()+
            xlab("Theoretical")+
            ylab("Observed")+
            ggtitle(paste("DispersionRatio=",round(pearson.ratio,4)))
          p1+p2
        })
    }else if (model_type == "Quasi-Poisson") {
      metaExpr({
        
        counts <- model$y
        lambdas <- fitted(model)
        
        rqr <- rep(NA, length(lambdas))
        for(i in 1:length(lambdas)){
          ai <-ppois(counts[i]-1,lambda=lambdas[i])
          bi <-ppois(counts[i], lambda=lambdas[i])
          ui <-ai+runif(1)* (bi-ai)
          ui <-max(min(ui,1-10^(-6)),10^(-6))
          rqr[i] <-qnorm(ui)
        }
        pearson.ratio <-sum(residuals(model, type= "pearson")^2)/model$df.residual
        
        p1<-ggplot(data=tibble(lambda=lambdas,e=rqr)) +
          geom_hline(yintercept=0,linetype="dotted")+
          geom_point(aes(x=lambda, y=e))+
          theme_bw()+
          xlab(bquote(lambda))+
          ylab("RandomizedQuantileResiduals")
        p2<-ggplot(data=tibble(e=rqr)) +
          stat_qq(aes(sample=e)) +
          stat_qq_line(aes(sample=e))+
          theme_bw()+
          xlab("Theoretical")+
          ylab("Observed")+
          ggtitle(paste("DispersionRatio=",round(pearson.ratio,4)))
        p1+p2
        
      })
    }else if (model_type == "Zero-Inflated Poisson") {
      
      counts <-  model$frame[[1]]
      lambdas <- fitted(model, type = "count")
      pis <- predict(model, type = "zprob")
      rqr <- rep(NA, length(lambdas))
      
      for(i in 1:length(lambdas)){
        ai <- pzpois(counts[i]-1, lambda=lambdas[i], pi=pis[i])
        bi <- pzpois(counts[i], lambda=lambdas[i], pi=pis[i])
        # this works even when ai=bi
        ui <- ai + runif(1) * (bi - ai)
        ui <- max(min(ui, 1-10^(-6)), 10^(-6))
        rqr[i] <- qnorm(ui)
      }
      
      pearson.ratio <- sum(residuals(model, type = "pearson")^2) / df.residual(model)
      
      p1 <- ggplot(data=tibble(lambda=lambdas,
                               e=rqr)) + 
        geom_hline(yintercept=0, linetype="dotted")+
        geom_point(aes(x=lambda, y=e)) +
        theme_bw()+
        xlab(bquote(lambda))+
        ylab("Randomized Quantile Residuals")
      p2 <- ggplot(data=tibble(e=rqr)) +
        stat_qq(aes(sample=e)) +
        stat_qq_line(aes(sample=e)) +
        theme_bw() +
        ggtitle(paste("Dispersion Ratio =", round(pearson.ratio, 4)))
      p1+p2
       
        
    }else if (model_type == "Zero Inflated Negative Binomial") {
      
     # browser()
      counts <-  model$frame[[1]]
      mus <- predict(model, type = "conditional")
      pis <- predict(model, type = "zprob")
      theta_val <- sigma(model) # This is your dispersion parameter
      rqr <- rep(NA, length(mus))
      
      for(i in 1:length(mus)){
        ai <- pznbinom(counts[i]-1, theta=theta_val, 
                       mu=mus[i], pi=pis[i])
        bi <- pznbinom(counts[i], theta=theta_val, 
                       mu=mus[i], pi=pis[i])
        ui <- ai + runif(1) * (bi - ai)
        ui <- max(min(ui, 1-10^(-6)), 10^(-6))
        rqr[i] <- qnorm(ui)
      }
      
      pearson.ratio <- sum(residuals(model, type = "pearson")^2) / df.residual(model)
      p1 <- ggplot(data=tibble(mu=mus,
                               e=rqr)) + 
        geom_hline(yintercept=0, linetype="dotted")+
        geom_point(aes(x=mu, y=e)) +
        theme_bw()+
        xlab(bquote(mu))+
        ylab("Randomized Quantile Residuals")
      p2 <- ggplot(data=tibble(e=rqr)) +
        stat_qq(aes(sample=e)) +
        stat_qq_line(aes(sample=e)) +
        theme_bw() +
        ggtitle(paste("Dispersion Ratio =", round(pearson.ratio, 4)))
      p1+p2
      
        
    }else if (model_type == "Tweedie") {
      #browser()
      metaExpr({
        rqr <- statmod::qres.tweedie(model)
        fitted_vals <- fitted(model)
        
        pearson.ratio <- sum(residuals(model, type = "pearson")^2) / model$df.residual
        
        p1 <- ggplot(data = tibble(fitted = fitted_vals, e = rqr)) +
          geom_hline(yintercept = 0, linetype = "dotted") +
          geom_point(aes(x = fitted, y = e), alpha = 0.5) +
          theme_bw() +
          xlab("Fitted Values") +
          ylab("Randomized Quantile Residuals")
        
        p2 <- ggplot(data = tibble(e = rqr)) +
          stat_qq(aes(sample = e)) +
          stat_qq_line(aes(sample = e)) +
          theme_bw() +
          xlab("Theoretical") +
          ylab("Observed") +
          ggtitle(paste("Dispersion Ratio =", round(pearson.ratio, 4)))
        
        p1 + p2
      })
    }
    
})
  
# Render plot to UI
  output$RQR_plot <- renderPlot({
      req(globalVars$RQRPlot) 
      globalVars$RQRPlot
  })
  
  output$RQR_plot2 <- renderPlot({
    req(globalVars$RQRPlot) 
    globalVars$RQRPlot
  })

  output$check_plot <- renderPlot({
    globalVars$make_check_plot
  })

# Download button for plots (call reactive function here to get plot object) ---- (NOTE PLOT) 5
output$downloadRQRPlot <- downloadHandler(  
  filename = function() { paste('RQRplot.', input$RQR_plot_format, sep='') },
  content = function(file) {
    ggsave(file, plot = globalVars$RQRPlot, device = input$RQR_plot_format, 
           height = as.numeric(input$RQR_plot_height), width = as.numeric(input$RQR_plot_width), 
           units = input$RQR_plot_units)
  }
)


# Display code for ci visualization plot ---- (NOTE PLOT) 6
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
    prepare_scaled_data(),
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


# Render table to UI  
output$vifTab <- DT::renderDataTable({
  globalVars$make_vif_num
},
extensions = 'Buttons',
options = list(
  dom = 'Bfrtip',
  buttons = 
    list("copy", "print", list(
      extend = "collection",
      buttons = list(
        list(extend="csv", filename="model-vif"),
        list(extend="excel", filename="model-vif"),
        list(extend="pdf", filename="model-vif")
      ),
      text = "Download",
      filename = "model-vif"
    ))
), rownames = FALSE)

make_vif_num <- metaReactive2({
  req(globalVars$model)
  model <- globalVars$model
  choice <- globalVars$model_choice
  dat <- globalVars$dataset
  
  if (choice %in% c("Zero-Inflated Poisson", "Zero Inflated Negative Binomial")) {
    term_count <- length(all.vars(formula(model))) - 1 
    count_formula <- formula(model, lhs=1, rhs=1)
    vif_model <- glm(count_formula, data = dat, family = poisson)
  }else {
    term_count <- length(attr(terms(model), "term.labels"))
    vif_model <- model 
  }
  
  variables <- all.vars(formula(model))
  
  is_categorical <- FALSE
  
  # Check Categorical Levels
  for (i in 2:length(variables)) {
    var_name <- variables[i]
    column_data <- dat[[var_name]]
    if (!is.numeric(column_data)) {
      is_categorical <- TRUE
      break
    }
  }
  
  if(term_count >= 2){
    shinyjs::show("vifdiv")

    vif_structure <- tryCatch(car::vif(vif_model), error = function(e) NULL)
    
   
    
    
    if(!is.null(vif_structure) && !is_categorical && is.null(nrow(vif_structure))) {
      # Case: Standard VIF (No categorical variables with >1 Df)
      metaExpr({
        "####################################"
        "# Compute VIF"
        "####################################"
        model.vif <- car::vif(vif_model)
        model.vif <- tibble::tibble(Terms = names(model.vif),
                                    VIF = model.vif) %>%
          dplyr::mutate_if(is.numeric, round, 4)
      })
    } else if (!is.null(vif_structure)) {
      # Case: GVIF (Includes categorical variables like 'EPAregion')
      metaExpr({
        "####################################"
        "# Compute GVIF"
        "####################################"
        model.vif <- car::vif(vif_model)
   
        model.vif <- as.data.frame(model.vif) %>%
          mutate(Term = rownames(.)) %>%
          relocate(Term) %>%
          set_rownames(NULL)
      }) 
    }
  } else {
    shinyjs::hide("vifdiv")
    return(NULL)
  }
}, inline=TRUE)
# Download button for summary (LaTeX version)----
output$downloadvifLatex <- downloadHandler(
  filename = function() {
    paste("model-viftable.tex", sep="")
  },
  content = function(file) {
    base::print(xtable(globalVars$make_vif_num, digits = 4), file, type = "latex")
  }
)

# R Code
observeEvent(input$code_vif, {
  code <- expandChain(
    "# Ensure to load your data as an object called dat.",
    quote({
      library(tidyverse) 
      library(magrittr)
      library(car)
      library(effectsize)
    }),
    "####################################",
    "# Load Data",
    "####################################",
    read_data(),
    refactor_data(),
    prepare_scaled_data(),
    fitmodel(),
    make_vif_num()
  )
  displayCodeModal(
    code, 
    title = "Regression VIF Table",
    size = "l", 
    fontSize = 16,
    clip=NULL
  )
})


#############################################################################################
# Leverage and Outliers
#############################################################################################
# Code check plot
make_check_plot <- metaReactive2({
  req(globalVars$model)
  model<-globalVars$model
  mod_type <-  globalVars$model_choice

    if (mod_type == "Zero-Inflated Poisson" || mod_type == "Zero Inflated Negative Binomial"){
      obs_zeros <- sum(model$frame[1] == 0)
      
      numZeros <-rep(NA,1000)
      
      for (i in 1:1000){
        numZeros[i] <- sum(simulate(model) == 0)
      }
      
      mean(numZeros <= obs_zeros)
      
      sim_data <- data.frame(zeros = numZeros)
      
      p1 <- ggplot(sim_data, aes(x = zeros)) +
        geom_histogram(aes(y = after_stat(density)), 
                       binwidth = 1, 
                       fill = "#5b5b5b", 
                       color = "blue") +
        
        geom_vline(xintercept = obs_zeros, 
                   color = "red", 
                   linetype = "dotted", 
                   linewidth = 1) +
        
        labs(title = "Assessing Zero Inflation",
             x = "Observed Zeros",
             y = "Density") +
        
        theme_bw()
      
      
      "####################################"
      "# 2. Overdispersion Check (p2)"
      "####################################"
      # Your r^2 vs Lambda implementation
      ggdat <- tibble(r2= resid(model, type = "pearson")^2,
                      lambdas = fitted(model))
      p2 <- ggplot(ggdat) +
        geom_point(aes(x=lambdas, y=r2)) +
        geom_hline(yintercept = 1, linetype="dotted", color="red") +
        geom_smooth(aes(x=lambdas, y=r2), color="black") +
        theme_bw() +
        labs(title = "Pearson Residuals", x = bquote(lambdas), y = bquote(r^2))
      
      
      "####################################"
      "# 3. RQR PLOT"
      "####################################"
      res <- simulateResiduals(model)
      
      ggdat <- data.frame(
        fitted = res$fittedPredictedResponse, # This is your 'lambdas'
        rqr = res$scaledResiduals             # These are your 'rqr' values (0 to 1)
      )
      
      ggdat$rqr_norm <- qnorm(ggdat$rqr)
      
      # 3. Create the Plot
      p3 <- ggplot(data = ggdat, aes(x = fitted, y = rqr_norm)) + 
        geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
        geom_point(alpha = 0.5) +
        theme_bw() +
        labs(title = "Randomized Quantile Residuals", x = "Fitted Values", y = "Residuals")
      
      
      
      "####################################"
      "# 4. Pearson Res vs. Index (p4)"
      "####################################"
      res <- residuals(model, type = "pearson")
      ggdat_res <- data.frame(obs = 1:length(res), res = res)
      
      # Logic for outlier highlighting
      ggdat_res <- ggdat_res %>% 
        mutate(Outlier = ifelse(abs(res) > 3, "Red", ifelse(abs(res) > 2, "Orange", "Normal")))
      
      p4 <- ggplot(ggdat_res, aes(x = obs, y = res, color = Outlier)) +
        geom_point(shape = 1) +
        scale_color_manual(values = c("Normal" = "black", "Orange" = "orange", "Red" = "red")) +
        geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
        geom_hline(yintercept = c(-3, -2, 2, 3), linetype = "dotted", alpha = 0.5) +
        theme_bw() +
        theme(legend.position = "none") +
        labs(title = "Pearson Residuals vs Index", x = "Observation Number", y = "Residual")
      
      "####################################"
      "# Print ZIP Diagnostic Grid"
      "####################################"
      (p1 | p2) / (p3 | p4)
    
  }
  else{
  metaExpr({
    
    "####################################"
    "# Create Data for Leverage Plot"
    "####################################"
    d <- model.frame(model) 
    n <- nrow(d)
    p <- length(coef(model))
    d <- d %>% mutate(obs = 1:n)
    ggdat <- d %>% mutate(h.values = hatvalues(model))
    
    "####################################"
    "# Create Leverage Plot"
    "####################################"
    p1<-ggplot(data=ggdat,aes(x=obs)) +
      geom_linerange(aes(ymin=0, ymax=h.values)) +
      theme_bw()+
      xlab("Observation Number")+
      ylab("Leverage")+
      geom_hline(yintercept =2*p/n, linetype="dotted", color="orange",size=.75)+
      geom_hline(yintercept =3*p/n, linetype="dotted", color="red",size=.75)
    
    "####################################"
    "# Create Data for Cook's D Plot"
    "####################################"
    ggdat <- d %>% mutate(cook.d = cooks.distance(model))
    "####################################"
    "# Create Cook's D Plot"
    "####################################"
    p2<-ggplot(data=ggdat, aes(x=obs)) +
      geom_linerange(aes(ymin=0, ymax=cook.d)) +
      theme_bw()+
      theme(axis.text.x = element_text(angle=60, vjust=1, hjust=1))+
      xlab("Observation Number")+
      ylab("Cook's Distance")+
      geom_hline(yintercept = qf(p=0.10, df1=p, df2=n-p), linetype="dotted", color="orange",size=.75)+
      geom_hline(yintercept = qf(p=0.50, df1=p, df2=n-p), linetype="dotted", color="red",size=.75)
    
    "####################################"
    "# Create Data for DFFITS Plot"
    "####################################"
    ggdat <- d %>% mutate(dffits=dffits(model))
    "####################################"
    "# Create DFFITS Plot"
    "####################################"
    p3<-ggplot(data=ggdat, aes(x=obs)) +
      geom_linerange(aes(ymin=0, ymax=dffits)) +
      theme_bw()+
      xlab("Observation Number")+
      ylab("DFFITs")+
      geom_hline(yintercept = c(-2*sqrt(p/n), 2*sqrt(p/n)),
                 linetype="dotted",
                 size=.75,
                 color="orange")+
      geom_hline(yintercept = c(-2,2),
                 linetype="dotted",
                 size=.75,
                 color="red")
    
    "####################################"
    "# Create Data for Residual Plot"
    "####################################"
    ggdat<-data.frame(obs=d$obs,
                      y=rstudent(model))
    ggdat.out2<-ggdat %>% filter(abs(y)>2)
    ggdat.out3<-ggdat %>% filter(abs(y)>3)
    
    "####################################"
    "# Create Residual Plot"
    "####################################"
    p4<-ggplot(data=ggdat,aes(x=obs,y=y))+
      geom_point(shape=1)+
      geom_hline(yintercept = 0,color="red",linetype="dashed")+
      xlab("Observation Number")+
      ylab("Studentized Residual")+
      theme_bw()+
      geom_hline(yintercept = c(-3,3), color="red", linetype="dotted",size=0.75)+
      geom_hline(yintercept = c(-2,2), color="orange", linetype="dotted",size=0.75)+
      geom_point(data=ggdat.out2, aes(x=obs,y=y), fill="orange", shape=21)+
      geom_point(data=ggdat.out3, aes(x=obs,y=y), fill="red", shape=21)
    
    "####################################"
    "# Print Plots"
    "####################################"
    (p1|p2)/(p3|p4)
  })
  }
},inline=TRUE)

# Render plot to UI
output$check_plot <- renderPlot({
  globalVars$make_check_plot
})

# Download button for plot
output$downloadcheckPlot <- downloadHandler(
  filename = function() { paste('checkplots.', input$check_plot_format, sep='') },
  content = function(file) {
    ggsave(file, plot = globalVars$make_check_plot, device = input$check_plot_format, 
           height = as.numeric(input$check_plot_height), width = as.numeric(input$check_plot_width), 
           units = input$check_plot_units)
  }
)

# Rcode
observeEvent(input$code_check, {
  code <- expandChain(
    "# Ensure to load your data as an object called dat.",
    quote({
      library(tidyverse) 
      library(patchwork)
    }),
    "####################################",
    "# Load Data",
    "####################################",
    read_data(),
    refactor_data(),
    prepare_scaled_data(),
    fitmodel(),
    make_check_plot()
  )
  
  displayCodeModal(
    code, 
    title = "Regression Leverage-Outlier-Influence Plot",
    size = "l", 
    fontSize = 16,
    clip=NULL
  )
})




############################################################################
# HANDLE CHECKING FOR OBSERVATIONS
############################################################################
observeEvent(input$check_obs, {
  shinyjs::show("check_note")
  
  if(input$check_1 == TRUE && input$check_2 == TRUE && input$check_3 == TRUE && input$check_4 == TRUE){
    discuss_text <- "The model likely isn't affected by outliers or high-leverage points. Please proceed to the next step."
    output$check_note <- renderText(discuss_text)
  } else{
    discuss_text <- "There are observations that might have an outsized effect on the model fit. Please re-evaluate the data and determine if the influential and/or outlying points are relevant to the data."
    output$check_note <- renderText(discuss_text)
  }
  
})

############################################################################
# HANDLE CHECKING FOR ASS.
############################################################################
observeEvent(input$check_asmp, {
  shinyjs::show("asmp_note")
  
  if(input$asmp_1 == FALSE){
    discuss_text <- "The collected data needs to be representative of the population of interest and the observations must be independent of one another. Inferences may not properly reflect the population. Please consider finding a new dataset.\n"
    output$asmp_note <- renderText(discuss_text)
  } else if (input$asmp_6 == FALSE){
    discuss_text <- "The model does not appear to be well fitting of your data. Please try selecting another model instead.\n"
    output$asmp_note <- renderText(discuss_text)
  } else if (input$asmp_2 == FALSE){
    discuss_text <- "The collected data shows evidence of (multi)collinearity. Please reevaluate your regression equation and eliminate any redundancies before proceeding.\n"
    output$asmp_note <- renderText(discuss_text)
  } else if (input$asmp_3 == FALSE){
    discuss_text <- "There may be insufficiently many events for each predictor, which may lead to issues in reliability. Please consider bootstrapping, resampling, or finding an alternative data set.\n"
    output$asmp_note <- renderText(discuss_text)
  } else if (input$asmp_4 == FALSE){
    discuss_text <- "There may be an issue with the polynomial order of your regression model. Please consider adding a squared term.\n"
    output$asmp_note <- renderText(discuss_text)
  } else if (input$asmp_5A == FALSE && input$model_choice %in% c("Poisson", "Negative Binomial")){
    discuss_text <- "The model appears to suffer from zero-inflation. It is recommended that you switch to one of the zero-inflated models instead.\n"
    output$asmp_note <- renderText(discuss_text)
  } else if (input$asmp_5B == FALSE && input$model_choice %in% c("Zero-Inflated Poisson", "Zero Inflated Negative Binomial")){
    discuss_text <- "The model does not appear to have a mixed process for generating zeros and counts. More specifically, the structural zeros may not be logistic. Please try refitting the model with a standard model instead of a zero-inflated model."
    output$asmp_note <- renderText(discuss_text)
  } else{
    discuss_text <- "All conditions for use of this model have been met. Please proceed to the next step."
    output$asmp_note <- renderText(discuss_text)
  }
  
  
  
})



#############################################################################################
# Scatterplot Matrix
#############################################################################################
# Code to create plot
make_ggpairs_plot <- metaReactive2({
  req(globalVars$model)
  model<- globalVars$model
  choice <- globalVars$model_choice
  dat <- globalVars$dataset
  
  metaExpr({
    "####################################"
    "# Create Pairwise Plots"
    "####################################"
    # problem: switch cor and points below
    # problem: can we make discrete a better plot? (mosaic?)
    upper <- list(continuous = "points", discrete = wrap("colbar", size = 0), combo = "box_no_facet", na = "na")
    lower <- list(continuous = "cor", discrete = wrap("colbar", size = 0), combo = "box_no_facet", na = "na")
      

    variables <- all.vars(formula(model))
 
    ggpairs(dat[, variables], progress = F, upper = upper, lower = lower) +
        theme_bw()+
        theme(axis.text.x = element_text(angle=60, vjust = 1, hjust=1)) + 
        scale_fill_grey()+
        scale_color_grey()
     
  })
},inline=TRUE)

# Render plot to UI
output$ggpairs_plot<- renderPlot({
  globalVars$make_ggpairs_plot
})

# Download button for plots (call reactive function here to get plot object) ----
output$downloadggpairsPlot <- downloadHandler(
  filename = function() { paste('pairwiseplots.',input$ggpairs_plot_format, sep='') },
  content = function(file) {
    ggsave(file, plot = globalVars$make_ggpairs_plot, device = input$ggpairs_plot_format, 
           width = as.numeric(input$ggpairs_plot_width), height = as.numeric(input$ggpairs_plot_height), 
           units = input$ggpairs_plot_units)
  }
)


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

# R Code
observeEvent(input$code_ggpairsplot, {
  code <- expandChain(
    "# Ensure to load your data as an object called dat",
    quote({
      library(GGally)
      library(tidyverse)
      #library(ggforce)
    }),
    "####################################",
    "# Load Data",
    "####################################",
    read_data(),
    refactor_data(),
    prepare_scaled_data(),
    fitmodel(),
    make_ggpairs_plot()
  )
  
  displayCodeModal(
    code, 
    title = "Pairwise Plots",
    size = "l", 
    fontSize = 16,
    clip=NULL
  )
})

#############################################################################################
# Correlation Matrix
#############################################################################################
# Code to create table
make_ggpairs_summary <- metaReactive2({
  req(globalVars$model)
  model <- globalVars$model
  choice <-  globalVars$model_choice
  
  if (choice == "Zero-Inflated Poisson" || choice == "Zero Inflated Negative Binomial" ){
    
    metaExpr({
      "####################################"
      "# Create Data for Correlation Matrix"
      "####################################"
      modmatrix <- model$frame %>% 
        select_if(is.numeric)
      cormats<-Hmisc::rcorr(as.matrix(modmatrix), type = "pearson")
      cormat<-round(cormats$r,4)
      pmat<-as.matrix(data.frame(stars.pval(cormats$P)))
      "####################################"
      "# Create Correlation Matrix"
      "####################################"
      cormat<-matrix(paste(cormat, pmat, sep=""),nrow(cormats$r),ncol(cormats$r))
      rownames(cormat)<-colnames(cormats$r)
      colnames(cormat)<-rownames(cormats$r)
      data.frame(cormat)
    })
    
  }
  else{
  
    metaExpr({
      "####################################"
      "# Create Data for Correlation Matrix"
      "####################################"
      modmatrix <- model.frame(model) %>% 
        select_if(is.numeric)
      cormats<-Hmisc::rcorr(as.matrix(modmatrix), type = "pearson")
      cormat<-round(cormats$r,4)
      pmat<-as.matrix(data.frame(stars.pval(cormats$P)))
      "####################################"
      "# Create Correlation Matrix"
      "####################################"
      cormat<-matrix(paste(cormat, pmat, sep=""),nrow(cormats$r),ncol(cormats$r))
      rownames(cormat)<-colnames(cormats$r)
      colnames(cormat)<-rownames(cormats$r)
      data.frame(cormat)
    })
  }
},inline=TRUE)

# Render table to UI
output$ggpairs_summary <- DT::renderDataTable({
  globalVars$make_ggpairs_summary
},
extensions = 'Buttons',
options = list(
  dom = 'Bfrtip',
  buttons = 
    list("copy", "print", list(
      extend = "collection",
      buttons = list(
        list(extend="csv", filename="model-correlations"),
        list(extend="excel", filename="model-correlations"),
        list(extend="pdf", filename="model-correlations")
      ),
      text = "Download",
      filename = "model-correlations"
    ))
), rownames = FALSE)

# Download button for summary (LaTeX version)----
output$downloadcormatLatex <- downloadHandler(
  filename = function() {
    paste("model-correlations.tex", sep="")
  },
  content = function(file) {
    base::print(xtable(globalVars$make_ggpairs_summary, digits = 4), file, type = "latex")
  }
)

#Rcode
observeEvent(input$code_corrmat, {
  code <- expandChain(
    "# Ensure to load your data as an object called dat.",
    quote({
      library(gtools)
      library(Hmisc)
      library(tidyverse)
    }),
    "####################################",
    "# Load Data",
    "####################################",
    read_data(),
    refactor_data(),
    prepare_scaled_data(),
    fitmodel(),
    make_ggpairs_summary()
  )
  
  displayCodeModal(
    code, 
    title = "Correlation Matrix",
    size = "l", 
    fontSize = 16,
    clip=NULL
  )
})


#############################################################################################
# Model ANOVA Table
#############################################################################################
# Code for model summary table

prepare_anova <- metaReactive2({
  
  req(globalVars$model)
  dat <- globalVars$dataset
  model <- globalVars$model
  type <-  globalVars$model_choice
  
  if (type == "Zero-Inflated Poisson" || type == "Zero Inflated Negative Binomial"){
    metaExpr({
      
      coef.table <- data.frame(summary(model)$coefficients$cond) 
      int.table <- coef.table[grep(x=rownames(coef.table), pattern=":", fixed = T),]
      anova_type <- if(nrow(int.table) > 0 && any(int.table[,4] < 0.05)) "III" else "II"    
      
      "####################################"
      "# ANOVA Table Extraction"
      "####################################" 
      anova.cond <- Anova(model, type = anova_type, component = "cond", test = "Chisq")
      anova.zi   <- Anova(model, type = anova_type, component = "zi",   test = "Chisq")
      
      null_loglik <- as.numeric(logLik(update(model, . ~ 1))) 
      
      "####################################"
      "# Combine and Clean Up Labels"
      "####################################"
      
      mod.table_count <- data.frame(anova.cond) %>% 
        mutate(
          "Term" = rownames(.), 
          "Model Part" = "Count Model",
          "Partial McFadden R2" = Chisq / (-2 * null_loglik)
        ) %>% 
        relocate(Term, `Model Part`) %>%  
        set_rownames(NULL) %>%
        set_colnames(c("Term", "Model Part", "LR Chisq (Deviance)", "df", "p-value", "Partial McFadden R2"))
      
      mod.table_zero <- data.frame(anova.zi) %>% 
        mutate(
          "Term" = rownames(.), 
          "Model Part" = "Zero-Inflation Model",
          "Partial McFadden R2" = Chisq / (-2 * null_loglik)
        ) %>% 
        relocate(Term, `Model Part`) %>%  
        set_rownames(NULL) %>%
        set_colnames(c("Term", "Model Part", "LR Chisq (Deviance)", "df", "p-value", "Partial McFadden R2"))
      
      anova.table <- rbind(mod.table_count,mod.table_zero)
      

    })
  }    

  else{
    coef.table <- data.frame(summary(model)$coefficients) 
    int.table <- coef.table[grep(x=rownames(coef.table), pattern=":", fixed = T),]
    if(nrow(int.table)>0 && any(int.table[4]<0.05)){ #significant interaction
      metaExpr({
        "####################################"
        "# ANOVA Table"
        "####################################" 
        anova.table <- Anova(model, type = "III", test = "LR")
        null_deviance <- model$null.deviance
        
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        
        
        anova.table <- as.data.frame(anova.table) %>%
          mutate(Term = rownames(.)) %>%
          relocate(Term) %>%
          mutate(`Partial McFadden R2` = `LR Chisq` / null_deviance)%>%
          rename(`p-value` = `Pr(>Chisq)`) %>%
          set_colnames(c("Term","LR Chisq (Deviance)", "df", "p-value", "Partial McFadden R2")) %>%
          set_rownames(NULL)
      })
    }else{
      metaExpr({
          "####################################"
          "# ANOVA Table"
          "####################################" 
          anova.table <- Anova(model, type = "II", test = "LR")
          null_deviance <- model$null.deviance
          
  
          "####################################"
          "# Clean Up Labels for Printing"
          "####################################"
          anova.table <- as.data.frame(anova.table) %>%
            mutate(Term = rownames(.)) %>%
            relocate(Term) %>%
            mutate(`Partial McFadden R2` = `LR Chisq` / null_deviance)%>%
            rename(`p-value` = `Pr(>Chisq)`) %>%
            set_colnames(c("Term","LR Chisq (Deviance)", "df", "p-value", "Partial McFadden R2")) %>%
            set_rownames(NULL)
          
        }) 
    }
  }
#  }
  
}, inline=TRUE)

make_anova_num <- metaReactive2({
  
  req(globalVars$anova)
  anova.table <- globalVars$anova
  metaExpr({
    "####################################"
    "# Print Table"
    "####################################"
    anova.table %>% 
      mutate_if(is.numeric, round, 4)%>%
      mutate(`p-value` = ifelse(`p-value` < 0.0001, "<0.0001", `p-value`))
    
  })
}, inline=TRUE)

# Render table to UI  
output$anovaTab <- DT::renderDataTable({
  globalVars$make_anova_num
},
extensions = 'Buttons',
options = list(
  dom = 'Bfrtip',
  buttons = 
    list("copy", "print", list(
      extend = "collection",
      buttons = list(
        list(extend="csv", filename="model-anova"),
        list(extend="excel", filename="model-anova"),
        list(extend="pdf", filename="model-anova")
      ),
      text = "Download",
      filename = "model-anova"
    ))
), rownames = FALSE)


# Download button for summary (LaTeX version)----
output$downloadanovaLatex <- downloadHandler(
  filename = function() {
    paste("model-anovatable.tex", sep="")
  },
  content = function(file) {
    base::print(xtable(globalVars$make_anova_num, digits = 4), file, type = "latex")
  }
)

# R Code
observeEvent(input$code_anova, {
  code <- expandChain(
    "# Ensure to load your data as an object called dat.",
    quote({
      library(tidyverse) 
      library(magrittr)
      library(car)
      library(effectsize)
    }),
    "####################################",
    "# Load Data",
    "####################################",
    read_data(),
    refactor_data(),
    prepare_scaled_data(),
    fitmodel(),
    prepare_anova(),
    make_anova_num()
  )
  
  displayCodeModal(
    code, 
    title = "Regression ANOVA Table",
    size = "l", 
    fontSize = 16,
    clip=NULL
  )
})

#NExt section now
output$anovainterp <- renderUI({  
  globalVars$prepare_anova_interp
})


# Now Were here
prepare_anova_interp <- function(){
  req(globalVars$anova)
  
  anova.table<-globalVars$anova %>%
    mutate(e2.text = case_when(`Partial McFadden R2`<0.02     ~ "minuscule and perhaps negligible.",
                               `Partial McFadden R2`>=0.02 & `Partial McFadden R2`<0.10  ~ "small.",
                               `Partial McFadden R2`>=0.10 & `Partial McFadden R2`<0.20  ~ "moderate.",
                               `Partial McFadden R2`>=0.20           ~ "large."))
  alpha <- input$alpha
  
  anova.interp <- NULL
  for(i in 1:(nrow(anova.table))){
    if(grepl("Intercept", anova.table$Term[i], ignore.case = TRUE)){
      #Skip
    }

    else if(!grepl(x=anova.table$Term[i], pattern=":")){ #HERE
      anova.interp <- (paste(anova.interp,"\U2022 The effect of ", sub("\\.scaled$", "", anova.table$Term[i]), " is ", ifelse(anova.table$`p-value`[i]<alpha, "statistically discernible ", "not significant "),
                             "(LR Chisq (Deviance) = ",  round(anova.table$`LR Chisq (Deviance)`[i],4),
                             ", p-value ",  ifelse(anova.table$`p-value`[i]<0.0001,"< 0.0001", paste("= ", round(anova.table$`p-value`[i],4), sep="")), ").",
                             ifelse(!is.na(anova.table[["Partial McFadden R2"]][i]),
                                    paste(" The effect size is ", round(anova.table[["Partial McFadden R2"]][i], 3), ", which indicates that the effect is ", anova.table$e2.text[i], sep=""), ""),
                             "<br/>", sep=""))
    }else{
      anova.interp <- (paste(anova.interp,"\U2022 The interactive effect of ", gsub("\\.scaled", "", anova.table$Term[i]), " is ", ifelse(anova.table$`p-value`[i]<alpha, "statistically discernible ", "not significant "),
                             "(LR Chisq (Deviance) = ", round(anova.table$`LR Chisq (Deviance)`[i],4),
                             ", p-value ",  ifelse(anova.table$`p-value`[i]<0.0001,"< 0.0001", paste("= ", round(anova.table$`p-value`[i],4), sep="")), ").",
                             ifelse(!is.na(anova.table[["Partial McFadden R2"]][i]),
                                    paste(" The effect size is ", round(anova.table[["Partial McFadden R2"]][i], 3), ", which indicates that the effect is ", anova.table$e2.text[i], sep=""), ""),
                             "<br/>", sep=""))
    }
  }
  
  HTML(anova.interp)
}

#############################################################################################
# ANOVA Comparison
#############################################################################################
prepare_anova_fctcomp <- metaReactive2({
  req(globalVars$model)
  dat <- globalVars$dataset
  model <- globalVars$model
  choice <- globalVars$model_choice


  metaExpr({
    if (choice == "Zero-Inflated Poisson" || choice == "Zero Inflated Negative Binomial" ){
      all_vars <- all.vars(formula(model))
      fct.vars <- all_vars[sapply(dat[all_vars], is.factor)]
    }
    else{
      fct.vars <- names(which(attr(model$terms, "dataClasses")=="factor"))
    }
    # Compare the different levels
    anova_fctcomp.table <- NULL
    
    for(fctvar in fct.vars){
      emm.obj <- emmeans(model, specs = fctvar, regrid = "response",data = dat)
      emm.test <- pairs(emm.obj, reverse = TRUE)
      emm.ci <- confint(emm.test)
      emm.df <- data.frame(emm.test)
      
      eff_col <- if("ratio" %in% names(emm.df)) "ratio" else "estimate"
      
      emm.df$Variable <- rep(fctvar, nrow(emm.df))
      emm.df$Lower.CI <- emm.ci[[grep("LCL", names(emm.ci))]]
      emm.df$Upper.CI <- emm.ci[[grep("UCL", names(emm.ci))]]
      emm.df$EffectSize <- emm.df[[eff_col]]
      anova_fctcomp.table <- rbind(anova_fctcomp.table, emm.df)   
      }
    
    # Return the table with clean labels
    anova_fctcomp.table %>%
      dplyr::select(Variable, contrast, everything()) %>%
      set_rownames(NULL) %>%
      set_colnames(c("Variable", "Contrast", "Ratio (IRR)", "SE", "df", 
                     "z", "p-value", "Lower CI", "Upper CI", "Effect Size (Ratio)"))
  })
}, inline=TRUE)

make_anovafctcomp_plot <- metaReactive2({
  req(globalVars$anova)
  anova_fctcomp.table <- globalVars$anova_fctcomp
  
  if(length(unique(anova_fctcomp.table$Variable))==1){
    metaExpr({
      "####################################"
      "# Create Plot"
      "####################################"
      ggplot(data=anova_fctcomp.table, aes(x=`Ratio (IRR)`, y=Contrast))+
        geom_point()+
        geom_errorbar(aes(xmin=`Lower CI`, xmax=`Upper CI`), width=0.1) +
        xlab("Estimate")+
        ylab("Contrast")+
        theme_bw()+
        ggtitle("Factor Comparisons") + 
        xlim(min(anova_fctcomp.table$`Lower CI`), max(anova_fctcomp.table$`Upper CI`)) +
        geom_vline(xintercept = 0, linetype = "dashed")
    })
  }else{
    metaExpr({
      "####################################"
      "# Create Plot"
      "####################################"
      ggplot(data=anova_fctcomp.table, aes(x=`Ratio (IRR)`, y=Contrast))+
        geom_point()+
        geom_errorbar(aes(xmin=`Lower CI`, xmax=`Upper CI`), width=0.1) +
        xlab("Estimate")+
        ylab("Contrast")+
        theme_bw()+
        ggtitle("Factor Comparisons")+
        facet_wrap(~Variable, scales = "free") +
        xlim(min(anova_fctcomp.table$`Lower CI`), max(anova_fctcomp.table$`Upper CI`)) +
        geom_vline(xintercept = 0, linetype = "dashed")
    }) 
  }
}, inline = TRUE)

# Code for model summary table
make_anovafctcomp_num <- metaReactive2({
  
  req(globalVars$anova)
  anova_fctcomp.table <- globalVars$anova_fctcomp
  metaExpr({
    "####################################"
    "# Print Table"
    "####################################"
    anova_fctcomp.table <- anova_fctcomp.table %>% 
      mutate_if(is.numeric, round, 4) %>%
      mutate(`p-value` = ifelse(`p-value` < 0.0001, "<0.0001", `p-value`))
  })
}, inline=TRUE)


output$anova_fctcompinterp <- renderUI({  
  globalVars$prepare_anova_fctcompinterp
})

prepare_anova_fctcompinterp <- function(){ # PROLLY ISSUES

  anova_fctcomp.table <- globalVars$anova_fctcomp %>%
    mutate(d.text=  case_when(`Effect Size (Ratio)`<0.20            ~ "minuscule and perhaps negligible.",
                              `Effect Size (Ratio)`>=0.20 & `Effect Size (Ratio)`<0.50  ~ "small.",
                              `Effect Size (Ratio)`>=0.50 & `Effect Size (Ratio)`<0.80  ~ "moderate.",
                              `Effect Size (Ratio)`>=0.80           ~ "large."))
  alpha <- input$alpha
  
  anova.interp <- NULL
  for(i in 1:(nrow(anova_fctcomp.table))){
    anova.interp <- (paste(anova.interp,"\U2022 The contrast of (", anova_fctcomp.table$Contrast[i], ") is ", ifelse(anova_fctcomp.table$`p-value`[i]<alpha, "statistically discernible ", "not significant "),
                           "(z = ",  round(anova_fctcomp.table$z[i],2),
                           ", p-value ",  ifelse(anova_fctcomp.table$`p-value`[i]<0.0001,"< 0.0001", paste("= ", round(anova_fctcomp.table$`p-value`[i],4), sep="")), 
                           ", 95% CI: ", round(anova_fctcomp.table$`Lower CI`[i],4), ",", round(anova_fctcomp.table$`Upper CI`[i], 4), ").",
                           " The effect size is ", round(anova_fctcomp.table[["Effect Size (Ratio)"]][i], 3), ", which indicates that the effect is ", anova_fctcomp.table$d.text[i], "<br/>",
                           sep=""))
  }
  anova.interp<-paste(anova.interp, "<br/><strong>Note:</strong> This approach contrasts the estimated marginal means with a Tukey adjustment. These values are calculated at the 'average' of the other variables in the model.",
                      "You may want to set the other variables to specific values, which is supported in R but not currently supported in this application.")
  
  HTML(anova.interp)
}

output$anova_fctcomp_plot <- renderPlot({
  globalVars$make_anovafctcomp_plot
})

# Render table to UI  
output$anova_fctcompTab <- DT::renderDataTable({
  globalVars$make_anovafctcomp_num
},
extensions = 'Buttons',
options = list(
  dom = 'Bfrtip',
  buttons = 
    list("copy", "print", list(
      extend = "collection",
      buttons = list(
        list(extend="csv", filename="model-anova_fctcomp"),
        list(extend="excel", filename="model-anova_fctcomp"),
        list(extend="pdf", filename="model-anova_fctcomp")
      ),
      text = "Download",
      filename = "model-anova_fctcomp"
    ))
), rownames = FALSE)


# Download button for summary (LaTeX version)----
output$downloadanovafctcompLatex <- downloadHandler(
  filename = function() {
    paste("model-anovafctcomptable.tex", sep="")
  },
  content = function(file) {
    base::print(xtable(globalVars$make_anovafctcomp_num, digits = 4), file, type = "latex")
  }
)

# Download PDF button for plots (call reactive function here to get plot object) ----
output$downloadanovafactorcomparePlot <- downloadHandler(
  filename = function() { paste('anovafactorcompare.', input$anova_fctcomp_plot_format, sep='') },
  content = function(file) {
    ggsave(file, plot = globalVars$make_anovafctcomp_plot, device = input$anova_fctcomp_plot_format, 
           height = as.numeric(input$anova_fctcomp_plot_height), width = as.numeric(input$anova_fctcomp_plot_width), 
           units = input$anova_fctcomp_plot_units)
  }
)

# R Code
observeEvent(input$code_anova_fctcomp, {
  code <- expandChain(
    "# Ensure to load your data as an object called dat.",
    quote({
      library(tidyverse) 
      library(magrittr)
      library(car)
      library(effectsize)
      library(emmeans)
    }),
    "####################################",
    "# Load Data",
    "####################################",
    read_data(),
    refactor_data(),
    prepare_scaled_data(),
    fitmodel(),
    prepare_anova_fctcomp(),
    make_anovafctcomp_num(),
    make_anovafctcomp_plot()
  )
  displayCodeModal(
    code, 
    title = "Regression ANOVA Factor Comparisons",
    size = "l", 
    fontSize = 16,
    clip=NULL
  )
})


})
