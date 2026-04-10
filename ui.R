library(Hmisc)      #correlations and tests (still here, here due to overriding summarize)

##########################################
# Shiny
##########################################
library(shiny)
library(shinyalert)
library(shinyjs)
library(shinymeta)
library(shinythemes)
library(shinycssloaders)
library(shinyAce)
library(shinyBS)
library(DT)
##########################################
# Data
##########################################
library(palmerpenguins)
library(ISLR)
##########################################
# General
##########################################
library(tidyverse) #ggplot2, tibble, tidyr, reader, 
#purrr, dplyr, stringr, forcats
library(magrittr)
library(patchwork) #combining plots
library(xtable) #LaTex Plots

##########################################
# Regression
##########################################
library(GGally)      #pairwise plots
library(gtools)      #pvales for cormat
#library(Hmisc)      #correlations and tests (still here, not loaded due to overriding summarize)
library(lmtest)      #constant error variance
library(margins)     #for marginal effects
library(multcomp)    #glht
library(interactions)#for johnson neyman
library(ggeffects)   #for marginal effects plots
library(emmeans)     #for estimated marginal means
library(car)         #for anova
#library(crayon)      #for removing style from Johnson Neyman output
library(effectsize)
#library(ggforce)

###----------------------------------------------------------------------------###
##U---------------------------------------------------------------------------I###

# taglist allows us to create a bundled package for UI 

ui <- tagList(
  tags$head(tags$link(rel = "icon",  type = "image/x-icon", href = "www/Shaw.png"), 
            tags$style(HTML(".paragraph {margin:auto;max-width: 50%;font-size: 15px; text-align:justify;} h1 {text-align:center;}")),   # Sets the paragraph layout
            tags$style(HTML("div.MathJax_Display{text-align: left !important;}"))), # Sets up math layout
  
  #Style
  tags$style(HTML(".btn {padding:5px; font-size:12px;}")),                          # Sets up custom button        
  tags$style(HTML(".checkbox {font-size:12px; margin:5px;}")),                      # Checkbox label text smaller
  tags$style(HTML(".control-label {font-size:14px;}")),                             # Input label text smaller
  tags$style(HTML(".form-control {height:auto; padding:5px;}")),                    # Text input smaller
  tags$style(HTML(".shiny-input-text {font-size:12px;}")),
  tags$style(HTML(".shiny-input-number {font-size:12px;}")),
  # dropdowns smaller
  tags$style(HTML(".item {height:auto;}")),
  tags$style(HTML(".selectize-input, .selectize-dropdown {height:auto; padding:5px; font-size: 12px;}")),
  # reduce space between inputs
  tags$style(HTML(".form-group {margin-bottom: 5px; }")),
  
  #Custom IDs
  tags$style(HTML("#asmp_1note {font-size:12px;}")), 
  tags$style(HTML("#asmp_2note {font-size:12px;}")),
  tags$style(HTML("#asmp_3note {font-size:12px;}")),
  tags$style(HTML("#asmp_4note {font-size:12px;}")),
  tags$style(HTML("#check_note {font-size:12px;}")),
  tags$style(HTML("#asmp_note {font-size:12px;}")),
  tags$style(type="text/css", ".selectize-input{overflow: auto;}"), # this fixes overflow in selectize
  
  
  #Whats br line break
  # whats id
  
  #tags$div(class = "paragraph", ...) utilizing our paragarph format from before 
  #div created an invisible box, class put in the fomrat we made ealrier
  #tags$hr horizontal rule
  
  
  useShinyjs(),
  navbarPage(title = "Count Regression Toolkit", id = "tabs", theme = shinytheme("flatly"),
             tabPanel("About", 
                      h1("Count Regression" , align = "center"), br(),
                      h3("What is Count Regression?", align = "center"),
                      tags$div(class = 'paragraph',align = "center", tags$hr(),
                               p("Count Regression is a statistical tool by which explanatory variables are used to estimate a discrete count of occurances of an event. The resulting model can be used to evaluate hypothesis about the relationships and make inferences, given that certain conditions are satisified."),
                               tags$p("1. The observations collected are representateive of the population of interest and are independent of one another."),
                               tags$p("2. The variable of interest is a count-response variable."),
                               tags$p("3. The relationship between the predictors and the log-mean is linear."),
                               tags$p("4. Ideally, at least 10-20 events per predictor variable."),
                               tags$p("5. For zero-inflated versions, there is a mixed process generating 0s and counts."),
                               tags$p("6. For zero-inflated versions, the relationship between the predictors and log-odds is linear for the logistic portion."),
                               tags$p("7. Little to no (multi)collinearity.")
                               
                               
                      ),
                      h3("How to use this app?", align="center"), 
                      tags$div(class = "paragraph", tags$hr(),
                               p("Step 1: To use this app, go to the 'Dataset & Model' tab and upload your .csv type dataset, or select a sample dataset."), 
                               p("Step 2: Fit your model by inputting your desired regression equation in the form:"),
                               wellPanel(strong("response_name ~ explanatory_1_name + explanatory_2_name + ... + explanatory_k_name")),
                               p("Designate interaction terms using the * or : symbol between the two variable names. Using the asterisk will include both variables and their interaction (recommended), whereas the colon will only include the interaction. For example, an interaction between explanatory variable 1 and 2 can be specified as follows."),
                               wellPanel(strong("response_name ~ explanatory_1_name * explanatory_2_name + ... + explanatory_k_name")),
                               wellPanel(strong("response_name ~ explanatory_1_name : explanatory_2_name + ... + explanatory_k_name")),
                               p("Step 3: You can visualize the data and see a summary of the different variables in the "), 
                               p("Step 4: You can check the assumptions provided in the 'Assumptions' tab. We recommend assessing assumptions visually using the provided graphical summary and confirming using the numerical summaries. The app will provide a histogram of the residuals, a randomized quantile residual plot, and a qq-plot with the dispersion ratio for the selected regression type. For non-zero inflated models, a zero-inflation assessment will also be provided. Warnings or notices will be provided based on the output within these charts."), 
                               p("Step 5: You can check the effect of outlying, influential, or leverage points in the 'Outliers' tab. Many models exhibit some influential points and researchers should ensure that the results of their model hold when using a robust regression model."), #NOTE: May need to change this depending on what we want to show
                               p("Step 6: A table will be provided detailing the results of a linear model test or a Vuong test, depending on the models being compared."),
                               p("Step 7: The resulting model and interpretation of key values can be found in the 'Interpretation' tab"),
                               p("Step 8 (Optional): If your model has an interaction, the appropriate analyses will be reported in the 'Interaction' tab.")),
                      h3("Contact us", align="center"), 
                      tags$div(class = 'paragraph',align = "center", tags$hr(),
                               tags$p("Please contact us if you have any questions at ", align="center", 
                                      br(),
                                      tags$a(href="mailto:lcambisaca@colgate.edu", "lcambisaca@colgate.edu."),
                                      tags$a(href="mailto:tlanuza@colgate.edu", "tlanuza@colgate.edu.")
                               )
                      ), 
                      br(), br(), br()
             ),
             tabPanel("Tutorial",
                      tabsetPanel(id = "Examples", # Basically creates a subset of tabs inside a page
                                  tabPanel("Example 1",
                                           h1("Example 1", align = "center"),
                                           
                                           tags$div(class = "paragraph", tags$hr(),
                                                    p("Within the regression app, we provide data collected on a representative sample of n=558 White Americans by Cooley et al. (2022). The researchers aimed to assess whether beliefs that White people are poor are associated with the humanization of welfare recipients among White Americans who feel intergroup status threat—namely, those high in racial zero-sum beliefs."),
                                                    p("If this were the case, it would suggest that the link between White-poor beliefs, the humanization of welfare recipients, and welfare policy support may be motivated by a desire to preserve the racial status quo.")
                                           ),
                                           
                                           tags$div(class = "paragraph", 
                                                    p("The researchers used perceived agency of welfare recipients as a measure of humanization, and they wanted to evaluate whether White-poor and racial zero-sum beliefs affect this perception by controlling for education, income, political affiliation (Democrat or not) and their beliefs that Black people are poor."),
                                                    p("Specifically, they hypothesized that the association between White-poor beliefs and the humanization of welfare recipients would be stronger among white Americans who also had higher racial zero-sum beliefs, indicating that an interaction term is necessary."),
                                                    tags$hr(),
                                                    wellPanel(strong("Zagency ~ ZWpoor*Zzerosum + Zedu + Zincome + Democrat + ZBpoor"))
                                           ),
                                           
                                           tags$div(class = "paragraph",  tags$hr(),
                                                    p("The data are quite noisy, and we can see that some variables are discrete. We see that the perceived agency of welfare recipients (humanization) is positively correlated with beliefs that White people are poor, beliefs that Black people are poor, and negatively correlated with zero-sum beliefs. Further, we can see that the perceived agency of welfare recipients (humanization) appears to be more prominent among Democrats than non-democrats."),
                                                    p("While these findings provide some insight toward our research question, they are zero-order, meaning we look at the pairs of correlations independently without considering how all the explanatory variables work together."),
                                                    tags$hr()
                                           ),
                                           # HTML('<center><img src="ex1-datasummary.png"></center>'),
                                           
                                           
                                  ),
                                  tabPanel("Example 2")
                      ) # acts as a contained for multiple tabPanel()
             ),
             tabPanel("Dataset & Model",
                      sidebarPanel( # Handels Data and settings
                        fileInput("file_upload", "Upload a File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        actionButton("sample", "Sample Data"),
                        hidden(div(id='choose_sample', #div is a box
                                   selectInput("sample_data_choice","Sample Data:",
                                               choices = c("Camera Data", "Palmer Penguins", "Bracht et al. MFAP4" ,"U.S. News College Data"),
                                               selected = "U.S. News College Data"))),
                        tags$hr(),
                        hidden(selectizeInput("select_factors",
                                              "Specify Categorical Variables in the Data:",
                                              choices = NULL,
                                              selected = NULL,
                                              multiple = TRUE)),
                        
                        #From here down not set up
                        textInput("equation", "Enter your desired regression equation:"),
                        bsTooltip("equation", "Example: response ~ explanatory_1 + explanatory_2 + ... + explanatory_k",
                                  "right", trigger = "hover", options = list(container = "body")), #helpful will need to adjuts for ZIP
                        checkboxInput("scalevars", "Scale all variables (standardize)", FALSE), #need to see if user wabrs to scale and do so if yes need to implement
                        numericInput("alpha", "Significance level (\u03B1): ", value = 0.05, step = 0.001, min = 0, max = 1),     # alpha level need to adjust if user wants to
                        div(class = "text-center", actionButton("DoCompute", "Compute Model Output")), #DoCompute id for button
                        div(h3("Interaction Analysis:"), id="interaction_analysis"),
                        selectizeInput("var_inter",
                                       "Select Interaction",
                                       choices = c("None"),
                                       selected = "None",
                                       multiple = FALSE),
                        
                        selectizeInput("var_moderator",
                                       "Select Moderator",
                                       choices = c("Select..."),
                                       selected = "Select...",
                                       multiple = FALSE),
                        hidden(checkboxInput("interaction.error", "Error Ribbon for Interaction", TRUE)),
                        
                        
                        
                        
                        
                      ),
                      mainPanel( # Presnets results #value = "---" is internal ID for tab
                        tabsetPanel(id = "workPanel",
                                    tabPanel("Data Preview", br(), value="data", #Value allows us to pick whats data
                                             shinycssloaders::withSpinner(DT::dataTableOutput("preview.data"))),
                                    tabPanel("Data Summary", value = "summary",
                                             fluidPage(
                                               h1("Pairwise Plots", align = "center"), br(),
                                               fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("modsumTab")))),br(), # Here Tom
                                               
                                             
                                               
                                               
                                               
                                               
                                               
                                               
                                               
                                             ),
                                             tags$hr(),
                                             br(),
                                             h1("Correlation Matrix", align = "center"), br()
                                             
                                             
                                             
                                             
                                             
                                    ),
                                    tabPanel("Assumptions", value = "assumptions"),
                                    tabPanel("Outliers", value = "checks"),
                                    tabPanel("Plots", value = "plot")
                                    
                                    
                                    
                                    
                                    
                        )
                        
                      )
                      
             ),
             tabPanel("Refrences"
                      
                      
             )
             
             
             
             
             
  ) # End of navbarPage
  
  
)
