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
                                       tags$p("1. Basically its Shaw"),
                                       tags$p("2. adino"),
                                       tags$p("1. idk"),
                                       tags$p("2. idk2")
                                       
                                   
                                      ),
                              h3("How to use this app?", align="center"), 
                              tags$div(class = 'paragraph',tags$hr(),
                                      p("Step1"),
                                      wellPanel(strong("response_name ~ explanatory_1_name + explanatory_2_name + ... + explanatory_k_name")),
                                      
                                      
                                      
                                      
                                      
                                      ),
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
                                actionButton("sample", "Sample Data")
                                
                                
                                
                              ),
                              mainPanel( # Presnets results
                                
                              )
                              
                              ),
                     tabPanel("Refrences"
                              
                              
                              )
                   
                     

                     
                     
          ) # End of navbarPage
          
          
)
          
