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
                                       tags$p("Please contact us if you have any questions at", align="center", 
                                              tags$a(href="mailto:datascience@colgate.edu", "datascience@colgate.edu."))), 
                              br(), br(), br()
                                  
                                       
                              
                              
                              
                      ),
                      tabPanel("Dataset & Model", "This is where the math happens")
          ) # End of navbarPage
)
          
