library(Hmisc)      #correlations and tests (still here, here due to overriding summarize)
# viruses.within.nucleus ~ CellType * SampleType
# TOTEXP23 ~ AGELAST + SEX + DIABDX_M18 + HIBPDX + OBTOTV23 + ERTOT23
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
library(marginaleffects)
library(multcomp)    #glht
library(interactions)#for johnson neyman
library(ggeffects)   #for marginal effects plots
library(emmeans)     #for estimated marginal means
library(car)         #for anova
#library(crayon)      #for removing style from Johnson Neyman output
library(effectsize)
#library(ggforce)
library(pscl)
library(DHARMa)
library(tweedie)
library(glmmTMB)
library(statmod)

###----------------------------------------------------------------------------###
##U---------------------------------------------------------------------------I###


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
                               p("Step 2: Choose the model that you wish to use to fit to your data. This can be updated later as well."),
                               p("Step 3: Fit your model by inputting your desired regression equation in the form:"),
                               wellPanel(strong("response_name ~ explanatory_1_name + explanatory_2_name + ... + explanatory_k_name")),
                               p("Designate interaction terms using the * or : symbol between the two variable names. Using the asterisk will include both variables and their interaction (recommended), whereas the colon will only include the interaction. For example, an interaction between explanatory variable 1 and 2 can be specified as follows."),
                               wellPanel(strong("response_name ~ explanatory_1_name * explanatory_2_name + ... + explanatory_k_name")),
                               wellPanel(strong("response_name ~ explanatory_1_name : explanatory_2_name + ... + explanatory_k_name")), #NOTE: Test if this notation still works.
                               p("Step 4: You can visualize the data and see a summary of the different variables in the "), 
                               p("Step 5: You can check the assumptions provided in the 'Assumptions' tab. We recommend assessing assumptions visually using the provided graphical summary and confirming using the numerical summaries. The app will provide a histogram of the residuals, a randomized quantile residual plot, and a qq-plot with the dispersion ratio for the selected regression type. For non-zero inflated models, a zero-inflation assessment will also be provided. Warnings or notices will be provided based on the output within these charts."), 
                               p("Step 6: You can check the effect of outlying, influential, or leverage points in the 'Outliers' tab. Many models exhibit some influential points and researchers should ensure that the results of their model hold when using a robust regression model."), #NOTE: May need to change this depending on what we want to show
                               p("Step 7: If necessary, repeat the above steps with an alternative model based on the recommendations provided in the 'Assumptions' tab."),
                               p("Step 8: If necessary, clean the data of outliers or other influential points visualized in the 'Outliers' tab that can be removed."),
                               p("Step 9: A table will be provided detailing the results of a linear model test or a Vuong test, depending on the models being compared."),
                               p("Step 10: The resulting model and interpretation of key values can be found in the 'Interpretation' tab"),
                               p("Step 11 (Optional): If your model has an interaction, the appropriate analyses will be reported in the 'Interaction' tab.")),
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
                                           h1("Example 1: Poisson to Zero-Inflated", align = "center"), #Note, this needs revision. Please revisit when I(Age^2) works properly.
                                           
                                           tags$div(class = "paragraph", tags$hr(),
                                                    p("Within this app, there is a provided dataset that contains a representative sample of n=47 Ache hunters by Micmillan et (2001). The researchers assessed the hunters' ages, the number of kills by each hunter, and the duration of each trek made by the hunters."),
                                                    p("The goal of the researchers was to determine whether or not there was the dependency of skill on the age or experience of the hunter. If this were the case, it would suggest that there is an age at which a hunter's physical performance and age are both at their optimal for hunting."),
                                           ),
                                           
                                           tags$div(class = "paragraph", 
                                                    p("The researchers noted prior studies that assess the effects of age on the ability to forage, there was a notable gap in evaluating strength and skill as predictors of hunting ability."),
                                                    p("More specifically, the researchers hypothesize that proficiency in hunting is associated with learning prior to and after a hunter matures physically, while accounting for hunting duration."),
                                                    tags$hr(),
                                                    wellPanel(strong("Kills ~ Age + Days"))
                                           ),
                                           
                                           tags$div(class = "paragraph", 
                                                    p("Upon pressing 'Compute Model Output', several plots and tables will be created. Upon processing, the app will show the 'Assumptions' tab, which presents a table, a checklist, and two plots."),
                                                    p("For the Poisson model, we can note a few issues. Starting with the table, our Age parameter does not appear to be statistically discernible, with a p-value of 0.116. Secondly, the data appears to have excess zeroes in our RQR Plot. Lastly, according to the Dispersion Ratio of 3.7485, the observed variace is about 3.75 times more than the estimate."),
                                                    tags$hr(),
                                                    tags$img(src = "/images/ache_poisson_asmp_table.png", height = "125px", width = "550px"),
                                                    tags$img(src = "/images/ache_poisson_asmp_graph.png", height = "367px", width = "525px")
                                           ),
                                           
                                           tags$div(class = "paragraph",  tags$hr(),
                                                    p("Furthermore, by navigating to the 'Outliers' tab, the graphs show that there are several points that may be influencing the regression overall."),
                                                    tags$hr(),
                                                    tags$img(src = "/images/ache_poisson_outliers_graph.png", height = "728px", width = "658px"),
                                                    tags$hr(),
                                                    p("Lastly, within the 'Plots' tab, there is a graph assessing zero inflation within the data, as shown below"),
                                                    tags$img(src = "/images/ache_poisson_zinf_graph.png", height = "164px", width = "453px"),
                                                    p("If the data has little to no zero-inflation, then the vertical red line should appear roughly centered in the histogram. However, in the case of the Ache Monkey Hunter data, this is not the case, indicating that the data likely suffers from zero inflation."),
                                                    p("Based on the earlier assumptions, it is a good idea to fit a Zero Inflated model instead. For the purposes of this example, the selected model will be changed to a 'Zero-Inflated Poisson' model.")
                                           ),
                                           # HTML('<center><img src="ex1-datasummary.png"></center>'),
                                           
                                           tags$div(class = "paragraph", tags$hr(),
                                                    p("First, the 'Zero-Inflated Poisson' model is selected from the dropdown menu and the desired equation equation is inputted."),
                                                    tags$hr(),
                                                    wellPanel(strong("Kills ~ Age + Days")),
                                                    tags$hr(),
                                                    p("Then, the model is recomputed and the assumptions are re-evaluated when the 'compute model output' button is pressed. The resulting table and graphs slightly different than that of the Poisson model, as shown below."),
                                                    tags$hr(),
                                           ),
                                           
                                           tags$div(class = "paragraph",
                                                    tags$img(src = "/images/ache_zip_asmp_table.png", height = "300px", width = "660px"),
                                                    p("In the zero-inflated poisson model, there is now a second intercept value that serves as the logistic regression. This helps us account for the 'zero inflation', for lack of a better term. However, we can see that Age does not seem to be an important parameter."),
                                                    tags$hr(),
                                                    tags$img(src = "/images/ache_zip_asmp_graph.png", height = "600px", width = "700px"),
                                                    p("Our dataset still has many fitted values around 0, but the residuals are generally OK. Thus, the next tab to check is the ANOVA tab."),
                                                    tags$hr(),
                                                    tags$img(src = "/images/ache_zip_anova_output.png", height = "300px", width = "570px"),
                                                    p("That said, in our interpretations, our output table shows that Age is still not considered statistically discernible, whereas days is. As such, in this model, Days is a discernible indicator for number of kills."),
                                                    tags$hr(),
                                                    tags$img(src = "/images/ache_poisson_error_check.gif", height = "300px", width = "300px")
                                           )
                                           
                                           
                                           
                                  ),
                                  tabPanel("Example 2",
                                           h1("Example 2: Interactions in Negative Binomial", align = "center"),
                                           tags$div(class = "paragraph", tags$hr(),
                                                    p("Within this app, there is data included about blood samples obtained from healthy donors of both sexes, aged 25-45."),
                                                    p("They infected the cells ohsr f primary (human donor’s blood) or THP1 (purchased cell lines) monocytes (n = 87 and n = 112, respectively) and macrophages (n = 93 and n = 170, respectively) and counted the number of viral genomes in the nuclei."),
                                                    p("The goal is to determine if the virus is more prevalent in the nucleus of monocytes or machophages to explain HCMV's dormancy in monocytes and activity in macrophages."),
                                                    tags$hr(),
                                                    wellPanel(strong("viruses.within.nucleus ~ SampleType * CellType"))
                                                    
                                           ),
                                           tags$div(class = 'paragraph', tags$hr(),
                                                    p("By including the '*' in our regression equation, the model is instructed to construct both individual and interaction terms. As such, the true regression equation would be as follows:"),
                                                    wellPanel(strong("virses.within.nucleus ~ SampleType + CellType + SampleType:CellType")),
                                                    tags$hr()
                                                    ),
                                           tags$div(class = "paragraph",
                                                    p("From here, the plots can now be analyzed. The first plot is the RQR and QQ plots in the 'Assumptions' tab."),
                                                    tags$img(src = "/images/kitsberg_nb_asmp_graph.png", height = "321px", width = "542px"),
                                                    p("The RQR doesn't indicate any uneven or imbalanced distribution of our residuals. Furthermore, the QQ plot indicates that a Negative Binomial is fitting, as the dispersion ratio suggests that the observed and estimated variances are very near one another."),
                                                    p("Meanwhile, our table suggests _______________ (NOTE: ADD TABLE ONCE COLLISION IS RESOLVED)"),
                                                    p("On the 'Plots' tab, unlike 'Example 1', there is little to no zero inflation in this data. As such, it is permissible to proceed without a zero-inflated model."),
                                                    tags$img(src = "/images/kitsberg_nb_zinf_graph.png", height = "332px", width = "551px"),
                                                    p("Thus, the next step is to tune the interaction analysis. In the sidebar panel, there is a new section that appeared after 'Compute Model Output' was pressed. For this example, the selected interaction is 'SampleType:CellType' and the selected moderator is 'SampleType'."),
                                                    p("From here, the 'Interaction' tab becomes visible. By navigating to this tab, various information about the interaction term and its significance can be displayed."),
                                                    tags$hr()
                                                    
                                                    ),
                                           tags$div(class = "paragraph",
                                                    p("First, a visualization by group is displayed. For this dataset, the two categories within 'SampleType' are separated, as shown below."),
                                                    tags$img(src = "/images/kitsberg_nb_interaction_graph.png", height = "390px", width="558px"),
                                                    p("Based on this graph, both cell types seem to be fairly even, but mac samples tend to have higher predicted viruses within the nucleus than those of mono samples. By scrolling down, more information is provided about the estimated marginal means."),
                                                    tags$img(src = "/images/kitsberg_nb_interaction_emmeans_table.png", height = "337px", width = "583px"),
                                                    tags$img(src = "/images/kitsberg_nb_interaction_emmeans_interpretation.png", height = "277px", width = "565px"),
                                                    p("Both the values and the significance interpration is presented as part of the interaction tab. From this, the differences between the levels or categories can be numerically visualized."),
                                                    p("For this app, it should be noted that values are calculated at average levels for the other non-interaction terms in the model. While this can be remedied and calculated at specific levels in R, it is not supported in this app at this time."),
                                                    p("Further down, there is also contrasted estimated marginal means with a Tukey Adjustment"),
                                                    tags$img(src = "/images/kitsberg_nb_interaction_contrast_table.png", height = "505px", width = "571px"),
                                                    tags$img(src = "/images/kitsberg_nb_interaction_contrast_interpretation.png", height = "266px", width = "575px"),
                                                    p("From these, it can be determined which groups have a statistically discernible difference from one another, and which groups are relatively similar to one another as well. In this instance, the report shows that mac primary and mono primary are different from one another, while mono primary and mono THP1 are not."),
                                                    p("By analyzing these, it is possible to group the pairs; the resulting groups would be divided by Mac and Mono in this instance."),
                                                    tags$hr()
                                                    ),
                                           tags$div(class = "paragraph",
                                                    p("Lastly, turning to the 'ANOVA' tab, the table shows the p-values, deviance, and partial McFadden's R. Additionally, there are more interpretations below."),
                                                    tags$img(src = "/images/kitsberg_nb_anova_table.png", height = "360px",  width = "686px"),
                                                    p("The table and interpretations indicate that the only statistically discernible term is the Cell Type, which lines up with the findings from the Interaction tab.")
                                                    )
          
                                          
                                  ),
                                  tabPanel("Example 3",
                                           h1("Example 3: Tweedie Model Data"),
                                           tags$div(class = "paragraph", tags$hr(),
                                                    p("The Tweedie Model is a type of Generalized Linear Model (GLM) which specializes in data for non-negative data with a mix of zeroes and positive, continuous values."),
                                                    p("In order to use the Tweedie model, the data should have a non-negative response variable (rainfall, insurance claims, energy usage, flood height, etc.), many zero values, and the variance should follow a power relationship with the mean. Additionally, the data should have mostly small values and occasional large values."),
                                                    p("Unlike the Zero-Inflated Poisson and Zero-Inflated Negative Binomial models, the zero values are not treated as part of a separate distribution, whereas the Zero-Inflated version draws them from a Logistic model.")
                                                    )
                                           
                                           
                                           )
                      ) # acts as a contained for multiple tabPanel()
             ),
             tabPanel("Dataset & Model",
                      sidebarPanel( # Handels Data and settings
                        #h3("Model Creation"),
                        fileInput("file_upload", "Upload a File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        actionButton("sample", "Sample Data"),
                        hidden(div(id='choose_sample', #div is a box
                                   selectInput("sample_data_choice","Sample Data:",
                                               choices = c("Camera Data", "Palmer Penguins", "Kitsberg et al. Nucleus" ,"U.S. News College Data", "Ache Monkey"),
                                               selected = "U.S. News College Data"))),
                        tags$hr(), #shaw shaw
                        div(id='choose_model',
                            selectInput("model_choice","Model:",
                                        choices = c("Poisson", "Negative Binomial", "Quasi-Poisson", "Zero-Inflated Poisson", "Zero Inflated Negative Binomial", "Tweedie"),
                                        selected = "Poisson")),
                        hidden(selectizeInput("select_factors",
                                              "Specify Categorical Variables in the Data:",
                                              choices = NULL,
                                              selected = NULL,
                                              multiple = TRUE)),
                        
                        textInput("equation", "Enter your desired regression equation:", value = ""),
                        #bsTooltip(id = "equation", title = "This is an input",
                        #     placement = "right", trigger = "hover"),
                        checkboxInput("scalevars", "Scale all variables (standardize)", FALSE), #need to see if user wabrs to scale and do so if yes need to implement
                        numericInput("alpha", "Significance level (\u03B1): ", value = 0.05, step = 0.001, min = 0, max = 1),     # alpha level need to adjust if user wants to
                        
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
                        div(class = "text-center", actionButton("DoCompute", "Compute Model Output")), #DoCompute id for button
                        
                        
                        
                        
                        
                      ),
                      mainPanel( # Presnets results #value = "---" is internal ID for tab
                        tabsetPanel(id = "workPanel",
                                    tabPanel("Data Preview", br(), value="data", #Value allows us to pick whats data
                                             shinycssloaders::withSpinner(DT::dataTableOutput("preview.data"))),
                                    tabPanel("Data Summary", value="summary",
                                             fluidPage(
                                               h1("Pairwise Plots", align = "center"), br(),
                                               fluidRow(column(12, actionButton("code_ggpairsplot", "R code", icon("code"))),
                                                        fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("ggpairs_plot")))),
                                                        fluidRow(
                                                          column(width=2, textInput("ggpairs_plot_height", "Enter Height", value=7)),
                                                          column(width=2, textInput("ggpairs_plot_width", "Enter Width", value=7)),
                                                          column(width=2, selectInput("ggpairs_plot_units", "Units", choices = c("in", "cm"))),
                                                          column(width=2, selectInput("ggpairs_plot_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                          column(width=2, downloadButton('downloadggpairsPlot'),style = "margin-top: 25px;"), #
                                                          tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                        ),
                                                        tags$hr(),
                                                        br(),
                                                        
                                                        h1("Correlation Matrix", align = "center"), br(),
                                                        fluidRow(column(12, actionButton("code_corrmat", "R code", icon("code")), downloadButton('downloadcormatLatex',label="LaTeX"))),br(),
                                                        fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("ggpairs_summary")))), "*<0.05; **<0.01; ***<0.001", br()
                                                        #verbatimTextOutput("summary")),
                                               ),
                                               tags$hr())
                                    ),
                                    tabPanel("Assumptions", value="assumptions", #NOTE: Haven't gotten a chance to fix this yet. Trying to find viable solutions that aren't codebreaking, but the best course of action is to somehow make a copy of the existing table from the Plots tab and shift it over here somehow.
                                             fluidPage(h1("Assumptions for Regression"),
                                                       fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("modsumTab2")))),br(), #This is supposed to load stuff but isn't working
                                                       
                                                       fluidRow(
                                                         
                                                         column(4, style = "background-color:#ecf0f1;", tags$hr(), br(),
                                                                h4("Make sure that you satisfy all regression assumptions:"), br(),
                                                                # Assumption 1
                                                                checkboxInput("asmp_1", HTML("The sample(s) is representative, and observations are independent."), FALSE),
                                                                hidden(div(id='asmp_1note', htmlOutput('asmp_1'))),
                                                                # Assumption 2
                                                                checkboxInput("asmp_2", "Little to no (multi)collinearity.", FALSE),
                                                                hidden(div(id='asmp_2note', htmlOutput('asmp_2'), style="margin-bottom:10px;margin-top:10px")),
                                                                hidden(div(id='log_button', actionButton("logtransform", "log transform"), style="margin-bottom:10px;margin-top:10px")),
                                                                hidden(div(id='lp1_button', actionButton("logplus1transform", "log(y+1) transform"), style="margin-bottom:10px;margin-top:10px")),
                                                                hidden(div(id='ihs_button', actionButton("ihstransform", "inverse hyperbolic sine transform"), style="margin-bottom:10px;margin-top:10px")),
                                                                # Assumption 3
                                                                checkboxInput("asmp_3", "For each predictor, about 10-20 events per observation."),
                                                                hidden(div(id='asmp_3note', htmlOutput('asmp_3'))),
                                                                # Assumption 4
                                                                checkboxInput("asmp_4", "The relationship between the predictors and the log-mean is linear."),
                                                                hidden(div(id='asmp_4note', htmlOutput('asmp_4'))),
                                                                
                                                                #Assumption 5A: Non-ZI model - excess 0
                                                                hidden(checkboxInput("asmp_5A", HTML("The model does not have excess zero values"), FALSE)), #NOTE: MAY NEED TO ADD PLOT FOR THIS
                                                                hidden(div(id='asmp_5Anote', htmlOutput("asmp_5A"))),
                                                                #Assumption 5B: ZI Model Structural 0s
                                                                hidden(checkboxInput("asmp_5B", HTML("The model has a mixed process for generating zeroes and counts"), FALSE)), #NOTE: MAY NEED TO ADD PLOT FOR THIS
                                                                hidden(div(id='asmp_5Bnote', htmlOutput("asmp_5B"))),
                                                                
                                                                #Assumption 6
                                                                checkboxInput("asmp_6", "The data is well fit by the chosen model", FALSE),
                                                                hidden(div(id='asmp_6note', htmlOutput("asmp_6"))),
                                                                
                                                                br(),
                                                                actionButton("check_asmp", strong("Check Assumptions")), br(), br(),    # Button to check all assumptions
                                                                # Hidden divs are displayed only for two-sample independent test
                                                                hidden(div(id='asmp_note', htmlOutput('asmp_note'))),
                                                                
                                                                
                                                         ),
                                                         
                                                         column(8, 
                                                                fluidRow(column(12, actionButton("code_RQR", "R code", icon("code")))),
                                                                
                                                                fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("RQR_plot2")))), # (NOTE PLOT) 7 This is how you render  plot in UI note we call it RQR_plot the same name we passed to output$RQR_plot in server
                                                                fluidRow(
                                                                  column(width=2, textInput("RQR_plot_height", "Enter Height", value=7)),
                                                                  column(width=2, textInput("RQR_plot_width", "Enter Width", value=7)),
                                                                  column(width=2, selectInput("RQR_plot_units", "Units", choices = c("in", "cm"))),
                                                                  column(width=2, selectInput("RQR_plot_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                                  column(width=2, downloadButton('downloadRQRPlot'),style = "margin-top: 25px;"), #
                                                                  tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                                )
                                                                
                                                         )
                                                       )
                                                       
                                             )
                                    ),
                                    tabPanel("Outliers", value = "checks",
                                             fluidPage(h1("Outliers, Influential, and Leverage Points"),
                                                       fluidRow(
                                                         column(4, style = "background-color:#ecf0f1;", tags$hr(), br(),
                                                                h4("Make sure to evaluate whether there are outliers or influential points:"), br(),
                                                                # Assumption 1
                                                                checkboxInput("check_1", HTML("Few/no observations with large leverage values."), FALSE),
                                                                # Assumption 2
                                                                checkboxInput("check_2", "Few/no observations with large Cook's distance values.", FALSE),
                                                                # Assumption 3
                                                                checkboxInput("check_3", "Few/no observations with DFFITS with large magnitude.", FALSE),
                                                                # Assumption 4
                                                                checkboxInput("check_4", "Few/no observations with outlying residuals."),
                                                                br(),
                                                                actionButton("check_obs", strong("Check Observations")), br(), br(),    # Button to check all assumptions
                                                                hidden(div(id='check_note', htmlOutput('check_note')))
                                                         ),
                                                         column(8,
                                                                fluidRow(actionButton("code_check", "R code", icon("code")), style = "margin-left: 20px;"),
                                                                br(),   # Show code using shinymeta pkg
                                                                fluidRow(shinycssloaders::withSpinner(plotOutput("check_plot")), style = "margin-left: 20px;"), 
                                                                fluidRow(
                                                                  column(width=2, textInput("check_plot_height", "Enter Height", value=7)),
                                                                  column(width=2, textInput("check_plot_width", "Enter Width", value=7)),
                                                                  column(width=2, selectInput("check_plot_units", "Units", choices = c("in", "cm"))),
                                                                  column(width=2, selectInput("check_plot_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                                  column(width=2, downloadButton('downloadcheckPlot'),style = "margin-top: 25px;"), #
                                                                  tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                                )
                                                         )
                                                       )
                                             )
                                    ),
                                    tabPanel("Plots", value = "plot",
                                             fluidPage(tags$hr(),
                                                       h1("RQR Plot for Pearson"),
                                                       h3("Visualization"),
                                                       fluidRow(column(12, actionButton("code_RQR", "R code", icon("code")))),
                                                       br(),
                                                       fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("RQR_plot")))), # (NOTE PLOT) 7 This is how you render  plot in UI note we call it RQR_plot the same name we passed to output$RQR_plot in server
                                                       fluidRow(
                                                         column(width=2, textInput("RQR_plot_height", "Enter Height", value=7)),
                                                         column(width=2, textInput("RQR_plot_width", "Enter Width", value=7)),
                                                         column(width=2, selectInput("RQR_plot_units", "Units", choices = c("in", "cm"))),
                                                         column(width=2, selectInput("RQR_plot_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                         column(width=2, downloadButton('downloadRQRPlot'),style = "margin-top: 25px;"), #
                                                         tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                       ),
                                                       tags$hr(),
                                                       fluidRow(column(12, actionButton("code_Pearson_Residual", "R code", icon("code")))),
                                                       br(),
                                                       fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("Pearson_Residual_Plot")))), 
                                                       fluidRow(
                                                         column(width=2, textInput("Pearson_Residual_Plot_height", "Enter Height", value=7)),
                                                         column(width=2, textInput("Pearson_Residual_Plot_width", "Enter Width", value=7)),
                                                         column(width=2, selectInput("Pearson_Residual_Plot_units", "Units", choices = c("in", "cm"))),
                                                         column(width=2, selectInput("Pearson_Residual_Plot_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                         column(width=2, downloadButton('downloadPearson_Residual_Plot'),style = "margin-top: 25px;"), #
                                                         tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                       ),
                                                       tags$hr(),
                                                       fluidRow(column(12, actionButton("code_ZeroInflated", "R code", icon("code")))),
                                                       br(),
                                                       fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("ZeroInflated_Plot")))), 
                                                       fluidRow(
                                                         column(width=2, textInput("ZeroInflated_Plot_height", "Enter Height", value=7)),
                                                         column(width=2, textInput("ZeroInflated_Plot_width", "Enter Width", value=7)),
                                                         column(width=2, selectInput("ZeroInflated_Plot_units", "Units", choices = c("in", "cm"))),
                                                         column(width=2, selectInput("ZeroInflated_Plot_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                         column(width=2, downloadButton('downloadZeroInflated_Plot'),style = "margin-top: 25px;"), #
                                                         tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                       ),
                                                       
                                             )
                                             
                                             
                                             
                                    ),
                                    tabPanel("ANOVA", value="anova",
                                             fluidPage(h1("ANOVA Table"), 
                                                       fluidRow(column(12, actionButton("code_anova", "R code", icon("code")), downloadButton('downloadanovaLatex',label="LaTeX"))),br(),
                                                       fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("anovaTab")))),br(),
                                                       h1("Interpretation"),
                                                       htmlOutput("anovainterp"),
                                                       tags$hr(),
                                                       hidden(div(id='anova_fctcomp',
                                                                  h1("Factor Comparisions"),
                                                                  fluidRow(column(12, actionButton("code_anova_fctcomp", "R code", icon("code")))),
                                                                  br(),
                                                                  fluidRow(shinycssloaders::withSpinner(plotOutput("anova_fctcomp_plot")), style = "margin-left: 20px;"), 
                                                                  br(),
                                                                  fluidRow(
                                                                    column(width=2, textInput("anova_fctcomp_plot_height", "Enter Height", value=7)),
                                                                    column(width=2, textInput("anova_fctcomp_plot_width", "Enter Width", value=7)),
                                                                    column(width=2, selectInput("anova_fctcomp_plot_units", "Units", choices = c("in", "cm"))),
                                                                    column(width=2, selectInput("anova_fctcomp_plot_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                                    column(width=2, downloadButton('downloadanovafactorcomparePlot'),style = "margin-top: 25px;"), #
                                                                    tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                                  ),
                                                                  fluidRow(column(12, downloadButton('downloadanovafctcompLatex',label="LaTeX"))),br(),
                                                                  fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("anova_fctcompTab")))),br(),
                                                                  h1("Interpretation"),
                                                                  htmlOutput("anova_fctcompinterp"),
                                                                  tags$hr()
                                                       ))
                                                       
                                                       
                                                       
                                                       
                                                       
                                             )
                                    ),
                                    tabPanel("Interpretation", value="interpretation", #NOTE- there is something not loading here. Not sure what was supposed to be here.
                                             fluidPage(h1("Count Model"), 
                                                       fluidRow(column(12, actionButton("code_modsum", "R code", icon("code")), downloadButton('downloadmodsumLatex',label="LaTeX"))),br(),
                                                       fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("modsumTab")))),br(), #found the thingy - this modsumTab is being problematic.
                                                       h1("Interpretation"),
                                                       htmlOutput("modelinterp"),
                                                       tags$head(tags$style("#clickGene{color:red; font-size:12px; font-style:italic;}")),
                                                       br(),
                                                       tags$hr(),
                                                       hidden(div(id='marginaleffectsdiv',
                                                                  h1("Marginal Effects"), 
                                                                  fluidRow(column(12, actionButton("code_margins", "R code", icon("code")), downloadButton('downloadmarginsLatex',label="LaTeX"))),br(),
                                                                  fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("marginsTab")))),br(),
                                                                  h1("Interpretation"),
                                                                  htmlOutput("marginsinterp"),br(),
                                                                  tags$hr()
                                                       ))
                                             )
                                    ),
                                    tabPanel("Interaction", value="interaction",
                                             fluidPage(tags$hr(),
                                                       h1("Interaction Analysis"),
                                                       h3("Visualization"),
                                                       fluidRow(column(12, actionButton("code_ggemmeans", "R code", icon("code")))),
                                                       fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("ggemmeans_plot")))),
                                                       fluidRow(
                                                         column(width=2, textInput("ggemmeans_plot_height", "Enter Height", value=7)),
                                                         column(width=2, textInput("ggemmeans_plot_width", "Enter Width", value=7)),
                                                         column(width=2, selectInput("ggemmeans_plot_units", "Units", choices = c("in", "cm"))),
                                                         column(width=2, selectInput("ggemmeans_plot_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                         column(width=2, downloadButton('downloadggemmeansPlot'),style = "margin-top: 25px;"), #
                                                         tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                       ),
                                                       tags$hr(),
                                                       
                                                       h3("Estimated Marginal Means"),
                                                       fluidRow(column(12, actionButton("code_interaction_emmeanstab", "R code", icon("code")), downloadButton('download_interaction_emmeansLatex',label="LaTeX"))),br(),
                                                       fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("interaction_emmeans_tab")))),
                                                       h3("Interpretation"),
                                                       htmlOutput("interaction_emmeans_interp"),br(),
                                                       
                                                       h3("Contrasts of Marginal Means"),
                                                       fluidRow(column(12, actionButton("code_interaction_emmeans_contrast", "R code", icon("code")), downloadButton('download_interaction_emmeanscontrastLatex',label="LaTeX"))),br(),
                                                       fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("interaction_emmeanscontrast_tab")))), br(),
                                                       h3("Interpretation"),
                                                       htmlOutput("interaction_emmeanscontrast_interp"), 
                                                       
                                                       ##############################################################################
                                                       # Should only be available for everything but factor/factor and numeric/factor with factor as the moderator
                                                       ##############################################################################
                                                       hidden(div(id='emtrendsdiv',
                                                                  tags$hr(),
                                                                  h3("Estimated Marginal Effects"),
                                                                  fluidRow(column(12, actionButton("code_interaction_emtrendstab", "R code", icon("code")), downloadButton('download_interaction_emtrendsLatex',label="LaTeX"))),br(),
                                                                  fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("interaction_emtrends_tab")))),
                                                                  h3("Interpretation"),
                                                                  htmlOutput("interaction_emtrends_interp"),br(),
                                                       )),
                                                       
                                                       ##############################################################################
                                                       # Should only be available for everything but factor/factor and numeric/factor with factor as the moderator
                                                       ##############################################################################
                                                       hidden(div(id='emtrendscontrastdiv',
                                                                  tags$hr(),
                                                                  h3("Contrast of Marginal Effects"),
                                                                  fluidRow(column(12, actionButton("code_interaction_emtrendscontrasttab", "R code", icon("code")), downloadButton('download_interaction_emtrendscontrastLatex',label="LaTeX"))),br(),
                                                                  fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("interaction_emtrendscontrast_tab")))),
                                                                  h3("Interpretation"),
                                                                  htmlOutput("interaction_emtrendscontrast_interp"),br(),
                                                       )),
                                                       
                                                       ##############################################################################
                                                       # Should only be available for numeric*numeric interactions
                                                       ##############################################################################
                                                       hidden(div(id='jndiv',
                                                                  tags$hr(),
                                                                  h3("Johnson Neyman"),
                                                                  fluidRow(column(12, actionButton("code_jn", "R code", icon("code")))),
                                                                  fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("jn_plot")))),
                                                                  fluidRow(
                                                                    column(width=2, textInput("jn_plot_height", "Enter Height", value=7)),
                                                                    column(width=2, textInput("jn_plot_width", "Enter Width", value=7)),
                                                                    column(width=2, selectInput("jn_plot_units", "Units", choices = c("in", "cm"))),
                                                                    column(width=2, selectInput("jn_plot_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                                    column(width=2, downloadButton('downloadjnPlot'),style = "margin-top: 25px;"), #
                                                                    tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                                  ),
                                                                  h3("Interpretation"),
                                                                  htmlOutput("jn_int"),br()
                                                       )),
                                                       ##############################################################################
                                                       tags$hr()
                                             )
                                    )
                                    
                        )
                        
                      )
                      
             ),
             tabPanel("Refrences"
                      
                      
             )
             
             
             
             
             
  ) # End of navbarPage
  
  
)
