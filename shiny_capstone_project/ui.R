#library(shiny)

shinyUI(fluidPage(
  titlePanel("Data Science Specialization Capstone Project"),

  #HTML("This application ", a("Link", "www.google.com")),
  
  HTML("<span style=\"color:black; font-style:normal\"> Key some text into the Input box. Some examples are: <b> \"I want to take a\", \"On a positive\", \"I don't\", , \"She hasn't\", or \"At the end\" </b> <br /> A prediction for the next word will then appear in the right panel. <br /> You can control the maximum number of predictions that appear, by using the slider. </span>"),
  br(), br(),
  
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "txt_input", label = "Input", value = "I"),
      #actionButton(inputId = "btn_submit", label = "Predict!"),
      sliderInput(inputId = "sld_num_predictions", label = "Max. Number of Predictions", min = 1, max = 10, value = 3, round = F),
      br()         
      
    ),
    
    mainPanel(
        h3("Prediction"),        
        strong(textOutput("txt_prediction")), # The predicted text.
        
        tags$head(tags$style("#txt_prediction {color: blue;
                                 font-size: 24px;
                                 }"
        )),
        tags$head(tags$style("#txt_alternatives {color: green;
                                 font-size: 16px;
                                 }"
        )),  # This adds the style to the <HEAD> tag for the page.
        
        
        h3("Alternatives"),
        #helpText("<Alternative predictions go here>"),
        strong(textOutput("txt_alternatives")), # The alternative text.
        br(), br(),
        p("For more details about this Shinyapp, please click", a("here", href = "http://rpubs.com/amosang/capstone_final_report", target = "_blank"), "."),
        br()
    )
  )
))

###################################
# References
# 1. http://www.showmeshiny.com/
# 
# 


