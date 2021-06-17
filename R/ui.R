#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application 
shinyUI(fluidPage(

    # Application title
    titlePanel("Johns Hopkins Data Science Specialization Capstone Project"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h2("Text Predictor"),
            h5("by Fernando Garcia"),
            br(),
            # h4("Please, write your initial text below:"),
            textInput(inputId = "userText", 
                      label = "Write your initial text here", 
                      value = "Hello, my name" # 
            ),
            actionButton("clean", "Clean"),
        ), # End of sideBar

        # Show a plot of the generated distribution
        mainPanel(
            h4(textOutput("n")),
            #h3(textOutput("wordPred"), align="left"),
            actionButton("do", textOutput("wordPred")),
            br(),
            hr()
        ) # End of mainPanel
    )
))
