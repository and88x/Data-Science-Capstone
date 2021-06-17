#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

quadgram  <- readRDS("./databases/quadgram.Rda")
trigram   <- readRDS("./databases/trigram.Rda")
bigram    <- readRDS("./databases/bigram.Rda")



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    source('./predictor.R')

    output$wordPred <- renderText({
        
        ngrams(input$userText,quadgram,trigram,bigram)
        
    })
    
    observeEvent(input$do, {
        x <- input$userText
        updateTextInput(session, "userText", value = paste(x, ngrams(input$userText,quadgram,trigram,bigram)))
    })
    
    observeEvent(input$clean, {
        updateTextInput(session, "userText", value = "")
    })
    
    # this is working
    output$n = renderText(
        paste0(
            "The predicted word is:", 
            collapse = "")
    ) 


})
