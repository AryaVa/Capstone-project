#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
})

# Define UI for application 
shinyUI(fluidPage(

    # Application title
    titlePanel("Prediction App"),

    # Sidebar with a text input
    sidebarLayout(
        sidebarPanel(
          textInput("user_input", h2("Please enter a phrase or a word:"), 
                    value = " ")
        ),

        # Show the output
        mainPanel(
          tabsetPanel(
            tabPanel("Predict",
                     h2("Predicted Next Word:"),
                     h3(em(span(textOutput("ngram_output"), style="color:red")))),
            tabPanel("About", includeMarkdown("aboutfinal.md"))
            )
          )
          
        )
    )
)
