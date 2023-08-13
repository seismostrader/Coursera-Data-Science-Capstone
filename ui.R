# load necessary libraries
library(shiny)
library(shinythemes)

# Define UI for application that predicts next word from input text and visualizes stupid backoff scores
shinyUI(fluidPage(theme = shinytheme("cerulean"),
        
        # application title
        titlePanel("Text Prediction Using the Stupid Backoff Method"),
        
        # sidebar with all included widgets
        sidebarPanel(
            p(
                "This app suggests up to five possible next words, given some input text. These words are predicted using the stupid backoff algorithm."
            ),
            
            p(
                "The user can choose to include or omit stopwords (commonly-used words such as 'a,' 'the,' 'but,' etc.). They can also set the 'alpha' value, which impacts how much a predicted word's score will drop per iteration of the algorithm."
            ),
            
            p(
                "A plot showing each predicted word and its corresponding stupid backoff score will be displayed, along with the list of predicted words."
            ),
        
            # add radio buttons to select whether stopwords will be included or omitted
            # NOTE: the function input asks if stopwords should be removed!
            radioButtons("include_stopwords", "Should stopwords be included?",
                        choices = list(
                            "yes" = FALSE,
                            "no" = TRUE
                        )),
        
            # add slider allowing user to select value of alpha
            sliderInput("alpha_val", "alpha:", 0, 1.0, 0.4),
        
            # add a button to start running the prediction algorithm
            submitButton("Predict"),
            
            # add some text warning about long loading times at the beginning of the simulation
            p(
                " "
            ),
            
            p(
                "*Please note that the first time running this app may take several seconds due to loading n-grams."
            )
        ),
        
        # display results in the main panel
        mainPanel(
            
            # add a text input box for the user to add some text
            textInput("input_text", "Enter some input text:", "text here"),
            
            # add title for predicted word list
            h3("Top Predicted Words:"),
            
            # list top predicted words
            verbatimTextOutput("list.predictions"),
            
            # add title for SB score plot
            h3("Stupid Backoff Scores:"),
            
            # plot SB scores of top predicted words
            plotOutput("SBplot")
        )
))