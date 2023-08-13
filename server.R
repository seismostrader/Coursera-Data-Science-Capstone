# load libraries
library(shiny)
library(ggplot2)

# source necessary functions
source("text-predict-funcs.R")

# read in profanity
profanity <- read.profanity()

# text prediction
shinyServer(function(input, output) {
    
    # tokenize input text
    valid.tokens <- reactive({
        tokens <- generate.tokens.from.input(input$input_text, profanity, highest.order.n = 7, 
                                             remove.stopwords = input$include_stopwords, remove.lastword = FALSE)
        
        tokens
    })
    
    # if input text was valid, generate dataframe with top five (maximum) predicted words and corresponding stupid backoff scores
    predicted <- reactive({
        prediction <- stupid.backoff(input$input_text, profanity, input$alpha_val, iteration = 0, 
                                     remove.stopwords = input$include_stopwords, remove.lastword = FALSE)
        
        prediction
    })
    
    output$list.predictions <- reactive({
        # get number of predicted words
        num.predictions <- nrow(predicted())
        
        # sort predictions by descending SB score
        predictions.sorted <- predicted()[order(predicted()[, "score"], decreasing = TRUE), ]
        
        # list top word predictions
        list.predictions <- ""
        for (i in 1:num.predictions) {
            predicted.word <- predictions.sorted[i, "prediction"]
            list.predictions <- paste(list.predictions, paste0(i, ". ", predicted.word), sep = "\n")
        }
        
        list.predictions
    })
    
    output$SBplot <- renderPlot({
        
        # sort predictions by descending SB score
        predictions.sorted <- predicted()[order(predicted()[, "score"], decreasing = TRUE), ]
        
        # generate plot
        SBplot <- ggplot(data = predictions.sorted, aes(x = reorder(prediction, score), y = score)) + geom_bar(stat = 'identity', fill = "skyblue2", color = "black") + xlab("predicted words") + ylab("SB score") + coord_flip() + theme(axis.text = element_text(size = 12), axis.title=element_text(size=14))
        
        SBplot
    })

})