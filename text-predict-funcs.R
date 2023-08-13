#######################################################################################################
# Functions for Predictive Text Shiny App
#
# Coursera Data Science Specialization Capstone Project
# Author: Anne Strader
#######################################################################################################

# load necessary R packages
library(quanteda)
library(quanteda.textstats)
library(dplyr)
library(stringi)

# load input data
# load n-grams with stopwords
# unigrams
freq.1grams.withstop <- read.csv(paste0(getwd(), "/1gram.csv"))
# bigrams
freq.2grams.withstop <- read.csv(paste0(getwd(), "/2gram.csv"))
# trigrams
freq.3grams.withstop <- read.csv(paste0(getwd(), "/3gram.csv"))
# quadrigrams
freq.4grams.withstop <- read.csv(paste0(getwd(), "/4gram.csv"))
# 5-grams
freq.5grams.withstop <- read.csv(paste0(getwd(), "/5gram.csv"))
# 6-grams
freq.6grams.withstop <- read.csv(paste0(getwd(), "/6gram.csv"))
# 7-grams
freq.7grams.withstop <- read.csv(paste0(getwd(), "/7gram.csv"))

# load n-grams without stopwords
# unigrams
freq.1grams.nostop <- read.csv(paste0(getwd(), "/1gram_nostop.csv"))
# bigrams
freq.2grams.nostop <- read.csv(paste0(getwd(), "/2gram_nostop.csv"))
# trigrams
freq.3grams.nostop <- read.csv(paste0(getwd(), "/3gram_nostop.csv"))
# quadrigrams
freq.4grams.nostop <- read.csv(paste0(getwd(), "/4gram_nostop.csv"))
# 5-grams
freq.5grams.nostop <- read.csv(paste0(getwd(), "/5gram_nostop.csv"))
# 6-grams
freq.6grams.nostop <- read.csv(paste0(getwd(), "/6gram_nostop.csv"))
# 7-grams
freq.7grams.nostop <- read.csv(paste0(getwd(), "/7gram_nostop.csv"))

# reads list of profane words for use in filtering profanity from corpus

read.profanity <- function() {
    # INPUT:
    # nothing
    
    # OUTPUT:
    # bad.words = list of profane words
    
    # define name and destination of downloaded file
    data.path <- paste(getwd(), "/bad-words.txt", sep = "")
    
    # define URL where text file containing a list of profane words/phrases can be downloaded
    url <- "http://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
    
    # if necessary, download the text file
    if (!file.exists(data.path)){
        download.file(url, data.path, mode = "wb")
    }
    
    # read the lines of the text file containing profanities
    bad.words.con <- file(data.path, open = "r")
    bad.words <- readLines(bad.words.con, encoding = "UTF-8", skipNul = TRUE)
    close(bad.words.con)
    bad.words
}

# tokenize words from corpus and perform following processing:

# 1. Website URLs, Twitter handles and email addresses are removed.
# 2. Non-ASCII characters are removed.
# 3. Numbers are removed.
# 4. Punctuation is removed.
# 5. Extra white space is removed.
# 6. Profanity is removed

generate.tokens <- function(corpus, profanity, remove.stopwords = FALSE) {
    # INPUT:
    # corpus = text corpus (quanteda corpus object) OR character object (in the case of input text)
    # profanity = list of profane words 
    # remove.stopwords = whether or not to remove stopwords during tokenization
    
    # OUTPUT:
    # text.tokens = quanteda tokens object containing word tokens from corpus 
    
    # tokenize the corpus into words and perform all data processing in steps 1-5
    text.tokens <- tokens(corpus,
                          what="word1",
                          remove_numbers = TRUE,
                          remove_punct = TRUE,
                          remove_url = TRUE,
                          remove_separators = TRUE,
                          remove_symbols = TRUE,
                          verbose = quanteda_options("verbose"))
    
    # remove profanity (data processing step 6)
    text.tokens <- tokens_remove(text.tokens, pattern = profanity)
    
    # remove stopwords if requested
    if (remove.stopwords == TRUE) {
        text.tokens <- tokens_remove(text.tokens, pattern = stopwords("en"))
    }
    
    text.tokens
}

# process, tokenize and trim text input

generate.tokens.from.input <- function(input.text, profanity, highest.order.n = 7, remove.stopwords = FALSE, remove.lastword = FALSE) {
    # INPUT:
    # corpus = text corpus (quanteda corpus object) OR character object (in the case of input text)
    # profanity = list of profane words 
    # highest.order.n = highest-order n-gram used in the model
    # remove.stopwords = whether or not to remove stopwords
    # remove.lastword = whether or not to remove last word
    
    # OUTPUT:
    # text.tokens = processed and trimmed character vector containing word tokens from input text
    
    # tokenize the corpus into words and perform all basic data processing
    text.tokens <- tokens(input.text,
                          what="word1",
                          remove_numbers = TRUE,
                          remove_punct = TRUE,
                          remove_url = TRUE,
                          remove_separators = TRUE,
                          remove_symbols = TRUE,
                          verbose = quanteda_options("verbose"))
    
    # remove profanity 
    text.tokens <- tokens_remove(text.tokens, pattern = profanity)
    
    # remove stopwords if requested
    if (remove.stopwords == TRUE) {
        text.tokens <- tokens_remove(text.tokens, pattern = stopwords("en"))
    }
    
    # make all words lowercase
    text.tokens <- tolower(text.tokens)
    
    # if necessary, the last word is removed
    if (remove.lastword == TRUE) {
        text.tokens <- head(text.tokens, n = length(text.tokens) - 1)
    }
    
    # if necessary, the text is trimmed to the last n-1 words
    if (length(text.tokens) > highest.order.n - 1) {
        text.tokens <- tail(text.tokens, n = highest.order.n - 1)
    }
    
    
    text.tokens
}

# generate n-gram prediction tables from quanteda token object 

gen.ngrams <- function(text.tokens, num.grams) {
    # INPUT: 
    # text.tokens = word tokens (quanteda tokens object)
    # num.grams = number of words in n-gram (1-gram, 2-gram, 3-gram, etc.)
    
    # OUTPUT:
    # ngrams.freqs = dataframe containing a list of ngrams and their corresponding frequencies
    
    # create a tokens object containing a list of character vectors of n-grams
    tokens.ngrams <- tokens_ngrams(text.tokens, n = num.grams, concatenator = ' ')
    
    # create a document-feature matrix, converting all n-grams to lowercase and removing extra white space
    dfm.tokens.ngrams <- dfm(tokens.ngrams, tolower = TRUE)
    
    # create a dataframe listing each n-gram and its corresponding frequency
    ngrams.freqs <- textstat_frequency(dfm.tokens.ngrams)
    
    # for n >= 2 n-grams:
    # add a column containing all but the last word in the n-gram
    # add a column containing the predicted word
    if (num.grams >= 2) {
        ngrams.freqs$match.text <- unlist(strsplit(ngrams.freqs$feature, " [^ ]+$"))
        ngrams.freqs$prediction <- sub('.* (.*)$','\\1', ngrams.freqs$feature)
    }
    
    ngrams.freqs
}

# identify the most commonly-occurring unigram

def.most.freq.unigram <- function(unigrams) {
    # INPUT:
    # unigrams = dataframe containing frequencies of unigrams (as generated in Task 3)
    
    # OUTPUT:
    # most.freq.unigram = most frequently-occurring unigram
    
    # define most frequently-occurring unigram
    most.freq.unigram <- unigrams[1, "feature"]
}

# main prediction function for simple backoff model

backoff1.predict.next.word <- function(input.text, profanity, highest.order.n) {
    # INPUT: 
    # input.text = some input text 
    # profanity = list of profane words to be filtered during input text processing 
    # highest.order.n = highest-order n-gram used in prediction model 
    
    # OUTPUT: 
    # predicted.word <- word predicted by model
    
    # process and trim input data
    processed.input.text <- generate.tokens.from.input(input.text, profanity, highest.order.n = highest.order.n)
    
    # calculate initial length of input text 
    initial.len.input.text <- length(processed.input.text)
    
    # iteratively check for a match between the input text and n-gram feature, starting with highest-order n-grams
    # 5-grams
    predicted.word <- text.matching(processed.input.text, freq.5grams)
    
    # 4-grams 
    if (is.na(predicted.word)) {
        # if the input text has a length greater than 3, remove the first word
        if (length(processed.input.text) > 3) {
            processed.input.text <- tail(processed.input.text, 3)
        }
        
        # check for match
        predicted.word <- text.matching(processed.input.text, freq.4grams)
        
        # trigrams 
        if (is.na(predicted.word)) {
            # if the input text has a length greater than 2, remove the first word
            if (length(processed.input.text) > 2) {
                processed.input.text <- tail(processed.input.text, 2)
            }
            
            # check for match
            predicted.word <- text.matching(processed.input.text, freq.3grams)
            
            # bigrams 
            if (is.na(predicted.word)) {
                # if the input text has a length greater than 1, remove the first word
                if (length(processed.input.text) > 1) {
                    processed.input.text <- tail(processed.input.text, 1)
                }
                
                # check for match
                predicted.word <- text.matching(processed.input.text, freq.2grams)
                
                # unigrams 
                if (is.na(predicted.word)) {
                    predicted.word <- most.freq.unigram
                }
            }
        }
        
    }
    
    return(predicted.word)
    
}

# run stupid backoff algorithm

stupid.backoff <- function(input.text, profanity, alpha, iteration = 0, remove.stopwords = FALSE, remove.lastword = FALSE) {
    # INPUT:
    # input.text = input text, where the next word will be predicted
    # profanity = list of profane words to be filtered during input text processing 
    # alpha = initial coefficient used to determine prediction scores
    # iteration = iteration of the stupid backoff algorithm (used to calculate n-gram score coefficient)
    # remove.stopwords = option to include stopwords or not (affects processing of input text and n-gram model usage)
    # remove.lastword = whether to remove last word in input text
    
    # OUTPUT:
    # predictions = dataframe containing two columns: next predicted words, scores
    
    # calculate coefficient to use in determining prediction scores
    coeff <- alpha ^ iteration
    
    # process, trim and tokenize the input text
    processed.input.text <- generate.tokens.from.input(input.text, profanity, highest.order.n = 7, 
                                                       remove.stopwords = remove.stopwords, remove.lastword = remove.lastword)
    
    # count number of words in input text 
    num.tokens <- length(processed.input.text)
    
    # identify predicted word(s) and calculate their scores
    # case where stopwords are removed
    if (remove.stopwords == TRUE) {
        
        # check for matching 6-grams (first six words of 7-grams)
        if (num.tokens == 6) {
            predictions <- text.matching.sb(processed.input.text, freq.7grams.nostop, alpha)
        }
        
        # check for matching 5-grams (first five words of 6-grams)
        if (num.tokens == 5) {
            predictions <- text.matching.sb(processed.input.text, freq.6grams.nostop, alpha)
        }
        
        # check for matching 4-grams (first four words of 5-grams)
        if (num.tokens == 4) {
            predictions <- text.matching.sb(processed.input.text, freq.5grams.nostop, alpha)
        }
        
        # check for matching trigrams (first three words of 4-grams)
        if (num.tokens == 3) {
            predictions <- text.matching.sb(processed.input.text, freq.4grams.nostop, alpha)
        }
        
        # check for matching bigrams (first two words of 3-grams)
        if (num.tokens == 2) {
            predictions <- text.matching.sb(processed.input.text, freq.3grams.nostop, alpha)
        }
        
        # check for matching unigrams (first word of bigrams)
        if (num.tokens == 1) {
            predictions <- text.matching.sb(processed.input.text, freq.2grams.nostop, alpha)
        }
        
        # length of input text was zero after filtering
        else {
            predictions <- data.frame()
        }
        
        # if there was no match:
        # if length of input text was one, return the most common unigrams and their corresponding SB scores
        # otherwise, remove first word from input text and repeat process with reduced coefficient
        if (nrow(predictions) == 0) {
            if (num.tokens == 1 | num.tokens == 0) {
                predictions <- head(freq.1grams.nostop, 5)
                predictions <- mutate(predictions, score = coeff * frequency / sum(frequency))
                predictions <- head(predictions[, c("feature", "score")], 5)
                colnames(predictions) <- c('prediction', 'score')
            }
            else {
                processed.input.text <- processed.input.text[-1]
                predictions <- stupid.backoff(processed.input.text, profanity, alpha, iteration = iteration + 1, remove.stopwords = TRUE)
            }
        } 
    }
    
    # case where stopwords are left in
    if (remove.stopwords == FALSE) {
        
        # check for matching 6-grams (first six words of 7-grams)
        if (num.tokens == 6) {
            predictions <- text.matching.sb(processed.input.text, freq.7grams.withstop, alpha)
        }
        
        # check for matching 5-grams (first five words of 6-grams)
        if (num.tokens == 5) {
            predictions <- text.matching.sb(processed.input.text, freq.6grams.withstop, alpha)
        }
        
        # check for matching 4-grams (first four words of 5-grams)
        if (num.tokens == 4) {
            predictions <- text.matching.sb(processed.input.text, freq.5grams.withstop, alpha)
        }
        
        # check for matching trigrams (first three words of 4-grams)
        if (num.tokens == 3) {
            predictions <- text.matching.sb(processed.input.text, freq.4grams.withstop, alpha)
        }
        
        # check for matching bigrams (first two words of 3-grams)
        if (num.tokens == 2) {
            predictions <- text.matching.sb(processed.input.text, freq.3grams.withstop, alpha)
        }
        
        # check for matching unigrams (first word of bigrams)
        if (num.tokens == 1) {
            predictions <- text.matching.sb(processed.input.text, freq.2grams.withstop, alpha)
        }
        
        # length of input text was zero after filtering
        else {
            predictions <- data.frame()
        }
        
        # if there was no match:
        # if length of input text was one, return the most common unigrams and their corresponding SB scores
        # otherwise, remove first word from input text and repeat process with reduced coefficient
        if (nrow(predictions) == 0) {
            if (num.tokens == 1 | num.tokens == 0) {
                predictions <- head(freq.1grams.withstop, 5)
                predictions <- mutate(predictions, score = coeff * frequency / sum(frequency))
                predictions <- head(predictions[, c("feature", "score")], 5)
                colnames(predictions) <- c('prediction', 'score')
            }
            else {
                processed.input.text <- processed.input.text[-1]
                # print(processed.input.text)
                predictions <- stupid.backoff(processed.input.text, profanity, alpha, iteration = iteration + 1, remove.stopwords = FALSE)
            }
        } 
    }
    
    predictions
}

# check if input text matches n-gram feature (simple backoff model)

text.matching <- function(input.text, ngrams.freq) {
    # reduce dimensionality of input text to 1
    input.text <- paste(input.text, collapse = " ")
    print(class(input.text))
    
    # check for match between feature and input text
    print(class(ngrams.freq$match.text))
    predicted.word <- ngrams.freq[match(input.text, ngrams.freq$match.text), ]$prediction
}

# check if input text matches n-gram feature (stupid backoff model)

text.matching.sb <- function(input.text, ngrams.freq, coeff) {
    # INPUT:
    # input.text = input text 
    # ngrams.freq = set of n-grams
    # coeff = stupid backoff coefficient, based on alpha and iteration number
    
    # OUTPUT:
    # predicted.word = dataframe containing two columns: top five predicted words (left) and stupid backoff scores (right)
    
    # reduce dimensionality of input text to 1
    input.text <- paste(input.text, collapse = " ")
    
    # check for match between feature and input text
    predicted.word <- subset(ngrams.freq, match.text == input.text)
    
    # calculate stupid backoff scores for each predicted word
    predicted.word <- mutate(predicted.word, score = coeff * frequency / sum(frequency))
    
    # return (at most) the top five predicted words
    if (nrow(predicted.word > 5)) {
        predicted.word <- head(predicted.word[, c("prediction", "score")], 5)
    }
    
    predicted.word
}

# return last token of input text

get.last.word <- function(input.text, profanity, highest.order.n = 7, remove.stopwords = FALSE) {
    # INPUT:
    # input.text = text to be tokenized, processed and trimmed
    # profanity = list of profane words to be filtered
    # highest.order.n = maximum n - 1 tokens
    # remove.stopwords = whether to remove stopwords
    
    # OUTPUT:
    # last.word = last token in processed input text
    
    # tokenize input text, process and trim to maximum of 7 tokens
    processed.input.text <- generate.tokens.from.input(input.text, profanity, highest.order.n = 7, 
                                                       remove.stopwords = remove.stopwords, remove.lastword = FALSE)
    
    # get last token of processed input text
    last.word = tail(processed.input.text, 1)
    
    last.word
}

# check if the last word of the test text is one of the top five predicted words

match.test.text <- function(predictions, last.word) {
    # INPUT:
    # predictions = data frame including predicted next words from all but last word of test text
    # last.word = last word of test text
    
    # OUTPUT:
    # match.test = match result (1 = at least one match; 0 = no matches)
    
    # reduce dimensionality of input text to 1
    last.word <- paste(last.word, collapse = " ")
    
    # check for match between prediction and last word of test text
    predicted.word <- subset(predictions, prediction == last.word)
    
    # return binary result (at least one match = 1, no match = 0)
    if (nrow(predicted.word) > 0) {
        match.test <- 1
    }
    else {
        match.test <- 0
    }
    
    match.test
}