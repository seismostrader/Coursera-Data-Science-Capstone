#######################################################################################################
# Functions for Predictive Text Shiny App
#
# Coursera Data Science Specialization Capstone Project
# Author: Anne Strader
#######################################################################################################

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

generate.tokens <- function(corpus, profanity) {
    # INPUT:
    # corpus = text corpus (quanteda corpus object) OR character object (in the case of input text)
    # profanity = list of profane words 
    
    # OUTPUT:
    # text.tokens = quanteda tokens object containing word tokens from corpus 
    
    # tokenize the corpus into words and perform all data processing in steps 1-5
    text.tokens <- tokens(text.EN.corpus,
                          what="word1",
                          remove_numbers = TRUE,
                          remove_punct = TRUE,
                          remove_url = TRUE,
                          remove_separators = TRUE,
                          remove_symbols = TRUE,
                          verbose = quanteda_options("verbose"))
    
    # remove profanity (data processing step 6)
    text.tokens <- tokens_remove(text.tokens, pattern = profanity)
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

# process, tokenize and trim text input

generate.tokens.from.input <- function(input.text, profanity, highest.order.n = 5) {
    # INPUT:
    # corpus = text corpus (quanteda corpus object) OR character object (in the case of input text)
    # profanity = list of profane words 
    # highest.order.n = highest-order n-gram used in the model
    
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
    
    # make all words lowercase
    text.tokens <- tolower(text.tokens)
    
    # convert from quanteda tokens object to character vector
    text.tokens <- as.character(text.tokens)
    
    # if necessary, the text is trimmed to the last n-1 words
    if (length(text.tokens) > highest.order.n - 1) {
        text.tokens <- tail(text.tokens, n = highest.order.n - 1)
    }
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

# check if input text matches n-gram feature (simple backoff model)

text.matching <- function(input.text, ngrams.freq) {
    # reduce dimensionality of input text to 1
    input.text <- paste(input.text, collapse = " ")
    
    # check for match between feature and input text
    predicted.word <- ngrams.freq[match(input.text, ngrams.freq$match.text), ]$prediction
}