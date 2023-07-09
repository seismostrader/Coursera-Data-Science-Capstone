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
    # corpus = text corpus (quanteda corpus object)
    # profanity = list of profane words 
    
    # OUTPUT:
    # text.tokens = quanteda tokens object containing word tokens from corpus 
    
    # tokenize the corpus into words and perform all data processing in steps 1-5
    text.tokens <- tokens(text.EN.corpus,
                          what="word1",
                          remove_numbers = TRUE,
                          remove_punct = TRUE,
                          remove_url =TRUE,
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

