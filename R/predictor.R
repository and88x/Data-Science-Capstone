library(tibble)
library(dplyr)
library(stringr)
#
big <- function(input_words,bigram_counts){
    num <- length(input_words)
    filter(bigram_counts, 
           word1==input_words[num]) %>% top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 2)) %>%
        as.character() -> out
    ifelse(out =="character(0)", "?", return(out))
}
#
trig <- function(input_words,trigrams_counts,b){
    num <- length(input_words)
    filter(trigrams_counts, 
           word1==input_words[num-1], 
           word2==input_words[num])  %>%  top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 3)) %>%
        as.character() -> out
    ifelse(out=="character(0)", big(input_words,b), return(out))
}
#
quadg <- function(input_words,quadgrams_counts,t,b){
    num <- length(input_words)
    filter(quadgrams_counts, 
           word1==input_words[num-2], 
           word2==input_words[num-1], 
           word3==input_words[num])  %>% top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 4)) %>%
        as.character() -> out
    
    ifelse(out=="character(0)", trig(input_words,t,b), return(out))
}
#
ngrams <- function(input,qc,tc,bc){

    # Create a dataframe
    input <- tibble(text = input)
    # Clean the Inpput
    replace_reg <- "[^[:alpha:][:space:]]*"
    input <- input %>%
        mutate(text = str_replace_all(text, replace_reg, ""))
    # Find word count, separate words, lower case
    input_count <- str_count(input, boundary("word"))
    input_words <- unlist(str_split(input, boundary("word")))
    
if (input_count > 0){
    # Call the matching functions
    if (input_count == 1) { 
        out = big(input_words,bc)
    } else if (input_count == 2) { 
        out = trig(input_words,tc,bc)
    } else {
        out = quadg(input_words,qc,tc,bc)
    } 
    # Output
    return(out)
} else { 
    return("?")
}

}