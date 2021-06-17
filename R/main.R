# Checking if the data was previously downloaded
source("./R/get_data.R")
#
# Loading the data into the workspace
twitter <- readLines("./final/en_US/en_US.twitter.txt", 
                     encoding = "UTF-8", skipNul = T)
blogs   <- readLines("./final/en_US/en_US.blogs.txt", 
                     encoding = "UTF-8", skipNul = T)
news    <- readLines("./final/en_US/en_US.news.txt", 
                     encoding = "UTF-8", skipNul = T)
#
set.seed(3030)
#
nsamples = 7000
#
# Sampling the data
twitter <- sample(twitter, nsamples)
blogs   <- sample(blogs, nsamples)
news    <- sample(news, nsamples)
#
sData <- c(twitter, blogs, news)
#
#
library(dplyr)
library(stringi)
text_df <- tibble(text = sData)
#
data(stop_words)
library(readr)
library(tidytext)
library(stringr)
#
# Remove profanities - list from http://www.bannedwordlist.com/lists/swearWords.csv, and stored in final/en_US folder
profanities <- read_delim("./final/en_US/swearWords.csv", delim = "\n", col_names = FALSE)

profanities <- unnest_tokens(profanities, word, X1)
replace_reg <- "[^[:alpha:][:space:]]*"
replace_url <- "http[^[:space:]]*"
replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"  

text_df <- text_df %>%
    mutate(text = str_replace_all(text, replace_reg, "")) %>%
    mutate(text = str_replace_all(text, replace_url, "")) %>%
    mutate(text = str_replace_all(text, replace_aaa, "")) %>% 
    mutate(text = iconv(text, "latin1", "ASCII", sub=""))
#
# Unigrams
unigram <- text_df %>%
    unnest_tokens(word, text) %>%
    anti_join(profanities) %>%
    anti_join(stop_words)
#
# Bigram
bigram <- text_df  %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    filter(!word1 %in% profanities$word) %>%
    filter(!word2 %in% profanities$word) %>%
    count(word1, word2, sort = TRUE)
#
# Trigram
trigram <- text_df  %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3)
#
# Quadgrams
quadgram <- text_df  %>%
    unnest_tokens(quadgram, text, token = "ngrams", n = 4)
#
bigram_counts <- bigram %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    count(word1, word2, sort = TRUE)
#
trigrams_counts <- trigram %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
    count(word1, word2, word3, sort = TRUE)
#
quadgrams_counts <- quadgram %>%
    separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
    # filter(!word1 %in% stop_words$word) %>%
    # filter(!word2 %in% stop_words$word) %>%
    # filter(!word1 %in% profanities$word) %>%
    # filter(!word2 %in% profanities$word) %>%
    count(word1, word2, word3, word4, sort = TRUE)
#
library(tidyr)
pentagrams_counts <- text_df  %>%
    unnest_tokens(quintgram, text, token = "ngrams", n = 5) %>%
    separate(quintgram, c("word1", "word2", "word3", "word4", "word5"), sep = " ") %>%
    count(word1, word2, word3, word4, word5, sort = TRUE)
#
saveRDS(bigram_counts, file="./bigram2.Rda")
saveRDS(trigrams_counts, file="./trigram2.Rda")
saveRDS(quadgrams_counts, file="./cuadgram2.Rda")
saveRDS(pentagrams_counts, file="./R/databases/pentagram2.Rda")
