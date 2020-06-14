###################################################################################################################
#Hult International Business School 
#MBAN 2020 Data Science: R
#Tuba Ali 
###################################################################################################################
###################################################################################################################
#                                           
#                                           ANALYZING IMDB TOP 5 & BOTTOM 5 MOVIES' REVIEWS
#
###################################################################################################################
#Calling libraries
library(dplyr)
library(tidytext)
library(textreadr)
library(reshape2)
library(tidyr)
library(stringr)
library(wordcloud)
library(ggplot2)
library(reshape2)


###################################################################################################################
#                                               DEFINING DATA FRAMES
###################################################################################################################
# Set working directory to where the text file is
setwd("~Users/tubaali/Desktop/Text Analytics")

# Reading text files 
#IMDB Top 5 Movies Reviews
top_5 <- read_document(file = "Top 5 .txt")
top_5_df <- tibble(text =as.character(top_5))

#IMDB Bottom 5 Movies Reviews
bottom_5 <- read_document(file = "Bottom 5.txt")
bottom_5_df <- tibble(text =as.character(bottom_5))

###################################################################################################################
#                                                 STOP WORDS
###################################################################################################################
#Calling the Stop Words
data("stop_words")

#Custom Stop Words +added helpful, sign, vote, reviews bec. it's website thing
cust_stop_top<- tibble(word = c("film", "movie", "is","movies", "spoilers", "makes", "de", "ii", "10", "review", "found", "films",
                                "godfather", "michael", "batman", "found", "1", "helpful", "sign", "vote", "reviews"))

cust_stop_bottom <-tibble(word = c("movie", "film", "10", "1", "vote", "review", "found", "imdb", "films", "movies", "helpful",
                                   "sign", "vote", "reviews"))

###################################################################################################################
#                                                 TOKENIZING TOP 5
###################################################################################################################
top_5_token <- top_5_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

top_5_token

#Taking out the customize stop words --> film, movie, is, spoilers etc. 
top_5_token_cust <- top_5_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop_top) %>%
  count(word, sort = TRUE)

top_5_token_cust

#Visualize top 15 words for Top 5
top_5_token_cust %>%
  mutate(word = reorder(word, n)) %>% 
  top_n(15) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

###################################################################################################################
#                                               SENTIMENT ANALYSIS FOR TOP 5
###################################################################################################################
#Get sentiments from bing
top_5_sentiment <- top_5_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop_top) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

top_5_sentiment

#Making a comparrison cloud with sentiment bing --> Positive/Negative
top_5_token_cust %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("gray80", "gray20"),
                   max.words=50,
                   scale = c(0.5,0.5),
                   title.size = 1)


###################################################################################################################
#                                             SENTIMENT TF-IDF FOR TOP 5
###################################################################################################################
#td-idf to sentiment
top_5_sentiment <- top_5_sentiment %>%
  bind_tf_idf(word, sentiment, n) %>%
  arrange(desc(tf_idf))

top_5_sentiment

#Visualizing Tf-idf to bing sentiment 
top_5_sentiment %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = sentiment)) + 
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~sentiment, ncol = 2, scales = "free") + 
  coord_flip()

###################################################################################################################
#                                           SENTIMENT BIGRAM FOR TOP 5
###################################################################################################################
#bigram with stop words
top_5_bigrams <- top_5_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)

top_5_bigrams

#Bigram filtered stop words
top_5_bigrams_separated <- top_5_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

top_5_bigrams_filtered <- top_5_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 

top_5_bigrams_filtered

#New bigram counts
top_5_bigram_counts <- top_5_bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

top_5_bigram_counts

###################################################################################################################
###################################################################################################################
#
#                                               SECOND PART 
#                                             Bottom 5 Analysis

##################################################################################################################
###################################################################################################################



###################################################################################################################
#                                             TOKENIZING BOTTOM 5
###################################################################################################################
#Tokenizing
bottom_5_token <- bottom_5_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

bottom_5_token

#Taking out the customize stop words --> film, movie, is, spoilers etc. 
bottom_5_token_cust <- bottom_5_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop_bottom) %>%
  count(word, sort = TRUE)

bottom_5_token_cust

#Visualize top 15 words for bottom 5
bottom_5_token_cust %>%
  mutate(word = reorder(word, n)) %>% 
  top_n(15) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

###################################################################################################################
#                                           SENTIMENT ANALYSIS FOR BOTTOM 5
###################################################################################################################
#Adding sentiment bing
bottom_5_sentiment <- bottom_5_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop_bottom) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bottom_5_sentiment

#Making a comparrison cloud with sentiment bing --> Positive/Negative
bottom_5_token_cust %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("gray80", "gray20"),
                   max.words=50,
                   scale = c(0.5,0.5),
                   title.size = 1)

###################################################################################################################
#                                           SENTIMENT TF-IDF FOR BOTTOM 5
###################################################################################################################
#td-idf to sentiment
bottom_5_sentiment <- bottom_5_sentiment %>%
  bind_tf_idf(word, sentiment, n) %>%
  arrange(desc(tf_idf))

bottom_5_sentiment

#Visualizing Tf-idf to bing sentiment 
bottom_5_sentiment %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = sentiment)) + 
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~sentiment, ncol = 2, scales = "free") + 
  coord_flip()

###################################################################################################################
#                                             SENTIMENT BIGRAM FOR BOTTOM 5
###################################################################################################################
#bottom bigram with stop words
bottom_5_bigrams <- bottom_5_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)

bottom_5_bigrams

#Bigram filtered stop words
bottom_5_bigrams_separated <- bottom_5_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bottom_5_bigrams_filtered <- bottom_5_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bottom_5_bigrams_filtered

# new bigram counts:
bottom_5_bigram_counts <- bottom_5_bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bottom_5_bigram_counts
