library(here)
library(readr)
library(stringr)
library(tidyverse)
library(tidytext)


# end goal - graph net sentiment over time against bit coin price 

setwd('~/projects/cointalk')

df <- read_file('cointalk.txt')

text = str_split(df,'\\n\n')
 
 
 first <- function(x){
   z = str_split(x, ":") %>% unlist(.)
   y= unlist(z[[1]])
   print(y[1])
   return(y[1])}
 
#str_split(new_text, ":") %>% unlist(.) %>% first()
 
# map(.x = text_df$new_text, first)
 
 
 second <- function(x){
   z = ifelse(str_detect(x,'Jay Kang:'),str_replace(x,'Jay Kang:', ""),x )
   y = ifelse(str_detect(z,'Aaron Lammer:'),str_replace(z,'Aaron Lammer:', ""),z )
   print(y)
   #y= unlist(z[[2]])
   #print(y)
   return(y)
 }
 
 #two<- map(.x = text_df$new_text, second)
 
 
new_text <- unlist(text)
 
test1<- map(new_text,first)
test2<- map(new_text,second)

 conversation_df <- tibble(name = unlist(test1),text = test2)
 
 conversation_df <-  conversation_df %>% mutate(linenumber=row_number())
 
 tidy_conversation <- conversation_df %>% 
                            unnest_tokens(word, text) 
 
 tidy_conversation <- tidy_conversation %>% 
                            anti_join(stop_words)
 
 
 bing <- get_sentiments('bing')
 
 tidy_conversation <- tidy_conversation %>%
   # With inner join, implement sentiment analysis using `bing`
   inner_join(bing)
 
 
lammer_df <- conversation_df %>% filter(name == 'Aaron Lammer')
kang_df <- conversation_df %>% filter(name == 'Jay Kang')
 


# tidy_conversation

lammer_tidy <- tidy_conversation %>% filter(name == 'Aaron Lammer')
kang_tidy <- tidy_conversation %>% filter(name == 'Jay Kang') 


lammer_tidy_no_grin <- lammer_tidy %>% filter(word != 'grin')

kang_tidy_no_grin <- kang_tidy %>% filter(word != 'grin')
 
lammer_tidy_no_grin <- lammer_tidy_no_grin %>% mutate(score = ifelse(sentiment == 'negative',0,1))
kang_tidy_no_grin   <- kang_tidy_no_grin %>% mutate(score = ifelse(sentiment == 'negative',0,1))


lammer_positive_new <- sum(lammer_tidy_no_grin$score)/dim(lammer_tidy_no_grin)[1]
kang_positive_new   <- sum(kang_tidy_no_grin$score)/dim(kang_tidy_no_grin)[1]



### episode 2

df <- read_file("coin_talk_2.txt")

text = str_split(df,'\\n\n')


first <- function(x){
  z = str_split(x, ":") %>% unlist(.)
  y= unlist(z[[1]])
  print(y[1])
  return(y[1])}


second <- function(x){
  z = ifelse(str_detect(x,'Jay Kang:'),str_replace(x,'Jay Kang:', ""),x )
  y = ifelse(str_detect(z,'Aaron Lammer:'),str_replace(z,'Aaron Lammer:', ""),z )
  print(y)
  #y= unlist(z[[2]])
  #print(y)
  return(y)
}




new_text <- unlist(text)

test1<- map(new_text,first)
test2<- map(new_text,second)

conversation_df <- tibble(name = unlist(test1),text = test2)

conversation_df <-  conversation_df %>% mutate(linenumber=row_number())

tidy_conversation <- conversation_df %>% 
  unnest_tokens(word, text) 

tidy_conversation <- tidy_conversation %>% 
  anti_join(stop_words)


bing <- get_sentiments('bing')

tidy_conversation <- tidy_conversation %>%
  # With inner join, implement sentiment analysis using `bing`
  inner_join(bing)


lammer_df <- conversation_df %>% filter(name == 'Aaron Lammer')
kang_df <- conversation_df %>% filter(name == 'Jay Kang')



# tidy_conversation

#tidy_conversation<- tidy_conversation %>% filter(word != 'grin')


lammer_tidy <- tidy_conversation %>% filter(name == 'Aaron Lammer')
kang_tidy <- tidy_conversation %>% filter(name == 'Jay Kang') 


lammer_tidy_no_grin <- lammer_tidy %>% filter(word != 'grin')

kang_tidy_no_grin <- kang_tidy %>% filter(word != 'grin')

lammer_tidy_no_grin <- lammer_tidy_no_grin %>% mutate(score = ifelse(sentiment == 'negative',0,1))
kang_tidy_no_grin   <- kang_tidy_no_grin %>% mutate(score = ifelse(sentiment == 'negative',0,1))


lammer_positive <- sum(lammer_tidy_no_grin$score)/dim(lammer_tidy_no_grin)[1]
kang_positive   <- sum(kang_tidy_no_grin$score)/dim(kang_tidy_no_grin)[1]






















 data(stop_words)
 
 tidy_books <- tidy_books %>%
   anti_join(stop_words)
 
 bing <- get_sentiments('bing')
 
 
 
 text_df <- tibble(line = 1:length(text[[1]]), text = text[[1]])
 
 text_df<- text_df[2:323,] 
 
 text_df <- text_df %>% mutate(speaker = str_split(text,))
 
 
 text_df %>% mutate(newtext = str_split(text, ':')) %>% sapply(extract2, 1)
 
 map_chr()
 
 first <- function(x){
   z = str_split(x, ":") %>% unlist(.)
   y= unlist(z[[1]])
   return(y)}
    
 str_split(new_text, ":") %>% unlist(.) %>% first()
 
 map(.x = text_df$new_text, first)
 
 
second <- function(x){
   z = str_split(x, ":") %>% unlist(.)
   y= unlist(z[[2]])
   print(y)
}

two<- map(.x = text_df$new_text, second)





library(tidyr)




# Load tidytext
library(tidytext)

tidy_shakespeare <- shakespeare %>%
  # Group by the titles of the plays
  # Define a new column linenumber
  mutate(linenumber=row_number()) %>%
  # Transform the non-tidy text data to tidy text data
  unnest_tokens(word, text) %>%
  ungroup()

# Pipe the tidy Shakespeare data frame to the next line
tidy_shakespeare %>% 
  # Use count to find out how many times each word is used
  count(word, sort = TRUE)



tidy_shakespeare %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, type, index = linenumber %/% 70, sentiment) %>%
  # Spread sentiment and n across multiple columns
  spread(sentiment, n, fill = 0) %>%
  # Use mutate to find net sentiment
  mutate(sentiment = positive-negative)


bing <- get_sentiments('bing')

# Use data frame with text data
geocoded_tweets %>%
  # With inner join, implement sentiment analysis using `bing`
  inner_join(bing)
