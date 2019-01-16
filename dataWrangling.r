library("dplyr")
library("jsonlite")
library("stringr")
library("stringi")
library("plotly")


get_word_frequency <- function(texts){
  
  #collapses all messages into one string and gets the word frequency for both of us
  texts <- paste(texts, collapse=" ")
  
  texts <- gsub("\\.", " ", texts)
  texts <- gsub("\\,", " ", texts)
  texts <- tolower(texts)
  
  words_in_texts <- strsplit(texts, " ")
  word_freq <- table(unlist(words_in_texts))
  
  return(word_freq)
  
}

#gets text data and parses through only needed columns
all_texts <- fromJSON("data/message_data_1_15.json")

both_texts <- select(all_texts, date, text, handle_id, is_from_me, payload_data) %>% 
               filter(handle_id == 152)

#adds formatted time to the df
both_texts$date <- substr(both_texts$date, 1, 9)
both_texts$date <- as.numeric(both_texts$date)

both_texts$converted_date <- as.POSIXct("01.01.1970 00:00:00", format = "%m.%d.%Y %H:%M:%S") 
both_texts$converted_date <- both_texts$converted_date + (both_texts$date + 978307200 - 28800 )

both_texts$date <- both_texts$converted_date

both_texts <- subset(both_texts, select = -converted_date)


#check numer of messages sent
sent_by_me <- nrow(filter(both_texts, is_from_me == 1))
sent_by_her <- nrow(filter(both_texts, is_from_me == 0))


#add character count to data frames
both_texts$text_length_char <- nchar(both_texts$text)
both_texts$text_length_word <- sapply(strsplit(both_texts$text, " "), length)

#separate who sent
harim_texts <- filter(both_texts, is_from_me == 1)
betsy_texts <- filter(both_texts, is_from_me == 0)

#get the number of texts sent on a day for both
texts_by_day_both <- both_texts
texts_by_day_both$date <- as.Date(as.character(both_texts$date))
texts_temp <- texts_by_day_both
texts_by_day_both <- texts_by_day_both %>% count(date)
colnames(texts_by_day_both) <- c("Date", "Number of Texts")

#get the number of texts sent on a day for me
texts_by_day_harim <- harim_texts
texts_by_day_harim$date <- as.Date(as.character(harim_texts$date))
texts_by_day_harim <- texts_by_day_harim %>% count(date)
colnames(texts_by_day_harim) <- c("Date", "Number of Texts")

#get the number of texts sent on a day for Betsy
texts_by_day_betsy <- betsy_texts
texts_by_day_betsy$date <- as.Date(as.character(betsy_texts$date))
texts_by_day_betsy <- texts_by_day_betsy %>% count(date)
colnames(texts_by_day_betsy) <- c("Date", "Number of Texts")


#emoji stufffffffffffff
texts <- paste(both_texts$text, collapse = " ")

emoji <- readLines("data/emoji_list.txt", encoding="UTF-8") %>%
  stri_subset_regex(pattern = "^[^#]") %>%
  stri_subset_regex(pattern = ".+")


## get the emoji characters and clean them up
emoji_chars <- emoji %>%
                stri_extract_all_regex(pattern = "# *.{1,2} *") %>%
                stri_replace_all_fixed(pattern = c("*", "#"),
                         replacement = "",
                         vectorize_all=FALSE) %>%
                stri_trim_both() 

## get the emoji character descriptions
emoji_descriptions <- emoji %>%
                        stri_extract_all_regex(pattern = "#.*$") %>%
                        stri_replace_all_regex(pattern = "# *.{1,2} *",
                         replacement = "") %>%
                        stri_trim_both()
emoji_descriptions <- gsub(" ", "_", emoji_descriptions)
emoji_descriptions <- paste(" ", emoji_descriptions, " ")

## replace emoji characters with their descriptions.

texts_emoji <- stri_replace_all_regex(texts,
                       pattern = emoji_chars,
                       replacement = emoji_descriptions,
                       vectorize_all=FALSE)


#get the number of times each emoji has shown up in texts
emoji_count <- data.frame(emoji_descriptions, str_count(texts_emoji, emoji_descriptions))
colnames(emoji_count) <- c("emoji", "times_sent")

#remove _ and replace it with space
emoji_count$emoji <- gsub("_", " ", emoji_count$emoji)

#get top 10 emoji
top_emoji <- emoji_count %>% arrange(-times_sent) %>% distinct() %>% slice(0:10)

#get most frequenlty used words
harim_words <- paste(harim_texts$text, collapse = " ")
harim_word_freq <- as.data.frame(get_word_frequency(harim_words))

betsy_words <- paste(betsy_texts$text, collapse = " ")
betsy_word_freq <- as.data.frame(get_word_frequency(betsy_words))

both_word_freq <- as.data.frame(get_word_frequency(texts_emoji))






#graphs

both_texts_over_time_p <- plot_ly(data = texts_by_day_both,
                                  x = texts_by_day_both$Date,
                                  y = texts_by_day_both$"Number of Texts",
                                  type = "scatter",
                                  mode = "lines",
                                  name = "Number of Texts Over Time (both)")

harim_texts_over_time_p <- plot_ly(data = texts_by_day_harim,
                                   x = texts_by_day_harim$Date,
                                   y = texts_by_day_harim$"Number of Texts",
                                   type = "scatter",
                                   mode = "lines",
                                   name = "Number of Texts Over Time (Harim)")

betsy_texts_over_time_p <- plot_ly(data = texts_by_day_betsy,
                                   x = texts_by_day_betsy$Date,
                                   y = texts_by_day_betsy$"Number of Texts",
                                   type = "scatter",
                                   mode = "lines",
                                   name = "Number of Texts Over Time (Betsy)")


top_emoji_p <- plot_ly(x = seq(1, 10, 1),
                       y = top_emoji$times_sent,
                       type = "bar",
                       name = "Top Emojis",
                       text = top_emoji$times_sent,
                       textposition = "auto",
                       marker = list(color = 'rgb(158,202,225)',
                                     line = list(color = 'rgb(8,48,107)', width = 1.5))) %>% 
               layout(xaxis = list(title = "Emoji Rank",
                                   showticklabels = TRUE),
                      yaxis = list(title = "Times Used"))
