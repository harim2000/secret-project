start_time <- Sys.time()

library("plyr")
library("dplyr")
library("jsonlite")
library("stringr")
library("stringi")
library("plotly")
library("lubridate")
library('tidyr')


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

#gets text data and parses through only needed columns--------------------------
#deletes after use
all_texts <- fromJSON("data/message_data.json")

both_texts <- select(all_texts, date, text, handle_id, is_from_me, payload_data) %>% 
               filter(handle_id == 152)
num_texts <- nrow(both_texts)

all_texts <- ""

#adds formatted time to the df--------------------------------------------------
both_texts$date <- substr(both_texts$date, 1, 9)
both_texts$date <- as.numeric(both_texts$date)

both_texts$converted_date <- as.POSIXct("01.01.1970 00:00:00", format = "%m.%d.%Y %H:%M:%S") 
both_texts$converted_date <- both_texts$converted_date + (both_texts$date + 978307200 - 28800 )

both_texts$date <- both_texts$converted_date

both_texts <- subset(both_texts, select = -converted_date)


#convert emoji to description---------------------------------------------------

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

#separate by who sent-----------------------------------------------------------
harim_texts <- filter(both_texts, is_from_me == 1)
betsy_texts <- filter(both_texts, is_from_me == 0)


#replaces emojis in text for both-----------------------------------------------
both_texts$text <- stri_replace_all_regex(both_texts$text,
                                          pattern = emoji_chars,
                                          replacement = emoji_descriptions,
                                          vectorize_all=FALSE)
#replaces emojis in text for me
harim_texts$text <- stri_replace_all_regex(harim_texts$text,
                                           pattern = emoji_chars,
                                           replacement = emoji_descriptions,
                                           vectorize_all=FALSE)
#replaces emojis in text for her
betsy_texts$text <- stri_replace_all_regex(betsy_texts$text,
                                           pattern = emoji_chars,
                                           replacement = emoji_descriptions,
                                           vectorize_all=FALSE)

#remove emoji from text for word counts-----------------------------------------
both_texts_noemoji <- both_texts

for(emoji in emoji_descriptions){
  both_texts_noemoji$text <- gsub(emoji, "", both_texts_noemoji$text)
}

harim_texts_noemoji <- harim_texts

for(emoji in emoji_descriptions){
  harim_texts_noemoji$text <- gsub(emoji, "", harim_texts_noemoji$text)
}

betsy_texts_noemoji <- betsy_texts

for(emoji in emoji_descriptions){
  betsy_texts_noemoji$text <- gsub(emoji, "", betsy_texts_noemoji$text)
}



#add character and word count to data frames------------------------------------
both_texts_noemoji$text_length_char <- nchar(both_texts_noemoji$text)
both_texts_noemoji$text_length_word <- sapply(strsplit(both_texts_noemoji$text, " "), length)

harim_texts$text_length_char <- nchar(harim_texts$text)
harim_texts$text_length_word <- sapply(strsplit(harim_texts$text, " "), length)
harim_texts_noemoji$text_length_char <- nchar(harim_texts_noemoji$text)
harim_texts_noemoji$text_length_word <- sapply(strsplit(harim_texts_noemoji$text, " "), length)

betsy_texts$text_length_char <- nchar(betsy_texts$text)
betsy_texts$text_length_word <- sapply(strsplit(betsy_texts$text, " "), length)
betsy_texts_noemoji$text_length_char <- nchar(betsy_texts_noemoji$text)
betsy_texts_noemoji$text_length_word <- sapply(strsplit(betsy_texts_noemoji$text, " "), length)

#get number sent----------------------------------------------------------------
num_harim_sent <- nrow(harim_texts)
num_betsy_sent <- nrow(betsy_texts)

#remove blank rows in no emoji--------------------------------------------------
both_texts_noemoji <- both_texts_noemoji %>% filter(text_length_char != 0)
harim_texts_noemoji <- harim_texts_noemoji %>% filter(text_length_char != 0)
betsy_texts_noemoji <- betsy_texts_noemoji %>% filter(text_length_char != 0)

#get the number of texts sent on a day for both---------------------------------
texts_by_day_both <- both_texts
texts_by_day_both$date <- as.Date(as.character(both_texts$date))
texts_temp <- texts_by_day_both
texts_by_day_both <- texts_by_day_both %>% dplyr::count(date)
colnames(texts_by_day_both) <- c("Date", "Number of Texts")

texts_by_day_both <- texts_by_day_both %>% 
  mutate(Date = as.Date(Date)) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by='day'))
texts_by_day_both$`Number of Texts` <- replace_na(texts_by_day_both$`Number of Texts`, 0)

#get the number of texts sent on a day for me-----------------------------------
texts_by_day_harim <- harim_texts
texts_by_day_harim$date <- as.Date(as.character(harim_texts$date))
texts_by_day_harim <- texts_by_day_harim %>% dplyr::count(date)
colnames(texts_by_day_harim) <- c("Date", "Number of Texts")

texts_by_day_harim <- texts_by_day_harim %>% 
  mutate(Date = as.Date(Date)) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by='day'))
texts_by_day_harim$`Number of Texts` <- replace_na(texts_by_day_harim$`Number of Texts`, 0)

temp <- data.frame(as.Date('2019-02-12'), 0)
colnames(temp) <- colnames(texts_by_day_harim)
texts_by_day_harim <- rbind(texts_by_day_harim, temp)
#get the number of texts sent on a day for Betsy--------------------------------
texts_by_day_betsy <- betsy_texts
texts_by_day_betsy$date <- as.Date(as.character(betsy_texts$date))
texts_by_day_betsy <- texts_by_day_betsy %>% dplyr::count(date)
colnames(texts_by_day_betsy) <- c("Date", "Number of Texts")

texts_by_day_betsy <- texts_by_day_betsy %>% 
  mutate(Date = as.Date(Date)) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by='day'))
texts_by_day_betsy$`Number of Texts` <- replace_na(texts_by_day_betsy$`Number of Texts`, 0)
#Both Emoji---------------------------------------------------------------------
## replace emoji characters with their descriptions. 
texts_emoji <- stri_replace_all_regex(paste(both_texts$text, collapse = " "),
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

#Harim Emoji--------------------------------------------------------------------
harim_emoji_count <- data.frame(emoji_descriptions, 
                                str_count(
                                  paste(harim_texts$text, collapse = " "),
                                  emoji_descriptions))

colnames(harim_emoji_count) <- c("emoji", "times_sent")

#remove _ and replace it with space
harim_emoji_count$emoji <- gsub("_",  " ", harim_emoji_count$emoji)

#get top 10 emoji
top_harim_emoji <- harim_emoji_count %>% arrange(-times_sent) %>% distinct() %>% slice(0:10)

top_harim_emoji$emoji <- factor(top_harim_emoji$emoji, levels = top_harim_emoji$emoji)

#Betsy Emoji--------------------------------------------------------------------
betsy_emoji_count <- data.frame(emoji_descriptions, 
                                str_count(
                                  paste(betsy_texts$text, collapse = " "),
                                  emoji_descriptions))

colnames(betsy_emoji_count) <- c("emoji", "times_sent")

#remove _ and replace it with space
betsy_emoji_count$emoji <- gsub("_",  " ", betsy_emoji_count$emoji)

#get top 10 emoji
top_betsy_emoji <- betsy_emoji_count %>% arrange(-times_sent) %>% distinct() %>% slice(0:10)


#get most frequenlty used words-------------------------------------------------
harim_words <- paste(harim_texts$text, collapse = " ")
harim_word_freq <- as.data.frame(get_word_frequency(harim_words))

betsy_words <- paste(betsy_texts$text, collapse = " ")
betsy_word_freq <- as.data.frame(get_word_frequency(betsy_words))

both_word_freq <- as.data.frame(get_word_frequency(texts_emoji))


#usage of hearts over time?-----------------------------------------------
heart_texts <- both_texts %>% filter(grepl("heart|corazon", tolower(both_texts$text)))
heart_texts$date <- as.Date(as.character((heart_texts$date)))
heart_texts$count <- str_count(tolower(heart_texts$text), "heart|corazon")

heart_texts_time <- heart_texts %>% dplyr::group_by(date) %>% dplyr::summarise(sum(count))
heart_texts_time <- heart_texts_time %>% 
                      mutate(date = as.Date(date)) %>% 
                        complete(date = seq.Date(min(date), max(date), by='day'))
heart_texts_time$`sum(count)` <- replace_na(heart_texts_time$`sum(count)`, 0)
heart_texts_time <- select(heart_texts_time, date, 'sum(count)')
colnames(heart_texts_time) <- c("Date", "Number of Hearts")
#I love you over time-----------------------------------------------------------
ily_texts <- both_texts %>% filter(grepl("love you ", tolower(both_texts$text)))
ily_texts$date <- as.Date(as.character((ily_texts$date)))
ily_texts$count <- str_count(tolower(ily_texts$text), "love you ")
ily_texts_time <- ily_texts %>%  dplyr::group_by(date) %>% dplyr::summarise(sum(count))
ily_texts_time <- ily_texts_time %>% 
                      mutate(date = as.Date(date)) %>% 
                        complete(date = seq.Date(min(date), max(date), by='day'))
ily_texts_time$`sum(count)` <- replace_na(ily_texts_time$`sum(count)`, 0)
ily_texts_time <- select(ily_texts_time, date, 'sum(count)')
colnames(ily_texts_time) <- c("Date", "Number of ILY's")

#hehe---------------------------------------------------------------------------

hehe_num <- sum(grepl( "heh", harim_texts_noemoji$text))
hehe_data <- data.frame(hehe_num/(nrow(harim_texts_noemoji)), 
                       (nrow(harim_texts_noemoji)-hehe_num)/nrow(harim_texts_noemoji))
colnames(hehe_data) <- c("Contain hehe", "Dont Contain hehe")
hehe_data <- as.data.frame(t(hehe_data))
colnames(hehe_data) <- "n"

#haha lol-----------------------------------------------------------------------

haha_lol_num <- sum(grepl( "haha", betsy_texts$text)) + 
                sum(grepl( "lol", betsy_texts$text)) + 
                sum(grepl( "lmao", betsy_texts$text))
haha_lol_data <- data.frame(haha_lol_num/(nrow(betsy_texts)),
                            (nrow(betsy_texts)-haha_lol_num)/nrow(betsy_texts))
colnames(haha_lol_data) <- c("Contain haha, lol, lmao, :)", 
                             "Dont Contain haha, lol, lmao, :)")
haha_lol_data <- as.data.frame(t(haha_lol_data))
colnames(haha_lol_data) <- "n"

#images sent--------------------------------------------------------------------
images_sent <- both_texts %>% filter(grepl("\UFFFC", both_texts$text))
num_images_sent <- nrow(images_sent)


#messages sent by day by hour---------------------------------------------------
both_texts$weekday <- weekdays.POSIXt(both_texts$date)
both_texts$hour <- hour(both_texts$date)
sent_by_day_by_hour <- both_texts %>% plyr::ddply(c("hour", "weekday"), summarise, N = length(date))

sent_by_day_by_hour_temp <- matrix(0, ncol = 7, nrow = 24)
colnames(sent_by_day_by_hour_temp) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
rownames(sent_by_day_by_hour_temp) <- seq(0, 23)


for(i in 1:nrow(sent_by_day_by_hour)){
  hour <- sent_by_day_by_hour[i, "hour"]
  weekday <- sent_by_day_by_hour[i, "weekday"]
  sent_by_day_by_hour_temp[hour+1, weekday] <- sent_by_day_by_hour[i, 'N']
}

#separate hour and day----------------------------------------------------------
sent_by_hour <- aggregate(sent_by_day_by_hour$N, by=list(Category=sent_by_day_by_hour$hour), FUN=sum)
colnames(sent_by_hour) <- c("Hour", "N")

sent_by_day <- aggregate(sent_by_day_by_hour$N, by=list(Category=sent_by_day_by_hour$weekday), FUN=sum)
colnames(sent_by_day) <- c("Day", "N")

sent_by_day$Day <- factor(sent_by_day$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
sent_by_day <- sent_by_day[order(sent_by_day$Day),]


#total words--------------------------------------------------------------------

harim_words_sent <- sum(harim_texts_noemoji$text_length_word)
betsy_words_sent <- sum(betsy_texts_noemoji$text_length_word)

all_texts <- both_texts %>% select(date, is_from_me)
write.csv(all_texts, "data/all_texts.csv")


#text graphs--------------------------------------------------------------------
both_texts_over_time_p <- plot_ly(data = texts_by_day_harim,
                                  x = texts_by_day_harim$Date,
                                  y = texts_by_day_harim$"Number of Texts",
                                  type = "bar",
                                  name = "Harim")%>% 
                                  add_trace(y = texts_by_day_betsy$`Number of Texts`,
                                            marker = list(color = 'pink'), 
                                            name = "Betsy")%>% 
                                  layout(paper_bgcolor = 'transparent',
                                         plot_bgcolor = 'transparent',
                                         barmode = "stack",
                                         bargap = 0.5, 
                                         legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
                                         xaxis = list(title = "Date"),
                                         yaxis = list(title = "Number of Texts"),
                                         title = "Texts We've Sent")

export(both_texts_over_time_p, "images/both_texts.png")

harim_texts_over_time_p <- plot_ly(data = texts_by_day_harim,
                                   x = texts_by_day_harim$Date,
                                   y = texts_by_day_harim$"Number of Texts",
                                   type = "bar",
                                   name = "Number of Texts Over Time (Harim)")%>% 
                           layout(paper_bgcolor = 'transparent',
                                  plot_bgcolor = 'transparent',
                                  xaxis = list(title = "Date"),
                                  yaxis = list(title = "Number of Texts"),
                                  title = "Texts I've Sent")

export(harim_texts_over_time_p, "images/harim_texts.png")


betsy_texts_over_time_p <- plot_ly(data = texts_by_day_betsy,
                                   x = texts_by_day_betsy$Date,
                                   y = texts_by_day_betsy$"Number of Texts",
                                   type = "bar",
                                   name = "Number of Texts Over Time (Betsy)",
                                   marker = list(color = 'pink')) %>% 
                           layout(paper_bgcolor = 'transparent',
                                  plot_bgcolor = 'transparent',
                                  xaxis = list(title = "Date"),
                                  yaxis = list(title = "Number of Texts"),
                                  title = "Texts You've Sent")

export(betsy_texts_over_time_p, "images/betsy_texts.png")
#heart graphs===================================================================
heart_texts_time_p <- plot_ly(data = heart_texts_time,
                              x = heart_texts_time$Date,
                              y = heart_texts_time$"Number of Hearts",
                              type = 'bar') %>% 
                      layout(title = 'Number of Hearts (and Heart Emojis) in Texts',
                             xaxis = list(title = "Date",
                                                 showticklabels = TRUE),
                             yaxis = list(title = "Number of Messages With a Heart"))

export(heart_texts_time_p, "images/heart_texts.png")

ily_texts_time_p <- plot_ly(data = ily_texts_time,
                              x = as.Date(ily_texts_time$Date),
                              y = ily_texts_time$"Number of ILY's",
                              type = 'bar') %>% 
                    layout(title = "Number of I Love You's in Texts", 
                           xaxis = list(tickformat = '%d %B', 
                                        title = "Date"),
                           yaxis = list(title = 'Number of Messages with an "I love you"'))

export(ily_texts_time_p, "images/ily_texts.png")

#emoji graphs===================================================================
top_emoji_p <- plot_ly(x = seq(1, 10, 1),
                       y = top_emoji$times_sent,
                       type = "bar",
                       text = top_emoji$times_sent,
                       textposition = "auto",
                       marker = list(color = 'rgb(158,202,225)',
                                     line = list(color = 'rgb(8,48,107)', width = 1.5))) %>% 
                       layout(xaxis = list(title = "Emoji Rank",
                                           showticklabels = TRUE),
                              yaxis = list(title = "Times Used"),
                              title = "Top Emojis - Both")

#export(top_emoji_p, "images/top_emoji.png")


top_harim_emoji_p <- plot_ly(x = seq(1, 10),
                             y = top_harim_emoji$times_sent,
                             type = "bar",
                             text = top_harim_emoji$times_sent,
                             textposition = "auto",
                             marker = list(color = 'rgb(158,202,225)',
                                      line = list(color = 'rgb(8,48,107)', width = 1.5))) %>% 
                            layout(xaxis = list(title = "Emoji Rank",
                                                showticklabels = TRUE),
                                   yaxis = list(title = "Times Used"),
                                   title = "Top Emojis - Harim")

#export(top_harim_emoji_p, "images/top_emoji_harim.png")

top_betsy_emoji_p <- plot_ly(x = seq(1, 10),
                             y = top_betsy_emoji$times_sent,
                             type = "bar",
                             text = top_betsy_emoji$times_sent,
                             textposition = "auto",
                             marker = list(color = 'rgb(158,202,225)',
                                           line = list(color = 'rgb(8,48,107)', width = 1.5))) %>% 
                             layout(xaxis = list(title = "Emoji Rank",
                                                 showticklabels = TRUE),
                                    yaxis = list(title = "Times Used"),
                                    title = "Top Emojis - Betsy")

#export(top_betsy_emoji_p, "images/top_emoji_betsy.png")

#hehe haha graphs===============================================================
percentage_hehe_p <- plot_ly(hehe_data, labels = rownames(hehe_data), 
                             values = hehe_data[,1], type = "pie") %>% 
                     layout(title = "Percentage of 'hehe's' in Texts")

export(percentage_hehe_p, "images/percentage_hehe.png")


percentage_haha_lol_p <- plot_ly(haha_lol_data, labels = rownames(haha_lol_data), 
                                 values = haha_lol_data[,1], type = "pie") %>% 
                         layout(title = "Percentage of 'haha's', 'lol's', and 'lmao's' in Texts")
export(percentage_haha_lol_p, "images/percentage_haha.png")

#text heat map------------------------------------------------------------------
time <- c(12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
timeam <- paste(time, "am", sep = "")
timepm <- paste(time, "pm", sep = "")
time <- c(timeam, timepm)

vals <- unique(scales::rescale(c(volcano)))
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)

text_heatmap_p <- plot_ly(z = sent_by_day_by_hour_temp,
                          x = colnames(sent_by_day_by_hour_temp),
                          y = time,
                          colorscale = colz,
                          type = "heatmap") %>% 
                  layout(title = "Heatmap of Texts (Time vs Day)")

export(text_heatmap_p, "images/text_heatmap.png")


#time and day graphs------------------------------------------------------------
sent_by_day_p <- plot_ly(data = sent_by_day,
                         x = sent_by_day$Day,
                         y = sent_by_day$N,
                         type = "bar") %>% 
                 layout(xaxis = list(title = "Day"),
                        yaxis = list(title = "Number of Texts"),
                        title = "Texts Sent by Day")

export(sent_by_day_p, "images/sent_by_day.png")

sent_by_hour_p <- plot_ly(data = sent_by_hour,
                          x = sent_by_hour$Hour,
                          y = sent_by_hour$N,
                          type = "bar") %>% 
                  layout(xaxis = list(title = "Hour"),
                         yaxis = list(title = "Number of Texts"),
                         title = "Texts Sent by Hour")

export(sent_by_hour_p, "images/sent_by_hour.png")

end_time <- Sys.time()