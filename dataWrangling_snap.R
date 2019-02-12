end_time_s <- Sys.time()

library("dplyr")
library("lubridate")
library("jsonlite")
library('plotly')

#messages ----------------------------------------------------------------------
#gets all data as json ---------------------------------------------------------
all_messages1 <- fromJSON("data/snap chat data/json/chat_history.json")
all_messages2 <- fromJSON("data/snap chat data 2/json/chat_history.json")
all_messages3 <- fromJSON("data/snap chat data 3/json/chat_history.json")


#recieved data------------------------------------------------------------------
#received 1 (12/14/18 - 1/11/19)
received_messages <- all_messages1$`Received Chat History`
received_messages <- received_messages %>% filter(grepl("betsyymirandaa", tolower(received_messages$From)))
received_messages <- received_messages %>% arrange(Created)

#received 2 (12/28/18 - 1/24/19)
received_messages2 <- all_messages2$`Received Chat History`
received_messages2 <- received_messages2 %>% filter(grepl("betsyymirandaa", tolower(received_messages2$From)))
received_messages2 <- received_messages2 %>% arrange(Created)

#received 3 (1/14/19 - 2/10/19)
received_messages3 <- all_messages3$`Received Chat History`
received_messages3 <- received_messages3 %>% filter(grepl("betsyymirandaa", tolower(received_messages3$From)))
received_messages3 <- received_messages3 %>% arrange(Created)

#merged all data tables
received_messages_all <- bind_rows(received_messages, received_messages2)
received_messages_all <- bind_rows(received_messages_all, received_messages3)
received_messages_all <- received_messages_all %>% distinct(Created, .keep_all = TRUE)

#adds a who sent it column: 0 = you 
received_messages_all$is_from_me <- 0
received_messages_all <- received_messages_all %>% select(Created, is_from_me)
colnames(received_messages_all) <- c("date", "is_from_me")

#counts the number of messages received (12/14 - 2/10)
num_messages_received <- nrow(received_messages_all)

#messages received per day
received_m_day <- received_messages_all
received_m_day$date <- as.Date(as.character(received_m_day$date))
received_m_day <- received_m_day %>% dplyr::count(date)
colnames(received_m_day) <- c('date', 'Number of Messages')

#sent data ---------------------------------------------------------------------
#sent 1 (12/14/18 - 1/11/19)
sent_messages <- all_messages1$`Sent Chat History`
sent_messages <- sent_messages %>% filter(grepl("betsyymirandaa", tolower(sent_messages$To)))

#sent 2 (12/28/18 - 1/24/19)
sent_messages2 <- all_messages2$`Sent Chat History`
sent_messages2 <- sent_messages2 %>% filter(grepl("betsyymirandaa", tolower(sent_messages2$To)))
sent_messages2 <- sent_messages2 %>% arrange(Created)

#sent 3 (1/14/19 - 2/10/19)
sent_messages3 <- all_messages3$`Sent Chat History`
sent_messages3 <- sent_messages3 %>% filter(grepl("betsyymirandaa", tolower(sent_messages3$To)))
sent_messages3 <- sent_messages3 %>% arrange(Created)


#merges all the sent data tables
sent_messages_all <- bind_rows(sent_messages, sent_messages2)
sent_messages_all <- bind_rows(sent_messages_all, sent_messages3)
sent_messages_all <- sent_messages_all %>% distinct(Created, .keep_all = TRUE)

#adds a who sent it column: 1 = me 
sent_messages_all$is_from_me <- 1
sent_messages_all <- sent_messages_all %>% select(Created, is_from_me)
colnames(sent_messages_all) <- c("date", "is_from_me")

#gets the number of messages sent
num_messages_sent <- nrow(sent_messages_all)


#messages sent per day
sent_m_day <- sent_messages_all
sent_m_day$date <- as.Date(as.character(sent_m_day$date))
sent_m_day <- sent_m_day %>% dplyr::count(date)
colnames(sent_m_day) <- c('date', 'Number of Messages')


#all snap messages -------------------------------------------------------------
all_messages <- bind_rows(sent_messages_all, received_messages_all)
all_messages <- all_messages %>% arrange(date) #arranges messages by date

#fixes dates to match timezone
all_messages$date <- as.POSIXct(all_messages$date)
all_messages$date <- with_tz(all_messages$date, tzone="America/Los_Angeles")

#exports data as csv
write.csv(all_messages, "data/all_snap_messages.csv")



#snaps--------------------------------------------------------------------------
#imports snap data from json
all_snaps <- fromJSON("data/snap chat data/json/snap_history.json")
all_snaps2 <- fromJSON("data/snap chat data 2/json/snap_history.json")
all_snaps3 <- fromJSON("data/snap chat data 3/json/snap_history.json")

#received 1 (12/14/18 - 1/11/19)
received_snaps <- all_snaps$`Received Snap History`
received_snaps <- received_snaps %>% filter(grepl("betsyymirandaa", tolower(received_snaps$From)))

#received 2 (12/28/18 - 1/24/19)
received_snaps2 <- all_snaps2$`Received Snap History`
received_snaps2 <- received_snaps2 %>% filter(grepl("betsyymirandaa", tolower(received_snaps2$From)))

#received 3 (1/14/19 - 2/10/19)
received_snaps3 <- all_snaps3$`Received Snap History`
received_snaps3 <- received_snaps3 %>% filter(grepl("betsyymirandaa", tolower(received_snaps3$From)))


#merged all data tables
received_snaps_all <- bind_rows(received_snaps, received_snaps2)
received_snaps_all <- bind_rows(received_snaps_all, received_snaps3)
received_snaps_all <- received_snaps_all %>% distinct(Created, .keep_all = TRUE)

#adds a who sent it column: 0 = you 
received_snaps_all$is_from_me <- 0
received_snaps_all <- received_snaps_all %>% select(Created, is_from_me)
colnames(received_snaps_all) <- c("date", "is_from_me")

#counts the number of snaps received (12/14 - 2/10)
num_snaps_received <- nrow(received_snaps_all)

#snaps sent per day
received_s_day <- received_snaps_all
received_s_day$date <- as.Date(as.character(received_s_day$date))
received_s_day <- received_s_day %>% dplyr::count(date)
colnames(received_s_day) <- c('date', 'Number of Snaps')


#sent data ---------------------------------------------------------------------
#sent 1 (12/14/18 - 1/11/19)
sent_snaps <- all_snaps$`Sent Snap History`
sent_snaps <- sent_snaps %>% filter(grepl("betsyymirandaa", tolower(sent_snaps$To)))

#sent 2 (12/28/18 - 1/24/19)
sent_snaps2 <- all_snaps2$`Sent Snap History`
sent_snaps2 <- sent_snaps2 %>% filter(grepl("betsyymirandaa", tolower(sent_snaps2$To)))
sent_snaps2 <- sent_snaps2 %>% arrange(Created)

#sent 3 (1/14/19 - 2/10/19)
sent_snaps3 <- all_snaps3$`Sent Snap History`
sent_snaps3 <- sent_snaps3 %>% filter(grepl("betsyymirandaa", tolower(sent_snaps3$To)))
sent_snaps3 <- sent_snaps3 %>% arrange(Created)


#merges all the sent data tables
sent_snaps_all <- bind_rows(sent_snaps, sent_snaps2)
sent_snaps_all <- bind_rows(sent_snaps_all, sent_snaps3)
sent_snaps_all <- sent_snaps_all %>% distinct(Created, .keep_all = TRUE)

#adds a who sent it column: 1 = me 
sent_snaps_all$is_from_me <- 1
sent_snaps_all <- sent_snaps_all %>% select(Created, is_from_me)
colnames(sent_snaps_all) <- c("date", "is_from_me")

#gets the number of snaps sent
num_snaps_sent <- nrow(sent_snaps_all)

#messages sent per day
sent_s_day <- sent_snaps_all
sent_s_day$date <- as.Date(as.character(sent_s_day$date))
sent_s_day <- sent_s_day %>% dplyr::count(date)
colnames(sent_s_day) <- c('date', 'Number of Snaps')


#all snap snaps -------------------------------------------------------------
all_snaps <- bind_rows(sent_snaps_all, received_snaps_all)
all_snaps <- all_snaps %>% arrange(date) #arranges snaps by date

#fixes dates to match timezone
all_snaps$date <- as.POSIXct(all_snaps$date)
all_snaps$date <- with_tz(all_snaps$date, tzone="America/Los_Angeles")

#exports data as csv
write.csv(all_snaps, "data/all_snap_snaps.csv")


#plots ---------------------------------- ---------------------------------------
#messages
messages_received_p <- plot_ly(received_m_day,
                               x = received_m_day$date,
                               y = received_m_day$`Number of Messages`,
                               name = 'Messages Received',
                               type = 'bar',
                               marker = list(color = 'pink')) %>% 
                       layout(xaxis = list(title = "Date"),
                              yaxis = list(title = "Number of Messages"),
                              title = "Messages You've Sent Through Snapchat")

export(messages_received_p, "images/received_m_snap.png")


messages_sent_p <- plot_ly(sent_m_day,
                           x = sent_m_day$date,
                           y = sent_m_day$`Number of Messages`,
                           name = 'Messages Sent',
                           type = 'bar') %>% 
                    layout(xaxis = list(title = "Date"),
                           yaxis = list(title = "Number of Messages"),
                           title = "Messages I've Sent Through Snapchat")

export(messages_sent_p, "images/sent_m_snap.png")

messages_all_p <- plot_ly(sent_m_day,
                          x = sent_m_day$date,
                          y = sent_m_day$`Number of Messages`,
                          name = 'Messages Sent',
                          type = 'bar') %>% 
                  add_trace(received_m_day,
                            x = received_m_day$date,
                            y = received_m_day$`Number of Messages`,
                            name = 'Messages Received',
                            type = 'bar',
                            marker = list(color = 'pink')) %>% 
                  layout(barmode = 'stack',
                         bargap = 0.5,
                         legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
                         xaxis = list(title = "Date"),
                         yaxis = list(title = "Number of Messages"),
                         title = "Messages We've Sent Through Snapchat")

export(messages_all_p, "images/all_m_snap.png")



snaps_received_p <- plot_ly(received_s_day,
                            x = received_s_day$date,
                            y = received_s_day$`Number of Snaps`,
                            name = 'Snaps Received',
                            type = 'bar',
                            marker = list(color = 'pink')) %>% 
                     layout(xaxis = list(title = "Date"),
                            yaxis = list(title = "Number of Snaps"),
                            title = "Snaps You've Sent Through Snapchat")

export(snaps_received_p, "images/received_s_snap.png")


snaps_sent_p <- plot_ly(sent_s_day,
                        x = sent_s_day$date,
                        y = sent_s_day$`Number of Snaps`,
                        name = 'Snaps Sent',
                        type = 'bar') %>% 
                layout(xaxis = list(title = "Date"),
                       yaxis = list(title = "Number of Snaps"),
                       title = "Snaps I've Sent Through Snapchat")

export(snaps_sent_p, "images/sent_s_snap.png")

snaps_all_p <- plot_ly(sent_s_day,
                       x = sent_s_day$date,
                       y = sent_s_day$`Number of Snaps`,
                       name = 'Snaps Sent',
                       type = 'bar') %>% 
               add_trace(received_s_day,
                         x = received_s_day$date,
                         y = received_s_day$`Number of Snaps`,
                         name = 'Snaps Received',
                         type = 'bar',
                         marker = list(color = 'pink')) %>% 
               layout(barmode = 'stack',
                      bargap = 0.5,
                      legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'), 
                      xaxis = list(title = "Date"),
                      yaxis = list(title = "Number of Snaps"),
                      title = "Snaps We've Sent Through Snapchat")

export(snaps_all_p, "images/all_s_snap.png")

start_time_s <- Sys.time()