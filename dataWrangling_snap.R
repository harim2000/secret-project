library("dplyr")
library("lubridate")
library("jsonlite")

all_messages1 <- fromJSON("data/snap chat data/json/chat_history.json")
all_messages2 <- fromJSON("data/snap chat data 2/json/chat_history.json")

received_messages <- all_messages1$`Received Chat History`
received_messages <- received_messages %>% filter(grepl("betsyymirandaa", tolower(received_messages$From)))
received_messages <- received_messages %>% arrange(Created)

received_messages2 <- all_messages2$`Received Chat History`
received_messages2 <- received_messages2 %>% filter(grepl("betsyymirandaa", tolower(received_messages2$From)))
received_messages2 <- received_messages2 %>% arrange(Created)

received_messages_all <- bind_rows(received_messages, received_messages2)
received_messages_all <- received_messages_all %>% distinct(Created, .keep_all = TRUE)

received_messages_all$is_from_me <- 0
received_messages_all <- received_messages_all %>% select(Created, is_from_me)
colnames(received_messages_all) <- c("date", "is_from_me")

num_messages_received <- nrow(received_messages_all)

sent_messages <- all_messages$`Sent Chat History`
sent_messages <- sent_messages %>% filter(grepl("betsyymirandaa", tolower(sent_messages$To)))

sent_messages2 <- all_messages2$`Sent Chat History`
sent_messages2 <- sent_messages2 %>% filter(grepl("betsyymirandaa", tolower(sent_messages2$To)))
sent_messages2 <- sent_messages2 %>% arrange(Created)

sent_messages_all <- bind_rows(sent_messages, sent_messages2)
sent_messages_all <- sent_messages_all %>% distinct(Created, .keep_all = TRUE)

sent_messages_all$is_from_me <- 1
sent_messages_all <- sent_messages_all %>% select(Created, is_from_me)
colnames(sent_messages_all) <- c("date", "is_from_me")

num_messages_sent <- nrow(sent_messages)


all_messages <- bind_rows(sent_messages_all, received_messages_all)
all_messages <- all_messages %>% arrange(date)
all_messages$date <- as.POSIXct(all_messages$date)
all_messages$date <- with_tz(all_messages$date, tzone="America/Los_Angeles")


write.csv(all_messages, "data/all_snap_messages.csv")



all_snaps <- fromJSON("data/snap chat data/json/snap_history.json")

received_snaps <- all_snaps$`Received Snap History`
received_snaps <- received_snaps %>% filter(grepl("betsyymirandaa", tolower(received_snaps$From)))

num_snaps_received <- nrow(received_snaps)


sent_snaps <- all_snaps$`Sent Snap History`
sent_snaps <- sent_snaps %>% filter(grepl("betsyymirandaa", tolower(sent_snaps$To)))

num_snaps_sent <- nrow(sent_snaps)
