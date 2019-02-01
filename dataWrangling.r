library("dplyr")


all_texts <- read.csv("data/all_texts.csv")
all_s_messages <- read.csv("data/all_snap_messages.csv")


all_texts <- all_texts %>% select(date, is_from_me)
all_s_messages <- all_s_messages %>% select(date, is_from_me)
all_messages <- bind_rows(all_texts, all_s_messages)

#get only dates where I have sc data too
all_messages <- all_messages %>% arrange(date)
all_messages <- all_messages %>% filter(date >= as.POSIXct("2018-12-14 19:59:58"))


all_messages$hour <- hour(all_messages$date)
all_messages$weekday <- weekdays.POSIXt(all_messages$date)
