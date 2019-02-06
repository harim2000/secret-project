library("dplyr")
library("plyr")


all_texts <- read.csv("data/all_texts.csv")
all_s_messages <- read.csv("data/all_snap_messages.csv")


all_texts <- all_texts %>% select(date, is_from_me)
all_s_messages <- all_s_messages %>% select(date, is_from_me)
all_messages <- bind_rows(all_texts, all_s_messages)

#get only dates where I have sc data too
all_messages <- all_messages %>% arrange(date)
all_messages <- all_messages %>% filter(date >= as.POSIXct("2018-12-14 19:59:58"))


all_messages$hour <- hour(all_messages$date)
all_messages$weekday <- weekdays.POSIXt(as.POSIXct(all_messages$date))

day_by_hour <- all_messages %>% plyr::ddply(c("hour", "weekday"), summarise, n = length(date))

day_by_hour_temp <- matrix(0, ncol = 7, nrow = 24)
colnames(day_by_hour_temp) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
rownames(day_by_hour_temp) <- seq(0, 23)


for(i in 1:nrow(day_by_hour)){
  hour <- day_by_hour[i, "hour"]
  weekday <- day_by_hour[i, "weekday"]
  day_by_hour_temp[hour+1, weekday] <- day_by_hour[i, 'n']
}

message_heatmap_p <- plot_ly(z = day_by_hour_temp,
                          x = colnames(day_by_hour_temp),
                          y = time,
                          colorscale = colz,
                          type = "heatmap")



all_messages$date <- as.Date(all_messages$date)
all_messages <- all_messages %>% dplyr::count(date)
