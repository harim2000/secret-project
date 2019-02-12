library("dplyr")
library("plyr")


all_texts <- read.csv("data/all_texts.csv")
all_s_messages <- read.csv("data/all_snap_messages.csv")
all_s_snaps <- read.csv('data/all_snap_snaps.csv')

num_messages_sent <- nrow(all_texts) + nrow(all_s_messages) + nrow(all_s_snaps)


all_texts <- all_texts %>% select(date, is_from_me)
all_s_messages <- all_s_messages %>% select(date, is_from_me)
all_s_snaps <- all_s_snaps %>% select(date, is_from_me)
all_messages <- bind_rows(all_texts, all_s_messages)
all_messages <- bind_rows(all_messages, all_s_snaps)

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

time <- c(12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
timeam <- paste(time, "am", sep = "")
timepm <- paste(time, "pm", sep = "")
time <- c(timeam, timepm)

vals <- unique(scales::rescale(c(volcano)))
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)

message_heatmap_p <- plot_ly(z = day_by_hour_temp,
                          x = colnames(day_by_hour_temp),
                          y = time,
                          colorscale = colz,
                          type = "heatmap") %>% 
                     layout(title = "Message Heatmap of Texts and Snaps")

export(message_heatmap_p, 'images/all_heatmap.png')

all_messages$date <- as.Date(all_messages$date)
all_messages <- all_messages %>% dplyr::count(date)


messages_date <- plot_ly(all_messages,
                         x = all_messages$date,
                         y = all_messages$n,
                         type = 'bar') %>% 
                  layout(xaxis = list(title = "Date"),
                         yaxis = list(title = "Number Sent"),
                         title = "iMessage and Snapchat Data")

export(messages_date, 'images/all_messages.png')
