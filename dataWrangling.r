library("dplyr")
library("jsonlite")

all_texts <- fromJSON("data/message_data.json")

betsy_texts <- select(all_texts, date, handle_id, is_from_me, is_emote, item_type, payload_data, text) %>% filter(handle_id == 152)
print(betsy_texts)
