library(sabRmetrics)
library(tidyverse)
library(httr)
#acknowledgement to Robert Frey's (https://github.com/robert-frey): Full Year Statcast Data in Minutes/statcast_full_season.R

start_date = "2025-03-27"
end_date = "2025-08-11"
dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")

df.savant <- download_baseballsavant(
  start_date,
  end_date,
  game_type = "R",
  cl = NULL,
  verbose = TRUE
)

df.mlbapi <- data_statsapi <- sabRmetrics::download_statsapi(
  start_date,
  end_date
)

mlb_pbp <- df.mlbapi[['pitch']] |> 
  select(game_id,at_bat_number = event_index, pitch_number, play_id) |> 
  mutate(at_bat_number = at_bat_number + 1)

savant_data <- left_join(df.savant, mlb_pbp,by=c("game_id","at_bat_number","pitch_number"))

#add video column
savant_data$video_url <- sapply(savant_data$play_id, sabRmetrics::get_video_url)

