library(ggplot2)
library(dplyr)
library(stringr)
library(patchwork)
library(lubridate)
library(tidyr)
library(baseballr)
library(DT)
library(plotly)
library(htmlwidgets)
#RETRIEVE SAVANT DATA FROM 'get-mlb-data.R'
savant2025 <- read.csv("")
# Filter Cease savant data
Cease_2025 <- savant2025 |> 
  filter(pitcher_id == 656302) |> 
  arrange(inning, outs)
#Convert release positions from feet to inches
Cease_2025 <- Cease_2025 |> 
  mutate(
    release_pos_x = release_pos_x * 12,  # Convert to inches
    release_pos_y = release_pos_y * 12,  # Convert to inches  
    release_pos_z = release_pos_z * 12   # Convert to inches
  )
#Cease fangraphs data
Cease_fangraphs <- fg_pitcher_game_logs(18525, 2025)
#organize fangraphs data
fangraphs_summary <- Cease_fangraphs |> 
  mutate(game_date = as.Date(Date)) |> 
  select(game_date, total_pitches = Pitches, innings_pitched = IP, GS = GSv2)
# Function to create boxplots with FF and SL only
create_ff_sl_boxplot <- function(data, position = "release_pos_x") {
  
  # Filter data for FF and SL only
  filtered_data <- data |> 
    filter(pitch_type %in% c("FF", "SL")) |> 
    filter(!is.na(!!sym(position))) |> 
    mutate(game_date = as.Date(game_date))
  
  # Calculate overall y-axis limits for standardization
  overall_min <- min(filtered_data[[position]], na.rm = TRUE)
  overall_max <- max(filtered_data[[position]], na.rm = TRUE)
  y_range <- overall_max - overall_min
  
  # Calculate FF and SL counts by game
  pitch_counts <- data |> 
    mutate(game_date = as.Date(game_date)) |> 
    filter(pitch_type %in% c("FF", "SL")) |> 
    group_by(game_date, pitch_type) |> 
    summarise(count = n(), .groups = 'drop') |> 
    pivot_wider(names_from = pitch_type, values_from = count, values_fill = 0) |> 
    rename(ff_count = FF, sl_count = SL)
  
  #boxplot labels
  game_labels <- fangraphs_summary |> 
    left_join(pitch_counts, by = "game_date") |> 
    mutate(
      ff_count = coalesce(ff_count, 0),
      sl_count = coalesce(sl_count, 0),
      label = paste0("IP: ", innings_pitched, "\nFF: ", ff_count, "\nSL: ", sl_count, "\nGS: ", round(GS, 2)),
      y_pos = overall_max + y_range * 0.05
    )
  
  # Define pitch type colors
  pitch_colors <- c("FF" = "orange", "SL" = "#1F78B4")
  
  # Create the plot
  p <- ggplot(filtered_data, aes(x = factor(game_date), y = !!sym(position), fill = pitch_type)) +
    geom_boxplot(aes(group = interaction(game_date, pitch_type)), 
                 outlier.color = "red", 
                 outlier.alpha = 0.6,
                 alpha = 0.7,
                 position = position_dodge(width = 0.8)) +
    geom_point(aes(color = pitch_type), 
               position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2),
               alpha = 0.4, size = 0.8) +
    geom_text(data = game_labels, 
              aes(x = factor(game_date), y = y_pos, label = label),
              vjust = 0, hjust = 0.5, size = 3, color = "black", fontface = "bold",
              inherit.aes = FALSE) +
    scale_fill_manual(values = pitch_colors, name = "Pitch Type") +
    scale_color_manual(values = pitch_colors, name = "Pitch Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          plot.title = element_text(size = 14, face = "bold"),
          panel.grid.minor = element_blank(),
          legend.position = "right") +
    labs(title = paste("Dylan Cease - Fastball (FF) and Slider (SL)", gsub("_", " ", str_to_title(position))),
         x = "Game Date",
         y = paste(gsub("_", " ", str_to_title(position)), "(inches)")) +
    scale_y_continuous(limits = c(overall_min - y_range * 0.02, overall_max + y_range * 0.20),
                       breaks = scales::pretty_breaks(n = 8))
  
  return(p)
}
ff_sl_plot_x <- create_ff_sl_boxplot(Cease_2025, "release_pos_x")
ff_sl_plot_x
ggsave("cease_release_plots_x.png", ff_sl_plot_x, 
       width = 16, height = 14, dpi = 600, bg = "white")
ff_sl_plot_y <- create_ff_sl_boxplot(Cease_2025, "release_pos_y")
ff_sl_plot_y
ff_sl_plot_z <- create_ff_sl_boxplot(Cease_2025, "release_pos_z")
ff_sl_plot_z
ggsave("cease_release_plots_z.png", ff_sl_plot_z, 
       width = 16, height = 8, dpi = 900, bg = "white")


#Make table
# Function to find median pitch videos
get_median_pitch_videos <- function(data, position = "release_pos_z") {
  
  median_pitches <- data |>
    filter(pitch_type %in% c("FF", "SL")) |>
    filter(!is.na(!!sym(position))) |>
    filter(!is.na(video_url)) |>  # Only include pitches with video URLs
    mutate(game_date = as.Date(game_date)) |>
    group_by(game_date, pitch_type) |>
    mutate(
      median_value = median(!!sym(position), na.rm = TRUE),
      distance_from_median = abs(!!sym(position) - median_value)
    ) |>
    # Find the pitch closest to median for each game/pitch type combination
    slice_min(distance_from_median, n = 1, with_ties = FALSE) |>
    mutate(
      # Create HTML hyperlinks for DT (and markdown for QMD)
      video_link_html = paste0('<span style="display: block; width: 100%;"><a href="', video_url, '" target="_blank" style="display: block; padding: 5px; text-decoration: none;">Watch Video</a></span>'),
      video_link_md = paste0('[Watch Video](', video_url, '){target="_blank"}')
    ) |>
    select(game_date, pitch_type, !!sym(position), median_value, 
           distance_from_median, video_link_html, video_link_md) |>
    ungroup() |>
    arrange(game_date, pitch_type)
  
  return(median_pitches)
}

# Get median pitch videos for release_pos_z
median_videos_z <- get_median_pitch_videos(Cease_2025, "release_pos_z")

# Create a QMD-ready formatted table with hyperlinks
create_video_table <- function(median_data, position_name = "Release Pos Z") {
  
  formatted_table <- median_data |>
    mutate(
      # Format the position values
      Position_Value = round(get(names(median_data)[3]), 3),
      Median_Value = round(median_value, 3),
      Distance_from_Median = round(distance_from_median, 4)
    ) |>
    select(
      `Game Date` = game_date,
      `Pitch Type` = pitch_type,
      !!paste0(position_name, " Value") := Position_Value,
      `Median Value` = Median_Value,
      `Distance from Median` = Distance_from_Median,
      `Video` = video_link_html
    )
  
  return(formatted_table)
}

# Create the table and display with data.table
video_table_z <- create_video_table(median_videos_z, "Release Pos Z")

datatable(video_table_z, 
          caption = "Cease: 2025 Median Vertical Release Position (inches)",
          extensions = 'Buttons',
          rownames = FALSE,
          escape = FALSE,  
          options = list(
            pageLength = 25,
            scrollX = TRUE,
            columnDefs = list(list(width = '120px', targets = 5)),
            dom = 'Bfrtip',
            buttons = c('copy', 'csv')
          ))

#create plotly for html viz. make manual tooltip adjustments
  p_with_tooltip <- ff_sl_plot_z + 
    guides(color = guide_legend(title = "Pitch Type")) +
    aes(text = paste("Game Date:", game_date,
                     "\nPitch Type:", pitch_type,
                     "\nRelease Pos Z:", round(release_pos_z, 2), "inches"))
  
  plotly_plot <- ggplotly(p_with_tooltip, tooltip = "text") %>%
    layout(showlegend = TRUE) %>%
    style(showlegend = TRUE, traces = c(1, 2)) %>%
    style(showlegend = FALSE, traces = c(3, 4))
  
  # Rename legend entries
  plotly_plot$x$data[[1]]$name <- "FF"
  plotly_plot$x$data[[2]]$name <- "SL"
