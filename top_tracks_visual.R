
###COLUMN CAPITALIZATION###

colnames(audio_feat_top_tracks) <- sapply(colnames(audio_feat_top_tracks), function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = ""))


###KEY FREQUENCY###

key_bar_chart <- ggplot(audio_feat_top_tracks, aes(x = Key)) +
  geom_bar(fill = "#66ff85", alpha = 1) +
  labs(title = "Number of Songs in Each Key", x = "Key", y = "Number of Songs") +
  theme_minimal()

ggsave("key_bar_chart.png", plot = key_bar_chart, width = 8, height = 6, dpi = 300)


###VALENCE###

colnames(audio_feat_top_tracks)[colnames(audio_feat_top_tracks) == "Artists"] <- "Artist"


positive_valence <- audio_feat_top_tracks %>% 
  arrange(Valence) %>% 
  select(Name, Artist, Valence) %>% 
  head(5) %>% 
  kable() %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position") %>%
  add_header_above(c("Top 5 Tracks with Lowest Valence" = 3)) %>%
  row_spec(row = 1:2, background = "#66ff85")


negative_valence <- audio_feat_top_tracks %>% 
  arrange(-Valence) %>% 
  select(Name, Artist, Valence) %>% 
  head(5) %>% 
  kable() %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position") %>%
  add_header_above(c("Top 5 Tracks with Highest Valence" = 3)) %>%
  row_spec(row = 1, background = "#66ff85")


audio_feat_top_tracks$Valence <- as.numeric(audio_feat_top_tracks$Valence)


valence_histo <- ggplot(audio_feat_top_tracks, aes(x = Valence)) +
  geom_histogram(fill = "#66ff85", bins = 15, alpha = 1.0) +
  labs(title = "Distribution of Song Valences",
       x = "Valence",
       y = "Frequency") +
  theme_minimal()

ggsave("valence_histo.png", plot = valence_histo, width = 8, height = 6, dpi = 300)


###NUMERIC MEANS###

  ###CREATE MEAN DF###

column_means <- audio_feat_top_tracks %>% 
    summarise(across(where(is.numeric),mean))


column_means <- column_means %>%
  mutate_at(vars(1:3, 5:9, 11), ~round(., 2))

  ###TRACK_LENGTH TRANSFORMATION

audio_feat_top_tracks$Length_seconds <- as.numeric(ms(audio_feat_top_tracks$Track_length))


length_mean <- mean(audio_feat_top_tracks$Length_seconds, na.rm = TRUE)


period_mean <- seconds_to_period(length_mean)

minutes <- minute(period_mean)
seconds <- second(period_mean)


mean_length_formatted <- sprintf("%02d:%02d", as.integer(minutes), as.integer(seconds))


Duration <- data.frame(Category = "Duration", Value = mean_length_formatted)


column_means$Duration <- mean_length_formatted

  ###MEAN VALUES VISUALIZATION###

column_means <- mutate_all(column_means, as.character)


colnames(column_means)[colnames(column_means) == "Time_signature"] <- "Time Signature"


column_means <- column_means %>%
  select(-Length_seconds)


column_means_long <- pivot_longer(column_means, everything(), names_to = "Feature", values_to = "Mean")


means_table <- column_means_long %>%
  kable(format = "html") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position") %>%
  add_header_above(c("Audio Features Value Means" = 2)) 

column_means %>%
  kable(format = "html") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position") %>%
  add_header_above(c("Audio Feature Value Means" = 12)) 


###DUMBBELL PLOT###

green_palette <- c("#00b224", "#00d62b", "#66ff85")


dumbell_plot <- audio_feat_top_tracks %>%
  gather(key = "variable", value = "value", Danceability, Energy, Speechiness) %>%
  mutate(color = factor(variable, levels = c("Danceability", "Energy", "Speechiness")),
         color = green_palette[as.integer(color)]) %>%
  ggplot(aes(x = value, y = variable, group = variable, color = color)) +
  geom_point(size = 3) +
  geom_segment(aes(x = 0, xend = 1, yend = variable), linetype = "dashed", size = 1) +
  scale_color_identity() +
  labs(title = "Musical Attribute Chart",
       x = "Value",
       y = "Variable") +
  theme_minimal() +
  theme(legend.title = element_blank())

ggsave("dumbell_plot.png", plot = dumbell_plot, width = 8, height = 6, dpi = 300)


###TOP 10 ARTISTS###

artists <- get_my_top_artists_or_tracks(
  type = "artists",
  limit = 10,
  offset = 0,
  time_range = "long_term",
  authorization = get_spotify_authorization_code(
    client_id = id,
    client_secret = secret,
    scope = 'user-top-read'
  ),
  include_meta_info = FALSE
)

colnames(artists) <- sapply(colnames(artists), function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = ""))

artists$Genre <- sapply(artists$Genres, function(genres) genres[1])

capitalize_first_letter <- function(x) {
  tools::toTitleCase(x)
}

artists$Genre <- sapply(artists$Genre, capitalize_first_letter)

ten_artists <- artists %>% 
  select(Name, Genre, Popularity) %>% 
  head(10) %>% 
  kable() %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position") %>%
  add_header_above(c("Top 10 Artists" = 3)) %>%
  row_spec(row = 1, background = "#66ff85")


###TOP 10 SONGS###

tracks <- get_my_top_artists_or_tracks(
  type = "tracks",
  limit = 10,
  offset = 0,
  time_range = "long_term",
  authorization = get_spotify_authorization_code(
    client_id = id,
    client_secret = secret,
    scope = 'user-top-read'
  ),
  include_meta_info = FALSE
)

u <- 1

for(u in 1:10){
  tracks$artists[u] <- tracks$artists[[u]]$name
}

colnames(tracks) <- sapply(colnames(tracks), function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = ""))

ten_tracks <- tracks %>% 
  select(Name, Artists ) %>% 
  head(10) %>% 
  kable() %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position") %>%
  add_header_above(c("Top 10 Songs" = 2)) %>%
  row_spec(row = 1, background = "#66ff85")

