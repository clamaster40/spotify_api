
id <- 'xxxxxxxxxxxxxxxxxxxxx'
secret <- 'xxxxxxxxxxxxxxxxxxxxx'

###API ACCESS###

Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)


access_code <- get_spotify_authorization_code

###TOKEN###

Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)


access_token <- get_spotify_access_token()


###TOP 50 TRACKS###

top_tracks <- get_my_top_artists_or_tracks(
  type = "tracks",
  limit = 50,
  offset = 0,
  time_range = "long_term",
  authorization = get_spotify_authorization_code(
    client_id = id,
    client_secret = secret,
    scope = 'user-top-read'
  ),
  include_meta_info = FALSE
)

###ARTIST NAME###

k <- 1

for(k in 1:50){
  top_tracks$artists[k] <- top_tracks$artists[[k]]$name
}

###SUBSET TOP TRACKS###

top_tracks <- subset(top_tracks, select = c(name, artists, id, duration_ms, explicit, album.name, album.release_date, popularity))


###AUDIO FEATURES###

audio_feat_list <- paste(top_tracks$id[1:50], collapse = ',')


audio_feat_top_tracks <- get_track_audio_features(
  audio_feat_list,
  authorization = get_spotify_access_token(
    client_id = id,
    client_secret = secret
  )
)


###JOIN AUDIO FEATURES AND TOP TRACKS###

audio_feat_top_tracks <- left_join(audio_feat_top_tracks, top_tracks, by = 'id')

###PITCH NOTATION###

pitch_mapping <- data.frame(
  key = 0:11,
  lettered_notation = c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
)


pitch_mapping$key <- as.character(pitch_mapping$key)


audio_feat_top_tracks$key <- as.character(audio_feat_top_tracks$key)


audio_feat_top_tracks <- audio_feat_top_tracks %>%
  left_join(pitch_mapping, by = c("key" = "key"))


colnames(audio_feat_top_tracks)[colnames(audio_feat_top_tracks) == "lettered_notation"] <- "key"


audio_feat_top_tracks <- select(audio_feat_top_tracks, -3)

###MILISECONDS TO MINUTES###

audio_feat_top_tracks <- audio_feat_top_tracks %>% select(-duration_ms.y)


as.POSIXct(Sys.Date()) + audio_feat_top_tracks$duration_ms.x/1000
format(as.POSIXct(Sys.Date()) + audio_feat_top_tracks$duration_ms.x/1000, "%M:%S")


audio_feat_top_tracks$track_length <- as.POSIXct(Sys.Date()) + audio_feat_top_tracks$duration_ms.x/1000
audio_feat_top_tracks$track_length <- format(audio_feat_top_tracks$track_length, "%M:%S")

###INSTSTRUMENTALNESS TO DECIMAL FROM SCIENTIFIC###

audio_feat_top_tracks$instrumentalness <- format(audio_feat_top_tracks$instrumentalness, scientific = FALSE)

###ROUNDING COLUMN VALUES###

audio_feat_top_tracks[, c("danceability", "energy", "loudness", "speechiness", "acousticness", "liveness", "valence", "tempo")] <- round(audio_feat_top_tracks[, c("danceability", "energy", "loudness", "speechiness", "acousticness", "liveness", "valence", "tempo")], 2)


###REMOVAL###

audio_feat_top_tracks <- audio_feat_top_tracks %>%
  select(-c(11:16))