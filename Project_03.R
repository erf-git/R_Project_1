# Project 3
# Ethan R Feldman

#--- Introduction ---#

# We want to understand the music landscape in terms of music genres and audio features.
# 1. See the differences between main music genres.
# 2. See if there are any correlations between track audio features.


#--- Data Preprocessing ---#

# Import libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(GGally)
library(plotly)
library(gridExtra)
library(rstatix)


# Get the Data
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

# The original purpose of this dataset is to recommend Spotify users new songs using data gathered from their current song libraries ("playlists").
# The theory is that users will want to listen to songs similar to what they already listen too.
# What defines a song as "similar" to another depends on its features and how artists/users categorize them into playlists.
# This dataset contains features of artists discographies recorded by Spotify.

# This is a clean dataset, so no cleaning needs to be done.

# Condensed Dataset
head(spotify_songs)

# Summary of Variables
# ADD Data Dictionary FROM GITHUB
summary(spotify_songs)

# Make genre a factor
spotify_songs$playlist_genre = as.factor(spotify_songs$playlist_genre)

# I want to make key and mode into a Character instead of a double
keyChrValues = c("C", "C♯/D♭", "D", "E♭", "E","F", "F♯/G♭", "G", "A♭", "A", "B♭", "B/C♭")
modeChrValues = c("Minor", "Major")

spotify_songs <- spotify_songs %>%
  mutate(keyChr = case_when(key == 0 ~ keyChrValues[1],
                            key == 1 ~ keyChrValues[2],
                            key == 2 ~ keyChrValues[3],
                            key == 3 ~ keyChrValues[4],
                            key == 4 ~ keyChrValues[5],
                            key == 5 ~ keyChrValues[6],
                            key == 6 ~ keyChrValues[7],
                            key == 7 ~ keyChrValues[8],
                            key == 8 ~ keyChrValues[9],
                            key == 9 ~ keyChrValues[10],
                            key == 10 ~ keyChrValues[11],
                            key == 11 ~ keyChrValues[12],
                            TRUE ~ "Unknown"
                            ))
spotify_songs <- spotify_songs %>%
  mutate(modeChr = case_when(mode == 0 ~ modeChrValues[1],
                             mode == 1 ~ modeChrValues[2],
                            TRUE ~ "Unknown"
  ))

# I want to make a dataframe of just the float values for regression
spotify_songs_reg <- spotify_songs %>%
  select(danceability, 
         energy,
         loudness,
         speechiness,
         acousticness,
         valence, 
         tempo)

# For EDA purposes I'm going to define color before hand to make it look nice
pink <- "#FFADAD"
orange <-"#FFD6A5"
yellow <- "#FBFF8A"
green <- "#CAFFBF"
blue <- "#9BF6FF"
black <- "#656565"


#--- Key by Genre ---#

key_by_genre <- spotify_songs %>%
  select(playlist_genre, keyChr) %>%
  group_by(playlist_genre, keyChr) %>%
  mutate(n=n()) %>%
  unique() %>%
  group_by(keyChr) %>%
  mutate(total=sum(n)) %>%
  mutate(percent=round((n/total)*100))

head(key_by_genre, 10)

vis1 <- ggplot(key_by_genre, aes(x=keyChr, fill=playlist_genre, y = n, 
                                 text = paste("Number of Songs: ", n, "<br>",
                                              "Percent Songs in Key: ", percent, "%")))+
  geom_bar(width=0.5, stat = "identity")+
  scale_fill_manual(values=c(blue, yellow, pink, green, orange, black))+
  labs(x="Key", y="Number of Songs", fill="Genre")+
  theme_minimal()+
  ggtitle("Musical Key Makeup by Playlist")

ggplotly(vis1, tooltip=c("text"))


#--- Mode by Genre ---#

mode_by_genre <- spotify_songs %>%
  select(playlist_genre, modeChr) %>%
  group_by(playlist_genre, modeChr) %>%
  mutate(n=n()) %>%
  unique() %>%
  group_by(playlist_genre) %>%
  mutate(total=sum(n)) %>%
  mutate(percent=round((n/total)*100))

head(mode_by_genre, 10)

vis2 <- ggplot(mode_by_genre, aes(x=playlist_genre, fill=modeChr, y = n, 
                                 text = paste("Number of Songs: ", n, "<br>",
                                              "Percent Songs in Mode: ", percent, "%")))+
  geom_bar(position="fill", width=0.5, stat = "identity")+
  scale_fill_manual(values=c(orange, blue))+
  labs(x="Mode", y="Percent of Songs", fill="Genre")+
  theme_minimal()+
  ggtitle("Musical Mode Percentage by Playlist")

ggplotly(vis2, tooltip=c("text"))


#--- Danceability by Genre ---#

vis3 <- ggplot(spotify_songs, aes(x=danceability, fill=playlist_genre,
                            text = paste(playlist_genre)))+
  geom_density(alpha=0.7, color=NA)+
  scale_fill_manual(values=c(blue, yellow, pink, green, orange, black))+
  labs(x="Danceability", y="Density", fill="Genre") +
  theme_minimal()+
  ggtitle("Distribution of Danceability Data")

ggplotly(vis3, tooltip=c("text"))

# Anova test for Danceability by Genre distributions
anova_danceability <- spotify_songs %>% anova_test(danceability ~ playlist_genre)
anova_danceability

# Boxplot for Danceability by Genre distributions
vis3_B <- ggplot(spotify_songs, aes(x=playlist_genre, y=danceability, color=playlist_genre))+
  scale_color_manual(values=c(blue, yellow, pink, green, orange, black))+
  labs(x="Genre", y="Danceability")+
  geom_boxplot()+
  ggtitle("Boxplot Distribution of Danceability Data")
ggplotly(vis3_B)

# Rock is significantly different to Rap and Latin
# Edm and Pop are very similarly distributed
# Rap contains the highest danceability

#--- Speechiness by Genre ---#

vis4 <- ggplot(spotify_songs, aes(x=speechiness, ..scaled.., color=playlist_genre,
                                  text = paste(playlist_genre)))+
  geom_density()+
  scale_color_manual(values=c(blue, yellow, pink, green, orange, black))+
  xlim(c(0, 0.4))+
  labs(x="Speechiness", y="Density", fill="Genre") +
  theme_minimal()+
  ggtitle("Distribution of Speechiness Data")

ggplotly(vis4, tooltip=c("text"))

# Anova test for Speechiness by Genre distributions
anova_speechiness <- spotify_songs %>% anova_test(speechiness ~ playlist_genre)
anova_speechiness

# Boxplot for Speechiness by Genre distributions
vis4_B <- ggplot(spotify_songs, aes(x=playlist_genre, y=speechiness, color=playlist_genre))+
  scale_color_manual(values=c(blue, yellow, pink, green, orange, black))+
  ylim(c(0, 0.4))+
  labs(x="Genre", y="Speechiness")+
  geom_boxplot()+
  ggtitle("Boxplot Distribution of Speechiness Data")
ggplotly(vis4_B)

# Rock is significantly different to Rap
# Edm and Pop are very similarly distributed
# Latin and R&B are very similarly distributed
# Rap contains the highest speechiness


#--- Valence by Genre ---#

vis5 <- ggplot(spotify_songs, aes(x=valence, color=playlist_genre, 
                                  text = paste(playlist_genre)))+
  geom_density()+
  scale_color_manual(values=c(blue, yellow, pink, green, orange, black))+
  labs(x="Valence", y="Density", fill="Genre") +
  theme_minimal()+
  ggtitle("Distribution of Valence Data")

ggplotly(vis5, tooltip=c("text"))

# Anova test for Valence by Genre distributions
anova_danceability <- spotify_songs %>% anova_test(valence ~ playlist_genre)
anova_danceability

# Boxplot for Valence by Genre distributions
vis5_B <- ggplot(spotify_songs, aes(x=playlist_genre, y=valence, color=playlist_genre))+
  scale_color_manual(values=c(blue, yellow, pink, green, orange, black))+
  labs(x="Genre", y="Valence")+
  geom_boxplot()+
  ggtitle("Boxplot Distribution of Valence Data")
ggplotly(vis5_B)

# Edm is significantly different to Latin
# All groups seems to share similar distributions in valence


#--- Correlation by Genre ---#
pairs(spotify_songs_reg, col = pink)

# It seems like there might be a correlation between Energy and Loudness, Loudness and Speechiness, and Danceability and Speechiness, Danceability and Loudness

vis6 <- ggplot(spotify_songs, aes(x=energy, y=loudness))+
  labs(x="Energy (0-1 Scale)", y="Loudness (-60-0 db Scale)", fill="Loudness by Energy")+
  geom_point(size = 2, color=pink)+
  geom_smooth(method='lm')

vis7 <- ggplot(spotify_songs, aes(x=loudness, y=speechiness))+
  labs(x="Loudness (-60-0 db Scale)", y="Speechiness (0-1 Scale)", fill="Speechiness by Loudness")+
  geom_point(size = 2, color=blue)+
  geom_smooth(method='lm')

vis8 <- ggplot(spotify_songs, aes(x=danceability, y=speechiness))+
  labs(x="Danceability (0-1 Scale)", y="Speechiness (0-1 Scale)", fill="Speechiness by Danceability")+
  geom_point(size = 2, color=orange)+
  geom_smooth(method='lm')

vis9 <- ggplot(spotify_songs, aes(x=danceability, y=loudness))+
  labs(x="Danceability (0-1 Scale)", y="Loudness (-60-0 db Scale)", fill="Loudness by Danceability")+
  geom_point(size = 2, color=green)+
  geom_smooth(method='lm')

grid.arrange(vis6, vis7, vis8, vis9, ncol=2)

# Upon closer inspection it seems like there might be a linear relation between Loudness and Energy, but no linear relationship between the others.
# The shape of the graphs do indicate that something is going on, but it's definitely not a linear relationship.


