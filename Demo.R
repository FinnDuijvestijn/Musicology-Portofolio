library(tidyverse)
library(spotifyr)
library("viridis")           # Load

Sys.setenv(SPOTIFY_CLIENT_ID="34c9d0d698064bf886a78b343db5445b")
Sys.setenv(SPOTIFY_CLIENT_SECRET="d0e93595f1ee411bb21938c5cb50d247")

access_token <- get_spotify_access_token()

pop <- get_playlist_audio_features("", "2AipD5zTjWWM13wk2hZmej")
phonk <- get_playlist_audio_features("", "37i9dQZF1DWWY64wDtewQt")
rap <- get_playlist_audio_features("", "37i9dQZF1DX76t638V6CA8")

awards <-
  bind_rows(
    pop |> mutate(category = "Pop"),
    phonk |> mutate(category = "Phonk"),
    rap |> mutate(category = "Rap")
    
  )

pop |> ggplot(aes(x = energy)) + geom_histogram(binwidth = 0.1)

rap |> ggplot(aes(x = valence)) + geom_density()

awards |>
  ggplot(aes(x = energy)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~category)

awards |>
  ggplot(aes(x = category, y = energy)) +
  geom_boxplot()

awards |>
  ggplot(aes(x = category, y = energy)) +
  geom_violin()

rap |> ggplot(aes(x = valence, y = energy)) + geom_point() + geom_smooth()

rap |> ggplot(aes(x = valence, y = energy)) + geom_jitter() + geom_smooth()

awards |>                    # Start with awards.
  mutate(
    mode = ifelse(mode == 0, "Minor", "Major")
  ) |>
  ggplot(                     # Set up the plot.
    aes(
      x = valence,
      y = energy,
      size = loudness,
      colour = mode
    )
  ) +
  geom_point() +              # Scatter plot.
  geom_rug(linewidth = 0.1) + # Add 'fringes' to show data distribution.
 
  facet_wrap(~ category) +    # Separate charts per playlist.
  scale_x_continuous(         # Fine-tune the x axis.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),   # Use grid-lines for quadrants only.
    minor_breaks = NULL       # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(         # Fine-tune the y axis in the same way.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),
    minor_breaks = NULL
  ) +
  scale_colour_brewer(        # Use the Color Brewer to choose a palette.
    type = "qual",            # Qualitative set.
    palette = "Paired"        # Name of the palette is 'Paired'.
  ) +
  scale_size_continuous(      # Fine-tune the sizes of each point.
    trans = "exp",            # Use an exp transformation to emphasise loud.
    guide = "none"            # Remove the legend for size.
  ) +
  theme_light() +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Valence",
    y = "Energy",
    colour = "Mode"
  )









awards |>                    # Start with awards.
  ggplot(                     # Set up the plot.
    aes(
      x = valence,
      y = energy,
      size = loudness,
      colour = energy
    )
  ) +
  geom_point() +              # Scatter plot.
  geom_rug(linewidth = 0.2) + # Add 'fringes' to show data distribution.

facet_wrap(~ category) +    # Separate charts per playlist.
  scale_x_continuous(         # Fine-tune the x axis.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),   # Use grid-lines for quadrants only.
    minor_breaks = NULL       # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(         # Fine-tune the y axis in the same way.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),
    minor_breaks = NULL
  ) +
  # scale_colour_brewer(        # Use the Color Brewer to choose a palette.
  #   type = "qual",            # Qualitative set.
  #   palette = "Paired"        # Name of the palette is 'Paired'.
  # ) +
  scale_size_continuous(      # Fine-tune the sizes of each point.
    trans = "exp",            # Use an exp transformation to emphasise loud.
    guide = "none"            # Remove the legend for size.
  ) +
  theme_light() +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Valence",
    y = "Energy",
  )


awards |>
  ggplot(aes(x = key_name, fill = key_name)) +
  geom_bar() +
  facet_wrap(~category) +
  ggtitle("Distribution of keys per genre")


awards |>
  ggplot(aes(x = category, y = danceability, fill = category)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette="PuRd")

ggplot(iris, aes(Sepal.Length, Sepal.Width))+
  geom_point(aes(color = Sepal.Length)) +
  scale_color_viridis(option = "A")+
  theme_minimal() +
  theme(legend.position = "bottom")  

ggplot(mpg, aes(x=class, y=hwy, fill=class)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")
