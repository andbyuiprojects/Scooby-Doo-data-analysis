# For the packages used to look at the data sets
library(tidyverse)
theme_set(theme_minimal())

# Looks at the data set fir Scooby Doo that tidy Tuesday
# had on that day
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-07-13')
scooby <- tuesdata$scoobydoo

# Quick look at the data in the set
glimpse(scooby)

# Fixes imdb ratings from char to number
scooby_sm <- scooby |> 
  select(series_name:format) |> 
  mutate(imdb = as.double(imdb))

# Initial graph of imdb ratings over time
ggplot(scooby_sm, aes(x = date_aired, y = imdb, color = format)) +
  geom_point() +
  # Makes the plots with collor for readability
  scale_color_brewer(palette = "Dark2")

# Looking at the format (crossovers, movies, TV series, etc...)
table(scooby_sm$format)

# Looking at just the crossovers
crossover <- filter(scooby_sm, format == "Crossover")
View(crossover)

# Looking at all the non-TV series episodes
scooby_sm <- scooby_sm |> 
  filter(format != "Crossover", format != "Movie", 
         format != "Movie (Theatrical)")
table(scooby_sm$format)

View(scooby_sm)

# Combining the segmented episodes for the consideration of the ratings 
# (to not skew the results)
segmented <- scooby_sm |> 
  filter(format == "TV Series (segmented)") |> 
  group_by(date_aired) |> 
  summarize(imdb = mean(imdb), network = unique(network),
            series_name = unique(series_name), total_runtime = sum(run_time))
View(segmented)

#Looking at the non-segmented episodes
non_seg <- scooby_sm |> 
  filter(format != "TV Series (segmented)") |> 
  select(date_aired,imdb, network, series_name, total_runtime = run_time)

scooby_tidied <- rbind(non_seg, segmented)
View(scooby_tidied)

# A new plot looking at just the shows on their networks, no movies
ggplot(scooby_tidied, aes(x = date_aired, y = imdb, col = network)) +
  geom_point() + scale_color_brewer(palette = "Dark2")

# Creates a bar chart for average imdb ratings for the various networks.
# Puts them in its order from lagest to smallest

scooby_tidied |> 
  group_by(network) |> 
  summarize(mean_imdb = mean(imdb, na.rm = TRUE)) |> 
  ggplot(aes(x = fct_reorder(network, -mean_imdb), y = mean_imdb, 
             fill = network)) + geom_col() +
  geom_text(aes(label = round(mean_imdb, 1)), nudge_y = .3) +
  labs(x = "Network", y = "Mean imdb rating", title = "Scooby doo") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 15))
