#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
library(rstanarm)

#### Read data ####
ces2020 <- read_parquet("data/analysis_data/cleaned_ces2020.parquet")

### Model data ####

# A graph of the data
ces2020 |>
  ggplot(aes(x = education, fill = voted_for)) +
  stat_count(position = "dodge") +
  facet_wrap(facets = vars(gender)) +
  theme_minimal() +
  labs(
    x = "Highest education",
    y = "Number of respondents",
    fill = "Voted for"
  ) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom")


# for simplicity and speed using a slice
ces2020_reduced <- 
  ces2020 |> 
  slice_sample(n = 1000)

political_preferences <-
  stan_glm(
    voted_for ~ gender + education,
    data = ces2020_reduced,
    family = binomial(link = "logit"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = 
      normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 8
  )

#### Save model ####
saveRDS(
  political_preferences,
  file = "models/reduced_voting_model.rds"
)


