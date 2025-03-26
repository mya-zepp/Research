
install.packages("usmap")
library(leaflet)
library(here)
library(tidyverse)
library(ggplot2)
library(polisciols)
library(broom)
library(dplyr)
library(usmap)


prison_data <- read_csv(
  here::here("incarcerated_survey_2019.csv")
) |>
  drop_na() |>
  ##prison_data$length_binary <- ifelse(prison_data$length_in_this_facility > median(prison_data$length_in_this_facility, na.rm = TRUE), 1, 0)


#6 states - Arkansas, California, 
#Illinois, Kansas, Montana, and Maine

plot_usmap(fill = "grey20", color = "white")


disenfran_laws <- read_csv(
  here::here("disenfranchisement_laws_data.csv")
)

colnames(disenfran_laws) <- c("state", "value")

disenfran_laws$state <- tolower(disenfran_laws$state)


disenfran_laws$value <- as.factor(disenfran_laws$value)

plot_usmap(data = disenfran_laws, values = "value") +
  scale_fill_manual(
    values = c("orange", "red", "darkred", "purple", "darkblue", "blue"),
    labels = c(
      "No disenfranchisement",
      "Voting rights restored upon release",
      "Voting rights restored for those on probation or parole 
who have not been incarcerated in the last 5 years",
      "Voting rights restored after completion of sentence 
(including probation or parole)",
      "Permanent disenfranchisement for some",
      "Permanent disenfranchisement for all"
    )
  ) +
  theme_minimal() +  
  theme(
    legend.position = "right",
    panel.grid = element_blank(), 
    panel.background = element_blank(), 
    axis.ticks = element_blank(),  
    axis.text = element_blank()  
  ) +
  labs(
    title = "Disenfranchisement Laws in the United States",
    caption = "Source: Brennan Center for Justice, 2024"
  )

df <- read.csv("incarcerated_survey_2019.csv", stringsAsFactors = FALSE) |>
  drop_na()

df[df == "TRUE"] <- 1
df[df == "FALSE"] <- 0



