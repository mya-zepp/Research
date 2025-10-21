
install.packages("usmap")
library(leaflet)
library(here)
library(tidyverse)
library(ggplot2)
library(broom)
library(dplyr)
library(usmap)
library(modelsummary)
library(poliscidata)
library(ggpattern)


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



prison_data <- read_csv(
  here::here("incarcerated_survey_2019.csv")
) |>
  drop_na()

prison_cleaned <- prison_data |>
  filter(!is.na(length_in_this_facility)) |>
  filter(!is.na(incarceration_impacts_motivation_to_vote)) 


prison_cleaned$length_in_this_facility <- ifelse(prison_cleaned$length_in_this_facility == "10 years or less", 0, 1)
#more time = 1

prison_cleaned$age <- ifelse(prison_cleaned$age == "36 or older", 1, 0)
#old = 1

prison_cleaned$identifies_as_black <-  ifelse(prison_cleaned$identifies_as_black == "TRUE", 1, 0)

prison_cleaned$identifies_as_white <-  ifelse(prison_cleaned$identifies_as_white == "TRUE", 1, 0)

prison_cleaned <- prison_cleaned |>
  mutate(
    how_often_officials_acting_in_your_interest = case_when(
      how_often_officials_acting_in_your_interest %in% c("Always", "Very Often") ~ 1,
      how_often_officials_acting_in_your_interest %in% c("Never", "Rarely") ~ 0,
      TRUE ~ NA_real_
    )
  )

prison_cleaned$identifies_as_hispanic_or_latinx <-  ifelse(prison_cleaned$identifies_as_hispanic_or_latinx == "TRUE", 1, 0)

prison_cleaned$ever_voted <-  ifelse(prison_cleaned$ever_voted == "Yes", 1, 0)

prison_cleaned <- prison_cleaned %>%
  mutate(incarceration_impacts_motivation_to_vote = case_when(
    incarceration_impacts_motivation_to_vote %in% c("Increased my motivation to vote", "Slightly increased my motivation to vote") ~ 1, incarceration_impacts_motivation_to_vote %in% c("No impact", "Decreased my motivation to vote",  "Slightly decreased my motivation to vote") ~ 0
  )) 

prison_cleaned <- prison_cleaned %>%
  filter(party %in% c("The Democratic party", "The Republican party", "Independent")) %>%
  mutate(party = case_when(
    party == "Independent" ~ 2,
    party == "The Democratic party" ~ 1,
    party == "The Republican party" ~ 0
  ))

unique(prison_cleaned$party)

mod <- lm(incarceration_impacts_motivation_to_vote ~ length_in_this_facility + ever_voted + identifies_as_hispanic_or_latinx + identifies_as_black + age + party, data = prison_cleaned)

##mod_1 <- lm(how_often_officials_acting_in_your_interest ~ length_in_this_facility + ever_voted + identifies_as_hispanic_or_latinx + identifies_as_black + age, data = prison_cleaned)

##summary(mod_1)


#tidy(mod)

modelsummary(mod,
             title = "Impact of Incarceration on Voting Motivation",
             coef_map = c(
               "length_in_this_facility" = "Length in Facility",
               "ever_voted" = "Voted in the past",
               #"identifies_as_hispanic_or_latinx" = "Hispanic/Latinx",
               # "identifies_as_black" = "Black",
               # "age" = "Age",
               "how_often_officials_acting_in_your_interest" = "Politicians are acting in your interest",
               "party" = "Party"
             ),
             stars = TRUE,
             gof_map = c("nobs", "r.squared")
             
)

prison_data <- read_csv(
  here::here("incarcerated_survey_2019.csv")
) |>
  drop_na()

prison_data <- prison_data |>
  mutate(
    how_often_officials_acting_in_your_interest = case_when(
      how_often_officials_acting_in_your_interest %in% c("Always", "Very Often") ~ 1,
      how_often_officials_acting_in_your_interest %in% c("Never", "Rarely") ~ 0,
      TRUE ~ NA_real_
    )
  ) |>
  drop_na(how_often_officials_acting_in_your_interest, length_in_this_facility) 

prison_data <- prison_data |>
  mutate(
    officials_acting_interest_label = factor(
      how_often_officials_acting_in_your_interest,
      levels = c(0, 1),
      labels = c("Never/Rarely", "Always/Very Often")
    )
  )

ggplot(prison_data, aes(x = officials_acting_interest_label)) +
  geom_bar(fill = "blue") +
  labs(
    title = "Trust in Political Officials",
    x = "How often are political officials acting in your interest?",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

prison_cleaned <- prison_cleaned %>%
  filter(party %in% c("The Democratic party", "The Republican party", "Independent")) %>%
  mutate(party = case_when(
    party == "Independent" ~ 2,
    party == "The Democratic party" ~ 1,
    party == "The Republican party" ~ 0
  ))


prison_data <- read_csv(
  here::here("incarcerated_survey_2019.csv")
) |>
  drop_na()

prison_cleaned <- prison_data |>
  filter(!is.na(length_in_this_facility)) |>
  filter(!is.na(incarceration_impacts_motivation_to_vote)) 


prison_cleaned$length_in_this_facility <- ifelse(prison_cleaned$length_in_this_facility == "10 years or less", 0, 1)
#more time = 1

prison_cleaned$age <- ifelse(prison_cleaned$age == "36 or older", 1, 0)
#old = 1

prison_cleaned$identifies_as_black <-  ifelse(prison_cleaned$identifies_as_black == "TRUE", 1, 0)

prison_cleaned$identifies_as_white <-  ifelse(prison_cleaned$identifies_as_white == "TRUE", 1, 0)

prison_cleaned <- prison_cleaned |>
  mutate(
    how_often_officials_acting_in_your_interest = case_when(
      how_often_officials_acting_in_your_interest %in% c("Always", "Very Often") ~ 1,
      how_often_officials_acting_in_your_interest %in% c("Never", "Rarely") ~ 0,
      TRUE ~ NA_real_
    )
  )

prison_cleaned$identifies_as_hispanic_or_latinx <-  ifelse(prison_cleaned$identifies_as_hispanic_or_latinx == "TRUE", 1, 0)

prison_cleaned$ever_voted <-  ifelse(prison_cleaned$ever_voted == "Yes", 1, 0)

prison_cleaned <- prison_cleaned %>%
  mutate(incarceration_impacts_motivation_to_vote = case_when(
    incarceration_impacts_motivation_to_vote %in% c("Increased my motivation to vote", "Slightly increased my motivation to vote") ~ 1, incarceration_impacts_motivation_to_vote %in% c("No impact", "Decreased my motivation to vote",  "Slightly decreased my motivation to vote") ~ 0
  )) 

prison_cleaned <- prison_cleaned %>%
  filter(party %in% c("The Democratic party", "The Republican party", "Independent")) %>%
  mutate(party = case_when(
    party == "Independent" ~ 2,
    party == "The Democratic party" ~ 1,
    party == "The Republican party" ~ 0
  ))

unique(prison_cleaned$party)

mod <- lm(incarceration_impacts_motivation_to_vote ~ length_in_this_facility + ever_voted + identifies_as_hispanic_or_latinx + identifies_as_black + age + party, data = prison_cleaned)

#mod_1 <- lm(how_often_officials_acting_in_your_interest ~ length_in_this_facility + ever_voted + identifies_as_hispanic_or_latinx + identifies_as_black + age, data = prison_cleaned)

summary(mod)


#tidy(mod)

modelsummary(mod,
             title = "Association Between Incarceration and Voting Motivation",
             coef_map = c(
               "length_in_this_facility" = "Length in Facility",
               "ever_voted" = "Voted in the past",
               #"identifies_as_hispanic_or_latinx" = "Hispanic/Latinx",
               "identifies_as_black" = "Black",
               # "age" = "Age",
               "how_often_officials_acting_in_your_interest" = "Politicians are acting in your interest",
               "party" = "Party"
             ),
             stars = TRUE,
             gof_map = c("nobs", "r.squared")
             
)


