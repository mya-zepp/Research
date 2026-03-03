library(haven)
library(dplyr)
library(mvtnorm)  
library(gtsummary)
library(janitor)
library(broom)  
library(broom.helpers)
library(gt)
library(tidyr)
library(tibble)
library(foreign)
library(usmap)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(modelsummary)
library(ggpattern)
library(mvtnorm)

CES_data <- read_csv(
  "C:/Users/mzepp/Downloads/School/R/CES/CCES24_Common_OUTPUT_vv_topost_final.csv",
  show_col_types = FALSE
) |>
  drop_na(
    felony, party_scale, ideology_scale, 
    CC24_324a, CC24_324b, CC24_324c, CC24_324d,
    CC24_326a, CC24_326b, CC24_326c, CC24_326d, CC24_326e, CC24_326f,
    CC24_328a, CC24_328b, CC24_328c, CC24_328d, CC24_328e, CC24_323f,
    CC24_323a, CC24_323b, CC24_323c, CC24_323d,
    CC24_321a, CC24_321b, CC24_321c, CC24_321d, CC24_321e, CC24_321f
  )

#CC24_pid7 - party 
#CC24_330a - ideology 

ggplot(CES_cleaned, aes(x=CC24_pid7)) +
  geom_bar(fill = "blue") +   
  theme_minimal() +
  theme(legend.position = "top")

CES_cleaned <- CES_data %>%
  mutate(party_scale = case_when(
    party_scale == 1 ~ "Strong Democrat",
    party_scale == 2 ~ "Not very strong Democrat",
    party_scale == 3 ~ "Lean Democrat",
    party_scale == 4 ~ "Independent",
    party_scale == 5 ~ "Lean Republican",
    party_scale == 6 ~ "Not very strong Republican",
    party_scale == 7 ~ "Strong Republican",
    party_scale == 8 ~ "Not sure",
    TRUE ~ NA_character_
  )) 
  
  
CES_cleaned <- CES_cleaned %>%
  mutate(felony = case_when(
    felony == "yes" ~ 1, 
    felony == "no" ~ 0
  ))

CES_cleaned <- janitor::clean_names(CES_cleaned)

colnames(CES_cleaned)

table(CES_cleaned$felony)

#1603 reported having been convicted of a felony

CES_cleaned %>%
  group_by(felony, CC24_pid7) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(felony) %>%
  mutate(percent = n / sum(n) * 100)

datasummary_crosstab(`CC24_pid7` ~ `felony`, data = CES_cleaned, title ="Table 1")



ggplot(CES_cleaned, aes(x=CC24_324a)) +
  geom_bar(fill = "blue") +   
  theme_minimal() +
  theme(legend.position = "top")




