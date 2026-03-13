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

felony_csv <- read_csv(
  'C:/Users/mzepp/Downloads/School/R/CES/UNMD0009_CES2024_felony.csv',
  show_col_types = FALSE
) 

CES_dta <- read_dta(
  'C:/Users/mzepp/Downloads/School/R/CES/CCES24_Common_OUTPUT_vv_topost_final.dta'
)
#CC24_pid7 - party 
#CC24_330a - ideology 

CES_combine <- merge(felony_csv, CES_dta, by = "caseid")|> 
  drop_na(
    felony, CC24_pid7, CC24_330a, # party, ideology 
    CC24_324c, CC24_324d, #abortion
    CC24_326c, CC24_326d, #environment 
    CC24_328c, CC24_328d, #tax
    CC24_323a, CC24_323c, #immigration
    CC24_321a, CC24_321b,#gun
  )


#recode the questions

  #republican abortion question
CES_combine <- CES_combine %>% 
  mutate(CC24_324c = case_when(
    CC24_324c == 1 ~ -1,
    CC24_324c == 2 ~ 1,
    TRUE ~ NA_real_
  ))

  #dem abortion question 
CES_combine <- CES_combine %>%
  mutate(CC24_324d = case_when(
    CC24_324d == 1 ~ 1,
    CC24_324d == 2 ~ -1,
    TRUE ~ NA_real_
  ))

  # dem environment
CES_combine <- CES_combine %>%
  mutate(CC24_326c = case_when(
    CC24_326c == 1 ~ 1,
    CC24_326c == 2 ~ -1,
    TRUE ~ NA_real_
  ))

  #rep environment 
CES_combine <- CES_combine %>%
  mutate(CC24_326d = case_when(
    CC24_326d == 1 ~ -1,
    CC24_326d == 2 ~ 1,
    TRUE ~ NA_real_
  ))


  #taxes rep 

CES_combine <- CES_combine %>%
  mutate(CC24_328c = case_when(
    CC24_328c == 1 ~ -1,
    CC24_328c == 2 ~ 1,
    TRUE ~ NA_real_
  ))

  #taxes dem 
CES_combine <- CES_combine %>%
  mutate(CC24_328d = case_when(
    CC24_328d == 1 ~ 1,
    CC24_328d == 2 ~ -1,
    TRUE ~ NA_real_
  ))



#immigration dem 

CES_combine <- CES_combine %>%
  mutate( CC24_323a = case_when(
    CC24_323a == 1 ~ 1,
    CC24_323a == 2 ~ -1,
    TRUE ~ NA_real_
  ))


#immigration rep
CES_combine <- CES_combine %>%
  mutate(CC24_323c = case_when(
    CC24_323c == 1 ~ -1,
    CC24_323c == 2 ~ 1,
    TRUE ~ NA_real_
  ))

#guns dem
CES_combine <- CES_combine %>%
  mutate(CC24_321a = case_when(
    CC24_321a == 1 ~ 1,
    CC24_321a == 2 ~ -1,
    TRUE ~ NA_real_
  ))

#guns rep
CES_combine <- CES_combine %>%
  mutate(CC24_321b = case_when(
    CC24_321b == 1 ~ -1,
    CC24_321b == 2 ~ 1,
    TRUE ~ NA_real_
  ))


#Reorder the variables so things make sense 

CES_combine <- CES_combine |>
  mutate(
    CC24_pid7 = factor(
      CC24_pid7,
      levels = c(
        "1",
        "2",
        "5",
        "7",
        "6",
        "4",
        "3",
        "8"
      ) ) )

CES_combine <- CES_combine %>%
  mutate(CC24_pid7 = case_when(
    CC24_pid7 == 1 ~ "1",
    CC24_pid7 == 2 ~ "2",
    CC24_pid7 == 5 ~ "3",
    CC24_pid7 == 7 ~ "4",
    CC24_pid7 == 6 ~ "5",
    CC24_pid7 == 4 ~ "6",
    CC24_pid7 == 3 ~ "7",
    CC24_pid7 == 8 ~ "8",
    TRUE ~ NA_character_
  )) 

party_binary <- CES_combine %>%
  filter(CC24_pid7 %in% c("1", "2", "3", "4" , "5", "6", "7")) %>%
  mutate(CC24_pid7 = case_when(
    CC24_pid7 == "1" ~ 0,
    CC24_pid7 ==  "2" ~ 0,
    CC24_pid7 == "3" ~ 0,
    CC24_pid7 == "4" ~ 1,
    CC24_pid7 ==  "5" ~ 2,
    CC24_pid7 == "6"~ 2,
    CC24_pid7 ==  "7" ~ 2
  ))



#Reorder the variables so things make sense 


ggplot(CES_combine, aes(x=CC24_pid7)) +
  geom_bar(fill = "blue") + 
  theme_minimal() 

CES_combine <- CES_combine %>%
  mutate(CC24_pid7 = case_when(
    CC24_pid7 == 1 ~ "Strong Democrat",
    CC24_pid7 == 2 ~ "Not very strong Democrat",
    CC24_pid7 == 3 ~ "Lean Democrat",
    CC24_pid7 == 4 ~ "Independent",
    CC24_pid7 == 5 ~ "Lean Republican",
    CC24_pid7 == 6 ~ "Not very strong Republican",
    CC24_pid7 == 7 ~ "Strong Republican",
    CC24_pid7 == 8 ~ "Not sure",
    TRUE ~ NA_character_
  )) 

CES_combine <- CES_combine %>%
  mutate(CC24_330a = case_when(
    CC24_330a == 1 ~ "Very Liberal",
    CC24_330a == 2 ~ "Liberal",
    CC24_330a == 3 ~ "Somewhat Liberal",
    CC24_330a == 4 ~ "Middle of the Road",
    CC24_330a == 5 ~ "Somewhat Conservative",
    CC24_330a == 6 ~ "Conservative",
    CC24_330a == 7 ~ "Very Conservative",
    CC24_330a == 8 ~ "Not sure",
    TRUE ~ NA_character_
  )) 
  
  
CES_party_id <- CES_combine %>%
  mutate(felony = case_when(
    felony == "yes" ~ 1, 
    felony == "no" ~ 0
  ))

ggplot(CES_combine, aes(x=CC24_pid7, y=CC24_330a )) +
  geom_point(fill = "blue") + 
  theme_minimal() 

table(CES_combine$CC24_330a)
table(CES_combine$CC24_pid7)

#1603 reported having been convicted of a felony

#CES_cleaned %>%
  #group_by(felony, newsint) %>%
  #summarize(n = n(), .groups = "drop") %>%
  #group_by(felony) %>%
  #mutate(percent = n / sum(n) * 100)

datasummary_crosstab(`party_scale` ~ `felony`, data = CES_cleaned, title ="Table 1")


datasummary_crosstab(`CC24_300b_5` ~ `felony`, data = CES_cleaned, title ="Table 1")
#watches fox


CES_party_id <- CES_combine %>%
  mutate(
    party_num = case_when(
      CC24_pid7 == "Strong Republican" ~ -3,
      CC24_pid7 == "Not very strong Republican" ~ -2,
      CC24_pid7 == "Lean Republican" ~ -1,
      CC24_pid7 == "Independent" ~ 0,
      CC24_pid7 == "Lean Democrat" ~ 1,
      CC24_pid7 == "Not very strong Democrat" ~ 2,
      CC24_pid7 == "Strong Democrat" ~ 3,
      TRUE ~ NA_real_
    ),
    
    ideology_num = case_when(
      CC24_330a == "Very Conservative" ~ -3,
      CC24_330a == "Conservative" ~ -2,
      CC24_330a == "Somewhat Conservative" ~ -1,
      CC24_330a == "Middle of the Road" ~ 0,
      CC24_330a == "Somewhat Liberal" ~ 1,
      CC24_330a == "Liberal" ~ 2,
      CC24_330a == "Very Liberal" ~ 3,
      TRUE ~ NA_real_
    )
  )

#graph for ideology and party with those who said they had a felony conviction marked as red

ggplot(CES_party_id, aes(x = ideology_num, y = party_num)) +
  # points colored by felony status
  geom_jitter(
    aes(color = factor(felony)),
    width = .25,
    height = .25,
    alpha = .4,
    size = 1.5
  ) +
  
  geom_hline(yintercept = 0, linewidth = 1.2) +
  geom_vline(xintercept = 0, linewidth = 1.2) +
  
  scale_color_manual(
    values = c("lightblue", "red"),
    labels = c("No felony conviction", "Felony conviction"),
    name = "Felony Status"
  ) +
  
  coord_cartesian(xlim = c(-3.5,3.5), ylim = c(-3.5,3.5)) +
  
  theme_minimal() +
  labs(
    x = "Ideology (Conservative → Liberal)",
    y = "Party Identification (Republican → Democrat)"
  )

geom_jitter(
  aes(color = factor(felony)),
  width = .25,
  height = .25,
  alpha = .6,
  size = ifelse(CES_combine$felony == 1, 2, 1)
)

# a bar chart cause it might look better then that crazy ass scatter plot

ggplot(CES_party_id, aes(x=CC24_330a, y=CC24_pid7, fill = felony)) +
  geom_col() +
  theme_minimal() +
  labs(
    x = "Ideology (Conservative → Liberal)",
    y = "Party Identification (Republican → Democrat)"
  ) 

mod <- lm(newsint ~ felony + CC24_421_1 + gender4 + race + hispanic + birthyr, data = party_binary)

summary(mod)

ggplot(CES_combine, aes(x = CC24_330a, y = CC24_pid7)) +
  geom_jitter(alpha = .6, width = .2, height = .2) +
  facet_wrap(~felony) +
  theme_minimal()

#look at the guns, environment, taxes, abortion, immigration - one question each - create a graph comparing self-identified ideology and actually ideology

#create a graph with conservative - liberal on one axis and dem-rep on the other (one color for having a felony)

#urban vs city 

# 