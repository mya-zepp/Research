#packages
library(haven)
library(dplyr)
library(mvtnorm)  
library(gtsummary)
library(broom)  
library(gt)
library(tidyr)
library(tibble)
library(foreign)
library(usmap)
library(leaflet)
suppressPackageStartupMessages(library(tidyverse))
library(ggplot2)
library(modelsummary)
library(ggpattern)
suppressPackageStartupMessages(library(mvtnorm))


#load data and clean the variables of interest

prison_data <- read_csv(
  "C:/Users/mzepp/Downloads/School/R/Research/incarcerated_survey_2019.csv",
  show_col_types = FALSE
) |>
  drop_na()


prison_cleaned <- prison_data |>
  filter(!is.na(length_in_this_facility)) |>
  filter(!is.na(incarceration_impacts_motivation_to_vote)) 



prison_cleaned$length_in_this_facility <- ifelse(prison_cleaned$length_in_this_facility == "10 years or less", 0, 1)
#more time = 1



prison_cleaned <- prison_cleaned %>%
  mutate(incarceration_impacts_motivation_to_vote = case_when(
    incarceration_impacts_motivation_to_vote %in% c("Increased my motivation to vote", "Slightly increased my motivation to vote") ~ 1, incarceration_impacts_motivation_to_vote %in% c("Decreased my motivation to vote",  "Slightly decreased my motivation to vote", "no impact") ~ 0
  )) 

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
  filter(party %in% c("The Democratic party", "The Republican party", "Independent")) %>%
  mutate(party = case_when(
    party == "The Democratic party" ~ 1,
    party == "Independent" ~ 2,
    party == "The Republican party" ~ 0
  ))

prison_cleaned <- prison_cleaned %>%
  mutate(incarceration_motivation_label = case_when(
    incarceration_impacts_motivation_to_vote == 1 ~ "Increased motivation",
    incarceration_impacts_motivation_to_vote == 0 ~ "Decreased motivation/No change"
  )) |>
  mutate(
    length_label = case_when(
      length_in_this_facility == 1 ~ "More than 10 years",
      length_in_this_facility == 0 ~ "10 years or less"
    )
  ) |> 
  rename(
    `Time incarcerated` = length_label,
    `Motivation to vote` = incarceration_motivation_label
  )


# Step 1: run the model 

probit.model <- glm(incarceration_impacts_motivation_to_vote ~ length_in_this_facility + identifies_as_black  + identifies_as_hispanic_or_latinx + ever_voted + party, 
                    family = binomial(link = "probit"),
                    data = prison_cleaned)

summary(probit.model)


modelsummary(probit.model,
             title = "Impact of Incarceration on Voting Motivation",
             coef_map = c(
               "length_in_this_facility" = "Length in Facility",
               "ever_voted" = "Voted in the past",
               "identifies_as_black" = "Black",
               "identifies_as_hispanic_or_latinx" = "Hispanic/Latino",
               "age" = "Age",
               "how_often_officials_acting_in_your_interest" = "Politicians are acting in your interest",
               "party" = "Party"
             ),
             stars = TRUE,
             gof_map = c("nobs", "r.squared"),
             output = "latex"
             
             
)

#Step 2: Baseline 

preds_baseline <- predict(probit.model, type = "response")
summary(preds)


coefs_probit <- coef(probit.model)
coefs_probit


#Step 3: predicted outcome

pprobit <- pnorm(coefs_probit[2]*prison_cleaned$length_in_this_facility + coefs_probit[3]*prison_cleaned$identifies_as_black + coefs_probit[4]*prison_cleaned$identifies_as_hispanic_or_latinx + coefs_probit[5]*prison_cleaned$ever_voted + coefs_probit[6]*prison_cleaned$party + coefs_probit[1])
summary(pprobit)

testp <- preds - pprobit
summary(testp)

px1_1 <- pnorm(coefs_probit[2]*1 + coefs_probit[3]*prison_cleaned$identifies_as_black + coefs_probit[4]*prison_cleaned$identifies_as_hispanic_or_latinx + coefs_probit[5]*prison_cleaned$ever_voted + coefs_probit[6]*prison_cleaned$party + coefs_probit[1])
summary(px1_1)


px1_0 <- pnorm(coefs_probit[2]*0 + coefs_probit[3]*prison_cleaned$identifies_as_black + coefs_probit[4]*prison_cleaned$identifies_as_hispanic_or_latinx + coefs_probit[5]*prison_cleaned$ever_voted + coefs_probit[6]*prison_cleaned$party + coefs_probit[1])

summary(px1_0)

#Step 4: Effect

effectx1 <- px1_1 - px1_0


summary(cbind(px1_1, px1_0, effectx1))

#Make a nice table here. Substantive Significance? 

df1 <- tibble(
  `Baseline`   = preds_baseline,
  `Time Incarcerated = 0`   = px1_0,
  `Time Incarcerated = 1`  = px1_1,
  `Avg difference (0 − 1)`   = effectx1
)


order_levels <- c(
  "Baseline",
  "Time Incarcerated = 0",
  "Time incarcerated = 1",
  "Avg difference (0 − 1)"
)


stats_tbl <- df1 |>
  pivot_longer(everything(), names_to = "Quantity", values_to = "value") |>
  group_by(Quantity) |>
  summarise(
    Mean   = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  dplyr::mutate(Quantity = factor(Quantity, levels = order_levels)) |>
  dplyr::arrange(Quantity)
stats_tbl
gt_tbl <-
  stats_tbl |>
  gt(rowname_col = "Quantity") |>
  tab_header(
    title = md("**Table 2. Summary of predicted probabilities**")
  ) |>
  tab_options(table.width = pct(80))

gt_tbl


#Step 6: Simulation 

m1 <- glm(incarceration_impacts_motivation_to_vote ~ length_in_this_facility + identifies_as_black  + identifies_as_hispanic_or_latinx + ever_voted + party, 
          family = binomial(link = "probit"),
          data = prison_cleaned)
summary(m1)

coefs <- coef(m1)
coefs

d <- prison_cleaned[complete.cases(prison_cleaned),]


n_draws <- 1000

set.seed(17)
sim_coefs <- rmvnorm(n_draws, coefs, vcov(m1)) 

dim(sim_coefs)
colnames(sim_coefs)

rbind(coefs, apply(sim_coefs, 2, mean)) 

p_mean_baseline <- NULL
for (i in 1:1000) {
  p_mean_baseline[i] <- mean(pnorm(sim_coefs[i,1] + sim_coefs[i,2]*d$length_in_this_facility + sim_coefs[i,3]*d$identifies_as_black + sim_coefs[i,4]*d$identifies_as_hispanic_or_latinx + sim_coefs[i,5]*d$ever_voted + sim_coefs[i,6]*d$party))
}

summary(p_mean_baseline)


#Step 7

px1_1_mean <- NULL
for (i in 1:1000) {
  px1_1_mean[i] <- mean(pnorm(sim_coefs[i,1] + sim_coefs[i,2]*1 + sim_coefs[i,3]*d$identifies_as_black + sim_coefs[i,4]*d$identifies_as_hispanic_or_latinx + sim_coefs[i,5]*d$ever_voted + sim_coefs[i,6]*d$party))
}


px1_0_mean <- NULL
for (i in 1:1000) {
  px1_0_mean[i] <- mean(pnorm(sim_coefs[i,1] + sim_coefs[i,2]*0 + sim_coefs[i,3]*d$identifies_as_black + sim_coefs[i,4]*d$identifies_as_hispanic_or_latinx + sim_coefs[i,5]*d$ever_voted + sim_coefs[i,6]*d$party))
}

#Step 8

effectx1_mean <- px1_1_mean - px1_0_mean
summary(effectx1_mean)


#Step 9 

margeffx2_mean <- NULL
for (i in 1:1000) {
  margeffx2_mean[i] <- mean(dnorm(sim_coefs[i,1] + sim_coefs[i,2]*d$length_in_this_facility + sim_coefs[i,3]*d$identifies_as_black)*sim_coefs[i,3] *sim_coefs[i,4] *sim_coefs[i,5]*sim_coefs[i,6])
}

lapply(means <- list(p_mean, px1_1_mean, px1_0_mean, effectx1_mean, margeffx2_mean), summary)


q <- do.call("rbind", (lapply(means, quantile, c(.025,.975)))) 
q                                                              

results.p <- cbind(q[,1], lapply(means, mean),q[,2]) 
colnames(results.p) <- c("2.5", "Mean", "97.5") 
rownames(results.p) <- c("p_mean", "px1_1_mean", "px1_0_mean", "effectx1_mean", "margeffx2_mean") 
results.p


#Table 3
ci_tbl <-
  as.data.frame(results.p) |>
  rownames_to_column("Quantity") |>
  rename(`2.5%` = `2.5`, `97.5%` = `97.5`) |>
  mutate(
    Quantity = recode( Quantity,
                       p_mean = "Baseline",
                       px1_0_mean       = "Time Incarcerated = 0",
                       px1_1_mean      = "Time Incarcerated = 1",
                       effectx1_mean   = "Avg difference (0 − 1)"
    ),
    Quantity = factor(
      Quantity,
      levels = c(
        "Baseline",
        "Time Incarcerated = 0",
        "Time Incarcerated = 1",
        "Avg difference (0 − 1)"
      )
    )
  ) |>
  arrange(Quantity)

gt_tbl <-
  ci_tbl |>
  gt(rowname_col = "Quantity") |>
  tab_header(
    title = md("**Table 3. Predicted Probabilities with 95% CIs (Simulation)**")
  ) |>
  cols_label(
    Mean  = "Mean",
    `2.5%`  = "Lower 95%",
    `97.5%` = "Upper 95%"
  ) |>
  fmt_number(columns = c(`2.5%`, Mean, `97.5%`), decimals = 3) |>
  tab_options(table.width = pct(75))

gt_tbl

