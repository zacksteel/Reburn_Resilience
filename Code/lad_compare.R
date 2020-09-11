## Purpose: Compare LAD distributions of select seral stages 
## Project: Reburn_Resilience

library(tidyverse)
library(brms)
library(broom)

## Bring in lad data
d <- read.csv("data/lad_data.csv", row.names = F)

## Compare once-burned early-seral with late-seral groups
d_el <- filter(d, year == 2009) %>% 
  mutate(stage = factor(st_stage)) %>% 
  dplyr::select(stage, value, metric, strata, stat) %>% 
  group_by(strata, stat) %>% 
  nest() %>% 
  mutate(model = purrr::map(data, ~brm(value ~ stage, data = .x)),
         summary = purrr::map(model, ~tidy(.x, intervals = T)))
## Lets also get draws so we can calcualte probability of positive
d_el2 <- mutate(d_el, 
               ## pull out mean values first
               means = purrr::map(data, ~{group_by(.x, stage) %>% 
                   summarise(mean = mean(value),
                             sd = sd(value), .groups = "keep") %>% 
                   ungroup()
               }),
               b1 = purrr::map(model, ~posterior_samples(.x, pars = c("b_")) %>% 
                                 pull(2)),
               prob_pos = purrr::map_dbl(b1, ~{length(.x[.x>0]) / length(.x)}),
               prob_neg = 1-prob_pos) %>% 
  unnest(cols = means) %>% 
  mutate(mean_dens = exp(mean)) %>% 
  dplyr::select(strata, stat, stage, mean_dens, mean, sd, prob_pos, prob_neg) 

## HS-HS and HS-LMS comparison in 2015
d_e <- filter(d, ch_stage %in% c("early", "early-mid") & year == 2015) %>% 
  mutate(stage = factor(ch_stage)) %>% 
  dplyr::select(stage, value, metric, strata, stat) %>% 
  group_by(strata, stat) %>% 
  nest() %>% 
  ## run some models
  mutate(model = purrr::map(data, ~brm(value ~ stage, data = .x)),
         summary = purrr::map(model, ~tidy(.x, intervals = T)))
## Lets also get draws so we can calcualte probability of positive
d_e2 <- mutate(d_e, 
                ## pull out mean values first
                means = purrr::map(data, ~{group_by(.x, stage) %>% 
                    summarise(mean = mean(value),
                              sd = sd(value), .groups = "keep") %>% 
                    ungroup()
                  }),
                b1 = purrr::map(model, ~posterior_samples(.x, pars = c("b_")) %>% 
                                    pull(2)),
                prob_pos = purrr::map_dbl(b1, ~{length(.x[.x>0]) / length(.x)}),
                prob_neg = 1-prob_pos) %>% 
  unnest(cols = means) %>% 
  mutate(mean_dens = exp(mean)) %>% 
  dplyr::select(strata, stat, stage, mean_dens, mean, sd, prob_pos, prob_neg) 

## Next looking at once burned forest (2009) vs. unburned
d_l <- filter(d, (st_stage == "late" & year == 2009) | 
                (st_stage == "suppressed" & year == 2015)) %>% 
  mutate(stage = factor(st_stage)) %>% 
  dplyr::select(stage, value, metric, strata, stat) %>% 
  group_by(strata, stat) %>% 
  nest() %>% 
  ## run some models
  mutate(model = purrr::map(data, ~brm(value ~ stage, data = .x)),
         summary = purrr::map(model, ~tidy(.x, intervals = T)))
## Lets also get draws so we can calcualte probability of positive
d_l2 <- mutate(d_l, 
               ## pull out mean values first
               means = purrr::map(data, ~{group_by(.x, stage) %>% 
                   summarise(mean = mean(value),
                             sd = sd(value), .groups = "keep") %>% 
                   ungroup()
               }),
               b1 = purrr::map(model, ~posterior_samples(.x, pars = c("b_")) %>% 
                                 pull(2)),
               prob_pos = purrr::map_dbl(b1, ~{length(.x[.x>0]) / length(.x)}),
               prob_neg = 1-prob_pos) %>% 
  unnest(cols = means) %>% 
  mutate(mean_dens = exp(mean)) %>% 
  dplyr::select(strata, stat, stage, mean_dens, mean, sd, prob_pos, prob_neg) 

## Conbine and export for later
out <- bind_rows(d_el2, d_e2, d_l2) %>% 
  dplyr::select(stage, everything()) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, signif, digits = 3)

write.csv(out, "data/lad_contrasts.csv", row.names = F)

