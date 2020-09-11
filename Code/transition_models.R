## Purpose: Run reburn transition models 
## Project: Reburn_Resilience

library(tidyverse)
library(brms)

## read in data
de_s <- readRDS("Data/earlymodel_data.rds")
dl_s <- readRDS("Data/latemodel_data.rds")
  
## Run the models 
options(mc.cores = parallel::detectCores())

me <- brm(ch_high ~ 
            dem_mn + dem_sd +
            twi_mn + rh_min + wind +
            lad_01_mnl + lad_01_sdl +
            lad_02_mnl + lad_02_sdl +
            lad_08_mnl + lad_08_sdl +
            gp(x_coord, y_coord, k = 10, c = 5/4),
          data = de_s, family = bernoulli,
          prior = c(prior(normal(0, 2), class = Intercept),
                    prior(normal(0, 1), class = b),
                    prior(cauchy(0, 1), class = sdgp)),
          init = 0,
          control = list(adapt_delta = 0.99, max_treedepth = 15)) 
  
ml <- brm(ch_high ~ 
            dem_mn + dem_sd +
            twi_mn + rh_min + wind +
            lad_01_mnl + lad_01_sdl +
            lad_02_mnl + lad_02_sdl +
            lad_08_mnl + lad_08_sdl +
            gp(x_coord, y_coord, k = 10, c = 5/4),
          data = dl_s, family = bernoulli,
          prior = c(prior(normal(0, 1), class = Intercept),
                    prior(normal(0, 1), class = b),
                    prior(cauchy(0, 1), class = sdgp)),
          init = 0,
          control = list(adapt_delta = 0.99, max_treedepth = 15)) 

## save models 
saveRDS(me, "Models/early_model.rds")
saveRDS(ml, "Models/late_model.rds")

