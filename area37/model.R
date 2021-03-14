## Run analysis, write model results

## Before: input.rds (data)
## After:  results.rds (model)

library(icesTAF)
library(dplyr)   # filter, mutate, sample_n, ungroup
library(purrr)   # map, map_lgl, safely
library(sraplus) # diagnose_sraplus, fit_sraplus, summarize_sralpus

mkdir("model")

nested_indo <- readRDS("data/input.rds")

## ----fit-sraplus----------------------------------------------------------------------------------

setwd("model") # compile inside 'model' folder
sfs <- safely(fit_sraplus)
samps <-30
a = Sys.time()
nested_indo <- nested_indo %>%
  ungroup() %>%
  sample_n(samps) %>%
  mutate(sraplus_fit = map(
    driors,
    ~ sfs(
      driors = .x,
      engine = "stan",
      model = "sraplus_tmb",
      adapt_delta = 0.9,
      max_treedepth = 10,
      n_keep = 4000,
      chains = 1,
      cores = 1,
      #q_slope prior= ,
      estimate_qslope = FALSE,
      estimate_proc_error = TRUE)
    ))
setwd("..")

Sys.time() - a

nested_indo <- nested_indo %>%
  mutate(sraplus_worked = map_lgl(map(sraplus_fit,"error"), is.null)) %>%
  filter(sraplus_worked) %>%
  mutate(sraplus_fit = map(sraplus_fit,"result"))

## -------------------------------------------------------------------------------------------------

nested_indo <- nested_indo %>%
  mutate(sraplus_summary = map(sraplus_fit, sraplus::summarize_sralpus))

## -------------------------------------------------------------------------------------------------

nested_indo <- nested_indo %>%
  mutate(sraplus_diagnostics = map2(sraplus_fit, driors, diagnose_sraplus))

saveRDS(nested_indo, "model/results.rds")
