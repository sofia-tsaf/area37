## Run analysis, write model results

## Before:
## After:

library(icesTAF)

mkdir("model")

## ----fit-sraplus----------------------------------------------------------------------------------

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


Sys.time() - a

  nested_indo <- nested_indo %>%
  mutate(sraplus_worked = map_lgl(map(sraplus_fit,"error"), is.null)) %>%
  filter(sraplus_worked) %>%
  mutate(sraplus_fit = map(sraplus_fit,"result"))


plot_driors(nested_indo$driors[[1]])

plot_prior_posterior(nested_indo$sraplus_fit[[1]], nested_indo$driors[[1]])

plot_prior_posterior(nested_indo$sraplus_fit[[1]], nested_indo$driors[[1]])


plot(nested_indo$driors[[1]]$catch[nested_indo$driors[[1]]$years %in% nested_indo$driors[[1]]$effort_years] / nested_indo$driors[[1]]$effort)



## -------------------------------------------------------------------------------------------------

nested_indo <- nested_indo %>%
  mutate(sraplus_summary = map(sraplus_fit, sraplus::summarize_sralpus))



## -------------------------------------------------------------------------------------------------

nested_indo <- nested_indo %>%
  mutate(sraplus_diagnostics = map2(sraplus_fit, driors, diagnose_sraplus))

save(nested_indo,file="C:/Users/rishi/Documents/Area37Marcelo/results/Area37NominalEffortAggSOFIAdata.RData")
