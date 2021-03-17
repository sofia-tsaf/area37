## Extract results of interest, write TAF output tables

## Before: results.rds (model)
## After:  stock_tables/*.csv, all_effort.txt, current_status.csv, results.rds,
##         stock_timeseries.csv (output)

library(icesTAF)
library(dplyr)        # case_when, count, group_by, select, filter, mutate
library(tidyr)        # pivot_wider, unnest

mkdir("output")

nested_indo <- readRDS("model/results.rds")
cp("model/results.rds", "output")

## -------------------------------------------------------------------------------------------------

# nested_indo <- nested_indo %>%
#   filter(map_lgl(
#     map_chr(sraplus_diagnostics, "fit_diagnostic_message"),
#     ~ .x == "There is no evidence that the model is not converged"
#   ))

## -------------------------------------------------------------------------------------------------

indo_results <- nested_indo %>%
  select(stock, taxa, sraplus_summary) %>%
  unnest(cols = sraplus_summary) %>%
  filter(variable == "b_div_bmsy") %>%
  mutate(status = dplyr::case_when(mean > 1.2 ~ "underfished",
                                   mean > 0.8 ~ "fully fished",
                                   TRUE ~ "overfished"))
write.taf(indo_results, "output/current_status.csv")

indo_results %>%
  group_by(status) %>%
  count()

## -------------------------------------------------------------------------------------------------

#walk2(nested_indo$stock,nested_indo$sraplus_fit_plot, savefoo)
# setwd("C:/Users/rishi/Documents/IndonesiaSDGStkStatus")
#for(i in 1:nrow(nested_indo)){
#out<-write.csv(nested_indo$sraplus_summary[i],file=paste(nested_indo$stock[i],".csv",sep=""))}

mkdir("output/stock_tables")
for(i in 1:nrow(nested_indo)){
  out<-write.csv(nested_indo$sraplus_fit[i][[1]]$results,file=paste0("output/stock_tables/",nested_indo$stock[i],".csv"))
  out2<-write.table(nested_indo$sraplus_fit[i][[1]]$results,file="output/all_effort.txt",append=T)
}

## -------------------------------------------------------------------------------------------------

#save(nested_indo,file="C:/Users/rishi/Documents/Area37Marcelo/results/Area37NominalEffort.RData")

#Area37Results=ldply(nested_indo$sraplus_fit[[i]]$results,function(x){
 # if (is.null(x)) return(NULL)
 # subset(x$results,variable%in%c("u_div_umsy","b_div_bmsy"))})

#save(sraresultsscenario3,file="C:/Users/rishi/Documents/ICEScompsdatasetRainer/results/icesSRAResultsscenario3Jun30.RData")

## -------------------------------------------------------------------------------------------------

# These tables were already created, see stock_tables
# setwd("C:/Users/rishi/Documents/Area67TestsUW")
# for(i in 1:(nsamps-1)){
# out<-write.csv(nested_indo$sraplus_fit[[i]]$results,file=paste(nested_indo$stock[i],".csv",sep=""))}

## -------------------------------------------------------------------------------------------------

# Already have this column:
# nested_indo <- nested_indo %>%
#   mutate(sraplus_diagnostics = map2(sraplus_fit, driors, diagnose_sraplus))

nested_indo$sraplus_diagnostics[[1]]$final_gradient

## -------------------------------------------------------------------------------------------------

n<-length(nested_indo$stock)
resList <- vector(mode="list",length=n)
for(i in 1:n){
  tmp <- nested_indo$sraplus_fit[[i]]$results %>%
    filter(variable %in% c("b_div_bmsy","u_div_umsy")) %>%
    pivot_wider(id_cols="year",names_from="variable", values_from="mean")
  resList[[i]] <- cbind(stock=nested_indo$stock[i],tmp)
}
resTab <- Reduce(rbind,resList)
newResTab <- resTab
names(newResTab)=c("Stock","yr","bbmsy","ffmsy")
newResTab$bbmsy.effEdepP<-newResTab$bbmsy
newResTab$ffmsy.effEdepP<-newResTab$ffmsy
write.taf(newResTab, "output/stock_timeseries.csv")
