## Extract results of interest, write TAF output tables

## Before:
## After:

library(icesTAF)

mkdir("output")

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

# nested_indo <- nested_indo %>%
#   filter(map_lgl(
#     map_chr(sraplus_diagnostics, "fit_diagnostic_message"),
#     ~ .x == "There is no evidence that the model is not converged"
#   ))
#
nsamps<-25
for(i in 1:(nsamps-1)){

a = sraplus::summarize_sralpus(nested_indo$sraplus_fit[[i]])

a <- nested_indo$sraplus_fit[[i]]$results

a %>%
  filter(variable == "index_hat_t")
#plot_prior_posterior(nested_indo$sraplus_fit[[i]], #nested_indo$driors[[i]])

  plot_sraplus(nested_indo$sraplus_fit[[i]])
}


## -------------------------------------------------------------------------------------------------

indo_results <- nested_indo %>%
  select(stock, taxa, sraplus_summary) %>%
  unnest(cols = sraplus_summary) %>%
  filter(variable == "b_div_bmsy") %>%
  mutate(status = dplyr::case_when(mean > 1.2 ~ "underfished",
                                   mean > 0.8 ~ "fully fished",
                                   TRUE ~ "overfished"))


indo_results %>%
  group_by(status) %>%
  count()




## -------------------------------------------------------------------------------------------------

resultsindo<-c(14/25,11/25,0/25)
names(resultsindo)=c("fullyfished","overfished","underfished")
barplot(resultsindo,col=c("yellow","red","green"))

resultsindoSOFIA<-c(0.35,0.625,0.025)
names(resultsindoSOFIA)=c("fullyfished","overfished","underfished")
barplot(resultsindoSOFIA,col=c("yellow","red","green"))



## -------------------------------------------------------------------------------------------------

nested_indo <- nested_indo %>%
  #mutate(sraplus_fit_plot = map(sraplus_fit, plot_sraplus))
   mutate(plot_prior_posterior_plot=map2(sraplus_fit,driors,plot_prior_posterior))
savefoo <- function(stock,plot, extension = "pdf"){

  ggsave(filename = paste(stock,extension,sep = "."), plot = plot + labs(title = stock))

}
walk2(nested_indo$stock,nested_indo$plot_prior_posterior_plot,savefoo)
#walk2(nested_indo$stock,nested_indo$sraplus_fit_plot, savefoo)
# setwd("C:/Users/rishi/Documents/IndonesiaSDGStkStatus")
#for(i in 1:nrow(nested_indo)){
#out<-write.csv(nested_indo$sraplus_summary[i],file=paste(nested_indo$stock[i],".csv",sep=""))}

for(i in 1:nrow(nested_indo)){
out<-write.csv(nested_indo$sraplus_fit[i][[1]]$results,file=paste(nested_indo$stock[i],".csv",sep=""))
out2<-write.table(nested_indo$sraplus_fit[i][[1]]$results,file="resultsallArea57sumeffort",append=T)}


## -------------------------------------------------------------------------------------------------
#save(nested_indo,file="C:/Users/rishi/Documents/Area37Marcelo/results/Area37NominalEffort.RData")

#Area37Results=ldply(nested_indo$sraplus_fit[[i]]$results,function(x){
 # if (is.null(x)) return(NULL)
 # subset(x$results,variable%in%c("u_div_umsy","b_div_bmsy"))})

#save(sraresultsscenario3,file="C:/Users/rishi/Documents/ICEScompsdatasetRainer/results/icesSRAResultsscenario3Jun30.RData")




## -------------------------------------------------------------------------------------------------

nested_indo <- nested_indo %>%
  mutate(sraplus_fit_plot = map(sraplus_fit, plot_sraplus))

savefoo <- function(stock,plot, extension = "pdf"){

  ggsave(filename = paste(stock,extension,sep = "."), plot = plot + labs(title = stock))

}

walk2(nested_indo$stock,nested_indo$sraplus_fit_plot, savefoo)
setwd("C:/Users/rishi/Documents/Area67TestsUW")
for(i in 1:(nsamps-1)){
out<-write.csv(nested_indo$sraplus_fit[[i]]$results,file=paste(nested_indo$stock[i],".csv",sep=""))}


## -------------------------------------------------------------------------------------------------

nested_indo <- nested_indo %>%
  mutate(sraplus_diagnostics = map2(sraplus_fit, driors, diagnose_sraplus))

nested_indo$sraplus_diagnostics[[1]]$final_gradient



## -------------------------------------------------------------------------------------------------

#Simple way..Run
library(egg)
setwd("C:/Users/rishi/Documents/Area67TestsUW/cpueeffforcedpriorfits")
ResultsArea67<-read.csv("ResultsArea67EffectiveeffortInformativePrior.csv")
p1 <- plotProp(ResultsArea67,method="area67.effEdepP",cats=3, type="prop")
p2 <- plotProp(ResultsArea67,method="area67.effEdepP",cats=3, type="all")

ggarrange(p1,p2,ncol=1)


## -------------------------------------------------------------------------------------------------

##Summary Plot after getting data sorted###
library(reshape)
library(plyr)
library(dplyr)
setwd("C:/Users/rishi/Documents/Area37Marcelo/sraplusfitfinerresolutionfigs")
Area37<-read.csv("Area67Effectiveeffortforcedpriors.csv")
meltdf <- melt(Area67,id="Year")
sp<-ggplot(meltdf,aes(x=Year,y=value,colour=variable,group=variable)) + geom_line(show.legend = T)
sp2<-sp + geom_hline(yintercept=0.8, linetype="dashed",
                color = "red", size=2)
sp2 + geom_hline(yintercept=1.2, linetype="dashed",
                color = "green", size=2, show.legend = FALSE)

## -------------------------------------------------------------------------------------------------

library(reshape)
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

library(ggplot2)
library(egg)
p1 <- plotProp(newResTab,method="effEdepP",cats=3, type="prop")
p2 <- plotProp(newResTab,method="effEdepP",cats=3, type="all")

ggarrange(p1,p2,ncol=1)

####
library(ggplot2)
ggplot(newResTab,aes(x=yr,y=bbmsy,colour=Stock,group=Stock)) +
  geom_line(show.legend = T) +
  geom_hline(yintercept=0.8, linetype="dashed",color = "red", size=2) +
  geom_hline(yintercept=1.2, linetype="dashed",color = "green", size=2)
