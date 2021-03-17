## Prepare plots and tables for report

## Before: current_status.csv, results.rds, stock_timeseries.csv (output)
## After:  bbmsy.png, cpue_1.png, driors_1.png, posterior_1.png,
##         status_by_year.png, status_sofia.png, status_sraplus.png,
##         stock_posterior.pdf, stock_timeseries.pdf (report)

library(icesTAF)
library(dplyr)        # mutate
library(egg)          # ggarrange
library(ggplot2)
library(purrr)        # map2, walk2
library(sraplus)      # plot_driors, plot_prior_posterior, plot_sraplus
source("utilities.R") # plotProp

mkdir("report")

nested_indo <- readRDS("output/results.rds")

plot_driors(nested_indo$driors[[1]]) # stock 1 is Sardinella aurita
ggsave("report/driors_1.png")

plot_prior_posterior(nested_indo$sraplus_fit[[1]], nested_indo$driors[[1]])
ggsave("report/posterior_1.png")

taf.png("cpue_1")
plot(nested_indo$driors[[1]]$catch[nested_indo$driors[[1]]$years %in% nested_indo$driors[[1]]$effort_years] / nested_indo$driors[[1]]$effort)
dev.off()

## -------------------------------------------------------------------------------------------------

# This plot is done more efficiently later, see stock_timeseries.pdf
# nsamps<-25
# for(i in 1:(nsamps-1)){
#   # a = sraplus::summarize_sralpus(nested_indo$sraplus_fit[[i]])
#   a <- nested_indo$sraplus_fit[[i]]$results
#   a %>%
#     filter(variable == "index_hat_t")
#   # plot_prior_posterior(nested_indo$sraplus_fit[[i]], #nested_indo$driors[[i]])
#   print(plot_sraplus(nested_indo$sraplus_fit[[i]]))
# }

## -------------------------------------------------------------------------------------------------

indo_results <- read.taf("output/current_status.csv")

taf.png("status_sraplus")
indo_results$status <- ordered(indo_results$status,
                               c("underfished","fully fished","overfished"))
barplot(prop.table(table(indo_results$status)), col=c("green","yellow","red"))
dev.off()

taf.png("status_sofia")
resultsindoSOFIA<-c(0.025,0.35,0.625)
names(resultsindoSOFIA)=c("underfished","fully fished","overfished")
barplot(resultsindoSOFIA,col=c("green","yellow","red"))
dev.off()

## -------------------------------------------------------------------------------------------------

nested_indo <- nested_indo %>%
  #mutate(sraplus_fit_plot = map(sraplus_fit, plot_sraplus))
  mutate(plot_prior_posterior_plot=map2(sraplus_fit,driors,plot_prior_posterior))

savefoo <- function(stock, plot){
  print(plot + labs(title = stock))
}

pdf("report/stock_posterior.pdf")
walk2(nested_indo$stock, nested_indo$plot_prior_posterior_plot, savefoo)
dev.off()

## -------------------------------------------------------------------------------------------------

nested_indo <- nested_indo %>%
  mutate(sraplus_fit_plot = map(sraplus_fit, plot_sraplus))

savefoo <- function(stock, plot){
  print(plot + labs(title = stock))
}

pdf("report/stock_timeseries.pdf")
walk2(nested_indo$stock,nested_indo$sraplus_fit_plot, savefoo)
dev.off()

## -------------------------------------------------------------------------------------------------

# Simple way..Run
# setwd("C:/Users/rishi/Documents/Area67TestsUW/cpueeffforcedpriorfits")
# ResultsArea67<-read.csv("ResultsArea67EffectiveeffortInformativePrior.csv")
# p1 <- plotProp(ResultsArea67,method="area67.effEdepP",cats=3, type="prop")
# p2 <- plotProp(ResultsArea67,method="area67.effEdepP",cats=3, type="all")
# ggarrange(p1,p2,ncol=1)

## -------------------------------------------------------------------------------------------------

# Summary Plot after getting data sorted###
# setwd("C:/Users/rishi/Documents/Area37Marcelo/sraplusfitfinerresolutionfigs")
# Area37<-read.csv("Area67Effectiveeffortforcedpriors.csv")
# meltdf <- melt(Area67,id="Year")
# sp<-ggplot(meltdf,aes(x=Year,y=value,colour=variable,group=variable)) + geom_line(show.legend = T)
# sp2<-sp + geom_hline(yintercept=0.8, linetype="dashed",
#                      color = "red", size=2)
# sp2 + geom_hline(yintercept=1.2, linetype="dashed",
#                  color = "green", size=2, show.legend = FALSE)

## -------------------------------------------------------------------------------------------------

newResTab <- read.taf("output/stock_timeseries.csv")

taf.png("status_by_year")
p1 <- plotProp(newResTab,method="effEdepP",cats=3, type="prop")
p2 <- plotProp(newResTab,method="effEdepP",cats=3, type="all")
ggarrange(p1,p2,ncol=1)
dev.off()

ggplot(newResTab,aes(x=yr,y=bbmsy,colour=Stock,group=Stock)) +
  geom_line(show.legend = T) +
  geom_hline(yintercept=0.8, linetype="dashed",color = "red", size=2) +
  geom_hline(yintercept=1.2, linetype="dashed",color = "green", size=2)
ggsave("report/bbmsy.png")
