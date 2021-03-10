## ----setup, include=FALSE-------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(sraplus)
library(here)


## -------------------------------------------------------------------------------------------------
# setwd("C:/Users/rishi/Documents/Area37Marcelo")
library(here)
indo <- read.csv("Area37cuyrrentsofia.csv", header = T)
# this is what we would call an "wide" format which is really not condusive to analysis. We're going to make it longer

indo <- indo%>%
  pivot_longer(-c(Year, Total), names_to = "stock", values_to = "capture") %>%
  filter(!is.na(Year)) %>%
  janitor::clean_names()

# these names don't look right, are these coming from the FAO or local data? If FAO, even for that region there are usually genus-species names.
# what's up with the



## ----Indo-catches,fig.cap="Individual trajectories of capture"------------------------------------

indo %>% 
  ggplot(aes(year, capture, color  = stock)) + 
  geom_line(show.legend = FALSE) + 
  geom_point()



## ----indo-totals,fig.cap="Total trajectories of capture"------------------------------------------

indo %>% 
  group_by(year) %>% 
  summarise(total_capture = sum(capture)) %>% 
  ggplot(aes(year, total_capture)) + 
  geom_line()



## -------------------------------------------------------------------------------------------------
viable_stocks <- indo %>% 
  group_by(stock) %>% 
  summarise(n_pos_catch = sum(capture > 0)) %>% 
  filter(n_pos_catch > 10)
  
indo <- indo %>% 
  filter(stock %in% viable_stocks$stock) %>% 
  group_by(stock) %>% 
  filter(year > min(year[capture > 0]),
         year <= max(year[capture > 0]))


indo %>% 
  group_by(stock) %>% 
  mutate(capture = capture / max(capture)) %>% 
  ggplot(aes(year, capture, group = stock)) + 
  geom_point()



## -------------------------------------------------------------------------------------------------

indo <- indo %>% 
  ungroup() %>% 
  mutate(stock_number_thing = str_extract_all(stock, '\\d')) %>% 
  mutate(taxa = str_replace_all(stock,"\\d",'')) %>% 
  mutate(taxa = str_replace_all(taxa, "\\."," ") %>% str_trim()) %>% 
  mutate(taxa = str_replace_all(taxa, "  "," ") %>% str_trim()) %>% 
  filter(!is.na(taxa))



## -------------------------------------------------------------------------------------------------
# setwd("C:\\Users\\rishi\\Documents\\Area37Marcelo")
Indoeffort<-read.csv("EffortindexRousseaAugNominal.csv", header=T)
index<-Indoeffort$E1

indo <- indo %>% 
  left_join(Indoeffort, by = c("year" = "Year"))  #%>% 
  # filter(!is.na(E1))

nested_indo<- indo %>% 
  group_by(stock, taxa) %>% 
  nest() %>% 
  ungroup()


## -------------------------------------------------------------------------------------------------
 
nested_indo <- nested_indo %>%
  mutate(
    driors = map2(
      taxa,
      data,
      ~
      format_driors(
      taxa = .x,shape_prior=2,      #use_heuristics = T,shape_prior=2,
      catch = .y$capture,
      years = .y$year,
      initial_state = 0.75,initial_state_cv = 0.1,b_ref_type = "k",
      terminal_state = 0.41,terminal_state_cv = 0.23,
      effort = .y$E1[!is.na(.y$E1)],effort_years=.y$year[!is.na(.y$E1)],
      growth_rate_prior = NA,
      growth_rate_prior_cv = 0.2) 
      #initial_state = 0.5,initial_state_cv = 0.25,b_ref_type = "k",
      #final_u = 1.2,final_u_cv = 0.25, f_ref_type = "fmsy"
      #sar = 4,
      #fmi = c(
        #"research" = .5,
        #"management" = .5,
        #"enforcement" = .35,
        #"socioeconomics" = 0.7
      #)
    ))
  

head(nested_indo)

plot_driors(nested_indo$driors[[2]])


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
# function to add categories describing the relationship between the
# true and approximate categories
library(reshape)
library(plyr)
library(dplyr)
library(egg)
compCat <- function(dat,method="cmsy.naive"){
  tmpDat <- dat
  sep <- ifelse(method=="","",".")
  # for 4 categories
  tmpDat$trueCat4 <- ifelse(dat$bbmsy>1, ifelse(dat$ffmsy<1,1,2),ifelse(dat$ffmsy<1,3,4))
  tmpDat$estCat4 <- ifelse(dat[,paste("bbmsy",method,sep=sep)]>1,
                   ifelse(dat[,paste("ffmsy",method,sep=sep)]<1,1,2),
                   ifelse(dat[,paste("ffmsy",method,sep=sep)]<1,3,4))
  tmpDat$confMat4 <- (tmpDat$trueCat4-1)*4 + tmpDat$estCat4
  
  # for 3 categories
  tmpDat$trueCat3 <- ifelse(dat$bbmsy<0.8, 3,ifelse(dat$bbmsy<1.2, 2, 1)) 
  tmpDat$estCat3 <- ifelse(dat[,paste("bbmsy",method,sep=sep)]<0.8, 3,
                   ifelse(dat[,paste("bbmsy",method,sep=sep)]<1.2, 2, 1))
  tmpDat$confMat3 <- (tmpDat$trueCat3-1)*3 + tmpDat$estCat3
  
  tmpDat
}

plotComp3cats <- function(){
  txt <- c("b>1.2","0.8<b<1.2","b<0.8")
  vals <- c("E","C","C",  "O","E","C",  "O","O","E")
  cols <- c(E="green",O="red",A="gray",C="blue")
  catNames <- c(E="Equal",O="Over optimistic",A="Ambiguous",C="Conservative")
  plot(1,1,xlim=c(0,3),ylim=c(0,3),xaxs="i",yaxs="i",type="n",xlab="",ylab="",xaxt="n",yaxt="n")
  for(i in 1:3){ #true
    for(j in 1:3){ #est
      numVal <- (i-1)*3 + j
      rect(xleft=i-1,xright=i,ybottom=j-1,ytop=j,col=cols[vals[numVal]])
      text(x=i-0.5,y=j-0.5,labels=paste(numVal))
    }
  }
  axis(side=1,at=(1:3)-0.5,labels=txt)
  axis(side=2,at=(1:3)-0.5,labels=txt)
  mtext(side=1,text="Truth",line=2.5)
  mtext(side=2,text="Approximation",line=2.5)  
}

plotComp4cats <- function(){
  txt <- c("b>1,f<1","b>1,f>1","b<1,f<1","b<1,f>1")
  vals <- c("E","C","C","C",  "O","E","A","C",  "O","A","E","C",  "O","O","O","E")
  cols <- c(E="green",O="red",A="gray",C="blue")
  catNames <- c(E="Equal",O="Over optimistic",A="Ambiguous",C="Conservative")
  plot(1,1,xlim=c(0,4),ylim=c(0,4),xaxs="i",yaxs="i",type="n",xlab="",ylab="",xaxt="n",yaxt="n")
  for(i in 1:4){ #true
    for(j in 1:4){ #est
      numVal <- (i-1)*4 + j
      rect(xleft=i-1,xright=i,ybottom=j-1,ytop=j,col=cols[vals[numVal]])
      text(x=i-0.5,y=j-0.5,labels=paste(numVal))
    }
  }
  axis(side=1,at=(1:4)-0.5,labels=txt)
  axis(side=2,at=(1:4)-0.5,labels=txt)
  mtext(side=1,text="Truth",line=2.5)
  mtext(side=2,text="Approximation",line=2.5)  
}


plotComp <- function(dat,method="cmsy.naive",cats=4, type="prop"){
  vals4 <- c("E","C","C","C",  "O","E","A","C",  "O","A","E","C",  "O","O","O","E")
  vals3 <- c("E","C","C",  "O","E","C",  "O","O","E")
  catNames <- c(E="Equal",O="Over optimistic",A="Ambiguous",C="Conservative")

  # create a new data frame with the categories
  tDat <- compCat(dat,method=method)
  tDat <- tDat[,c("Stock","yr","confMat3","confMat4")]
  tDat$cat3 <- vals3[tDat$confMat3]
  tDat$category3 <- catNames[tDat$cat3]
  tDat$cat4 <- vals4[tDat$confMat4]
  tDat$category4 <- catNames[tDat$cat4]
  
  if(cats==3) tDat$category <- tDat$category3 else tDat$category <- tDat$category4
  
  # plot
  library(ggplot2)
  if(type=="prop"){
    ggplot(data=tDat,aes(x=yr,color=category)) +
      geom_bar(aes(fill=category), width = 0.5) +
      theme_minimal()
  }else if(type=="all"){
    ggplot(tDat, aes(x = yr, y = Stock, fill = category)) +
      geom_raster() +
      theme_minimal()
  }
}

plotProp <- function(dat,method="cmsy.naive",cats=4,type="prop"){
  txt3 <- c("b>1.2","0.8<b<1.2","b<0.8")
  txt4 <- c("b>1,f<1","b>1,f>1","b<1,f<1","b<1,f>1")
  
  # create a new data frame with the categories
  tDat <- compCat(dat,method=method)
  tDat <- tDat[,c("Stock","yr","estCat3","estCat4")]

  if(cats==3){
    tDat$estCat <- factor(txt3[tDat$estCat3],levels=txt3)
    cols <- c("darkgreen", "yellow", "red")
  }else{
    tDat$estCat <- factor(txt4[tDat$estCat4],levels=txt4)
    cols <- c("darkgreen", "orange", "yellow", "red")
  } 
  
  # plot
  library(ggplot2)
  if(type=="prop"){
    ggplot(data=tDat,aes(x=yr,color=estCat)) +
      geom_bar(aes(fill=estCat), width = 0.5) +
      theme_minimal() +
      scale_fill_manual(values=cols)
  }else if(type=="all"){
    ggplot(tDat, aes(x = yr, y = Stock, fill = estCat)) +
      geom_raster() +
      theme_minimal() +
      scale_fill_manual(values=cols)
  }
}


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



