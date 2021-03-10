## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)

library(tidyverse)
library(sraplus)
library(here)
library(janitor)

mkdir("data")

indo <- read.csv("bootstrap/data/Area37cuyrrentsofia.csv", header = TRUE)
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
Indoeffort <- read.csv("bootstrap/data/EffortindexRousseaAugNominal.csv", header = TRUE)
index<-Indoeffort$E1

indo <- indo %>%
  left_join(Indoeffort, by = c("year" = "Year"))  #%>%
  # filter(!is.na(E1))

nested_indo<- indo %>%
  group_by(stock, taxa) %>%
  nest() %>%
  ungroup()
