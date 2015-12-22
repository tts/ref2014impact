library(dplyr)
library(shiny)
library(shinydashboard)
library(ggvis)
library(DT)
#library(rCharts)
library(d3heatmap)
#library(reshape)
library(tidyr)

load("ref2014impact.Rda")

# Thresholds for rendering
relThr <- 0.850
sentThr <- 0.850

# Credits
alchemyapi.image <- "alchemyAPI.png"

# Sentiment statistics
sentStat <- kw.df %>%
  group_by(UnitOfA, SentimentType) %>%
  summarise(n = n()) %>%
  mutate(Procent = round((n / sum(n))*100, digits = 2)) %>%
  group_by(SentimentType, Procent) %>%
  arrange(SentimentType, desc(Procent)) %>%
  select(-n) %>%
  spread(key = SentimentType, value = Procent)

rownames(sentStat) <- sentStat$UnitOfA

sentStat <- sentStat %>%
  select(-UnitOfA)

neg <- paste0(round((nrow(kw.df[kw.df$SentimentType == 'negative',])) / nrow(kw.df) * 100, digits=1), "%")
pos <- paste0(round((nrow(kw.df[kw.df$SentimentType == 'positive',])) / nrow(kw.df) * 100, digits=1), "%")
neu <- paste0(round((nrow(kw.df[kw.df$SentimentType == 'neutral',])) / nrow(kw.df) * 100, digits=1), "%")


