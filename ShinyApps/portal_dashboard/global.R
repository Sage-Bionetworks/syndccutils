library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(forcats)
library(tidyverse)
library(feather)
library(lubridate)
library(plotly)
library(DT)
library(kableExtra)
library(ggthemes)
library(rlang)
library(PythonEmbedInR)
library(synapser)
library(syndccutils)
library(yaml)
library(pier)

config = yaml::yaml.load_file("configuration.yaml")

consortium_donut <- function(consortium_counts, key, key_label) {
  consortium_counts %>% 
  select(label = consortium, value = key) %>% 
    mutate(color = if_else(label == "CSBC", "#679EC1", "#EB8231")) %>% 
    pier() %>% 
    pie.size(inner = 75, outer = 95, height = 100) %>% 
    pie.header(text = key_label, 
               size = 15,
               font='Lato', 
               location='pie-center') %>% 
    pie.labels(outer = list(format = 'value', pieDistance = 10), 
               percentage = list(fontSize = 0), 
               value = list(fontSize = 12, 
                            font = 'Lato', 
                            color = "#3C4A63"))
}