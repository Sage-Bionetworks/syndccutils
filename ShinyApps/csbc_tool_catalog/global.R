options(stringsAsFactors = FALSE)
library(shiny)
library(shinydashboard)
library(DT)

library(plyr)
library(rjson)

library(synapser)
if(exists("outputCapture")){
  rm(outputCapture)
}
synLogin()

source("getData.R")
