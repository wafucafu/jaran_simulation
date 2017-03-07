# 環境設定
library(shiny)
library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(lazyeval)


source("data_loading/load_data.R")
source("data_loading/validation.R")
source("preprocess/preprocess_data.R")
source("preprocess/cross_validation.R")
source("simulation/prepare_simulation.R")
source("simulation/simulator.R")
source("simulation/organize_output.R")
source("system/constants.R")

source("ui.R")
source("server.R")

old = theme_set(theme_gray(base_family="HiraKakuProN-W3"))
options(scipen = 999)


shinyApp(ui = ui, server = server)















