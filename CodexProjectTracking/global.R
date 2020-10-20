library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(plotly)

# increase max upload file size
options(shiny.maxRequestSize = 10*1024^2)

### This lib was previously in CRAN, but for now, must be installed by dev.
if("dqshiny" %in% rownames(installed.packages()) == FALSE) {remotes::install_github("daqana/dqshiny")}
library(dqshiny)

page_files <- c("home.R")
purrr::walk(page_files, source, local = environment())
