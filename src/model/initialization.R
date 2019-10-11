loadPackages <- function(){
  library(data.table) # fread
  library(R.utils) # to set source directory
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(here) # folder structure - here points to folder "model"
  library(readxl)
  library(leaflet) # geo map
  library(feather) # fst files
  library(tictoc)
  library(ggpubr)
  library(reshape2)
}

installPackages <- function(){
  install.packages("data.table") # fread
  install.packages("R.utils") # to set source directory
  install.packages("dplyr")
  install.packages("tidyr")
  install.packages("ggplot2")
  install.packages("here") # folder structure - here points to folder "model"
  install.packages("readxl")
  install.packages("leaflet")
  install.packages("feather")
  install.packages("tictoc")
  install.packages("ggpubr")
  install.packages("reshape2")
}




