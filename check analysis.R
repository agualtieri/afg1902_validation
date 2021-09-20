# Check Analysis
rm(list = ls())

library(tidyverse)
library(openxlsx)
library(waldo)
library(arsenal)

data_cleaned <- read.xlsx("./input/AFG_ERM11_HEAT_July2021_Cleaned_Data.xlsx")
data_analysis <- read.csv("./input/anaysis/AFG_ERM11_HEAT_July2021_Cleaned_Data.csv", stringsAsFactors = FALSE)
names(data_analysis)[names(data_analysis) == "ï..start"] <- "start"
names(data_analysis)[names(data_analysis) == "X_uuid"] <- "_uuid"
names(data_analysis)[names(data_analysis) == "X_index"] <- "_index"

all.equal(data_cleaned, data_analysis)

summary(comparedf(data_cleaned, data_analysis, by = "_uuid"))
