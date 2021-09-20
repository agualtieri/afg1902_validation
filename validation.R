# Validation
rm(list = ls())

library(cleaninginspectoR)
library(tidyverse)
library(readxl)
library(openxlsx)
library(cluster)

# sources
source("./R/check_log.R")
source("./R/check_time.R")
source("./R/data_falsification.R")

`%nin%` <- Negate(`%in%`)

# inputs
data <- read.xlsx("./input/REACH_AFG_ERM11_HEAT_Cleaned_Data.xlsx")
#names(data)[names(data) == "ï..start"] <- "start"
names(data)[names(data) == "_uuid"] <- "uuid"
data <- data %>% mutate(index = 1:nrow(data))

log <- read.xlsx("./input/REACH_AFG_ERM11_HEAT_Cleaning log.xlsx", sheet = 1)
dels <- read.xlsx("./input/REACH_AFG_ERM11_HEAT_Cleaning log.xlsx", sheet = 2)
names(dels)[names(dels) == "_uuid"] <- "uuid"

tool <- read.xlsx("./input/AFG_ERM11_HEAT.xlsx")
 

# inspect data
issues <- inspect_all(data) %>% filter(!is.na(index)) %>% left_join(., select(data, "uuid", "index"), "index")
write.xlsx(issues, paste0("./output/outliers_",lubridate::today(),".xlsx"))


# check log
question.not.in.data <- log %>% filter(question.name %nin% names(data))
uuid.not.in.data <- log %>% filter(uuid %nin% data$uuid)
write.xlsx(uuid.not.in.data, paste0("./output/uuid not in data_",lubridate::today(),".xlsx"))

log <- log %>% filter(question.name %in% names(data))
log <- log %>% filter(uuid %in% data$uuid)

log.c <- check_log(data, log) %>% mutate(., check = ifelse(new.value == value_extracted, "Log applied correctly", "Please check log")) %>%
  filter(check == "Please check log")
write.xlsx(log.c, paste0("./output/log check_",lubridate::today(),".xlsx"))

# similar survyes
similar.surveys <- calculateDifferences(data, tool) %>% filter(number.different.columns < 5)
write.xlsx(similar.surveys, paste0("./output/similar surveys_",lubridate::today(),".xlsx"))

# deletions
dels$uuid %in% data$uuid
uuid.not.in.data$uuid %in% dels$uuid
uuid.not.in.data$uuid %in% data$uuid


# hh members
hh.c <- data %>% select("uuid", "hh_mem", "hh_count") %>% mutate(check = ifelse((hh_memb = hh_count), "Ok", "Error")) %>% filter(check == "Error")

# tables
prop.table(table(data$lcs_severity))
prop.table(table(data$fcs_status))

fcs.c <- data %>% select("uuid", "fcs_score", "fcs_status") %>% mutate(check = case_when(
                                                                                fcs_score > 42 ~ "Acceptable",
                                                                                fcs_score >= 28.5 & fcs_score < 42 ~ "Borderline",
                                                                                fcs_score <= 28 ~ "Poor",
                                                                                TRUE ~ as.character(fcs_score))) %>%
                                                                mutate(check2 = ifelse(fcs_status == check, 1, 0)) %>%
                                                                filter(check2 == 0)

# disability
prop.table(table(data$hhh_disability))
