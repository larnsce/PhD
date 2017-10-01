# header ------------------------------------------------------------------

# A script for "tidying" of JMP 2017 sanitation data and analysis of countries
# with less than 25% access to piped sewers

# Created by Lars Schoebitz
# MIT Licence
# 01.10.2017

# comments ----------------------------------------------------------------

## 01.10.2017: Data manually downloaded from washdata.org/data by clicking "World file"


# clear R's brain ---------------------------------------------------------

rm(list = ls())

# load libraries ----------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(stringr)

# load data ---------------------------------------------------------------

dat <- read.xlsx(xlsxFile = "data/gather/data_sources/JMP_wash_data/JMP_2017_WLD.xlsx", sheet = 3, startRow = 4, colNames = FALSE) %>% 
        as_tibble()

# manipulate data ---------------------------------------------------------

## tidy the dataset

data_tidy <- dat %>% 
        select(1:4, 26:46) %>% 
        gather(key = variable, value = percent, X26:X46) %>%
        mutate(
                variable = case_when(
                        variable == "X26" ~ "safely_managed_n",
                        variable == "X27" ~ "disposed_in_situ_n",
                        variable == "X28" ~ "emptied_and_treated_n",
                        variable == "X29" ~ "wastewater_treated_n",
                        variable == "X30" ~ "latrines_and_others_n",
                        variable == "X31" ~ "septic_tanks_n",
                        variable == "X32" ~ "sewer_connections_n",
                        variable == "X33" ~ "safely_managed_r",
                        variable == "X34" ~ "disposed_in_situ_r",
                        variable == "X35" ~ "emptied_and_treated_r",
                        variable == "X36" ~ "wastewater_treated_r",
                        variable == "X37" ~ "latrines_and_others_r",
                        variable == "X38" ~ "septic_tanks_r",
                        variable == "X39" ~ "sewer_connections_r",
                        variable == "X40" ~ "safely_managed_u",
                        variable == "X41" ~ "disposed_in_situ_u",
                        variable == "X42" ~ "emptied_and_treated_u",
                        variable == "X43" ~ "wastewater_treated_u",
                        variable == "X44" ~ "latrines_and_others_u",
                        variable == "X45" ~ "septic_tanks_u",
                        variable == "X46" ~ "sewer_connections_u"
                )
        ) %>% 
        mutate(
                residence = case_when(
                        variable = str_detect(variable, "_n") == TRUE ~ "national",
                        variable = str_detect(variable, "_r") == TRUE ~ "rural",
                        variable = str_detect(variable, "_u") == TRUE ~ "urban"
                )
        ) %>%
        mutate(variable = str_replace(variable, pattern =  "_n", replacement = "")) %>% 
        mutate(variable = str_replace(variable, pattern =  "_r", replacement = "")) %>% 
        mutate(variable = str_replace(variable, pattern =  "_u", replacement = "")) %>% 
        rename(
                country = X1,
                iso3 = X2,
                year = X3,
                population = X4
        ) %>% 
        mutate(percent = "is.na<-"(percent, percent == "-")) %>%   ## https://stackoverflow.com/a/27909247/6816220
        mutate(percent = as.double(percent)) %>% 
        mutate(population = population * 1000) %>% 
        filter(!is.na(percent))

# analyse data ------------------------------------------------------------

## identify number of countries with less than 25% sewer connection and estimate for SMS

## create dataset to join with main data on national level
sewer_smaller_25 <- data_tidy %>% 
        filter(
                country != "World",
                year == 2015,
                residence == "national",
                variable == "sewer_connections" & percent < 25
        ) %>% 
        mutate(
                sewer_connection = "smaller_25"
        ) %>% 
        select(iso3, sewer_connection)

## join datasets
plot_data <- data_tidy %>% 
        filter(
                year == 2015,
                residence == "national",
                variable == "safely_managed"
        ) %>% 
        left_join(sewer_smaller_25) %>% 
        filter(!is.na(sewer_connection)) 


## prepare plot for proposal

ggplot(plot_data, aes(x = country, y = percent)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        ylab("") +
        xlab("") +
        theme_bw(base_size = 12)





        
        