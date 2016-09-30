

# In order to have understand the meaning of DEA analysis I make this pictures for AS.


# encoding = "utf-8"
Sys.setlocale("LC_CTYPE", "ukrainian")

# Libraries
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(Benchmarking)
library(doSNOW)

# Loading data
source("R/join_sg_names.R")
source("R/calc_sg_val.R")
source("R/get_sg_val.R")
source("R/simpl_sg_expr.R")
source("R/filter_sg_data.R")
source("R/parallel_setup.R")


# Setting the problem -----------------------------------------------------

# The problem is to demonstrate the ways of calculating the 

#   Filter enterprisese, who spends on wheat more than 30 % of own costs 
threshold = 0.30

# Path to the folder with the output data and results.
folder <- "output/wheat_30_percent_example/"

# Preparing data ----------------------------------------------------------

# After sourcing the script for loading data we get a dataset "sgData"
source("vignettes/load_50sg_work.R")

# For the purposes of this example we use only one year and one form of reporting.
data <- sgData %>% filter(god == 2012, forma == "50")
# rm(sgData)

# Preparing data set with all data used for calculating efficiency scores
eff_all_data <-
  data %>%
  
  # Selecting only variables used for further analysis
  filter_sg_data(c(25, 21,  25, 21, 25, 21, 216), 
                 c("G1","G1","G2","G2","G3","G3","G3")) %>% 
  unite(code_var, code, var) %>% 
  spread(code_var, val) %>% 
  rowwise() %>% 
  
  # Calculating extra parameters
  mutate(`Area` = sum(`25_G1`, `21_G1`, na.rm = T),
         `Production` = sum(`25_G2`, `21_G2`, na.rm = T), 
         `Total_costs` = sum(`25_G3`, `21_G3`, na.rm = T)) %>% 
  mutate(`costo_share_of_wheat` = `Total_costs`/ `216_G3`,
         filterLog = `costo_share_of_wheat` >= threshold ) %>% 
  select(final_code, god, `Area`, `Production`, `Total_costs`, `costo_share_of_wheat`, filterLog) %>% 
  
  # Filtering data accordingly
  filter(!is.na(`Area` ), !is.na(`Production`), !is.na(`Total_costs`)) %>% 
  tbl_df()

eff_data <- 
  eff_all_data %>% 
  filter(filterLog)


# Efficiency analysis ---------------------------------------------

# Efficiency with constant return of scale
eff_crs <-
  dea(
    Y = as.matrix(eff_data$`Production`),
    X = as.matrix(eff_data %>% select(`Area`, `Total_costs`)),
    RTS = "crs",
    ORIENTATION = "in", 
    SLACK = TRUE
  )

# Efficiency with variable return of scale
eff_vrs <-
  dea(
    Y = as.matrix(eff_data$`Production`),
    X = as.matrix(eff_data %>% select(`Area`, `Total_costs`)),
    RTS = "vrs",
    ORIENTATION = "in", 
    SLACK = TRUE
  )

# Combining all efficiencies into one table
eff_data$eff_crs <- eff_crs$eff
eff_data$eff_vrs <- eff_vrs$eff

# Calculating scale efficiency
eff_data$eff_se <- eff_crs$eff / eff_vrs$eff 

# Summ of all slacks for all firms
eff_data$slack_crs <- eff_crs$sum
eff_data$slack_vrs <- eff_vrs$sum

# Preparing dataset with the categories of efficiecny
eff_data <- 
  eff_data %>%  
  mutate(
    Yields = `Production` / `Area`,
    `Efficiency group` =
      ifelse(
        eff_vrs >= 0.65,
        "1. eff >= 65%",
        ifelse(eff_vrs >= 0.3 , "2. 30% >= eff < 65%", "3. eff < 30%")
      ))

# Saving graphical output -------------------------------------------------

# Saving a table with the farms, whos efficiecncy is greater than 65%
write.csv(
  x = eff_data %>% 
    filter(eff_vrs >= 0.65) %>% 
    arrange(Area),
  file =  paste0(folder, "firms_eff_vrs_more_65.csv"),
  row.names = FALSE
)

# Building destributions of yields based on efficiencies
png(
  filename = paste0(folder, "yields distribution based on efficiecny group.png"),
  width = 20,
  height = 14,
  units = "cm", 
  res = 300
)


ggplot(eff_data, aes(x = Yields, fill = `Efficiency group`)) +
  geom_density(alpha = .3) + 
  scale_x_continuous( limits = c(0,100))

dev.off()


# Constructing frontier with the VRS with production vs area
png(
  filename = paste0(folder, "VRS front production vs costs.png"),
  width = 20,
  height = 14,
  units = "cm", 
  res = 300
)

dea.plot.frontier(
  y = as.matrix(eff_data$`Production` / 10000),
  x = as.matrix(eff_data$Total_costs / 1000),
  # txt = as.matrix(eff_data$final_code),
  RTS = "vrs",
  ylab = "Wheat production, 1000 Tone",
  xlab = "Wheat costs, million UAH"
)

dev.off()


# Constructing frontier with the VRS with production vs area
png(
  filename = paste0(folder, "VRS front production vs area.png"),
  width = 20,
  height = 14,
  units = "cm", 
  res = 300
)

dea.plot.frontier(
  y = as.matrix(eff_data$`Production` / 10000),
  x = as.matrix(eff_data$`Area` / 1000),
  # txt = as.matrix(eff_data$final_code),
  RTS = "vrs",
  ylab = "Wheat production, 1000 Tone",
  xlab = "Wheat area, 1000 Ha"
)

dev.off()
