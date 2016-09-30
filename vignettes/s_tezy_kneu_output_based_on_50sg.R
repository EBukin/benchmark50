

# In this bit of code we produced the pictures for the Skripnik - Bukin joint 
#     reserch for the conference in KNEU held October 2016


# Here we perfom some of the basic analysis of the form 50 sg.
#   according to the plan, we first are taking agricultural enterprises,
#   which are specialised in production of crop production.

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
#   DEA efficiency scores for the simplets one year data example and only 
#   for one output - wheat production. 


#   We focus only on those enterprises, which has spend on whet more than 30% 
#       of all costs.  
threshold = 0.30

# Path to the folder with the output data and results.
folder <- "output/kneu_tezy_september_2016/"

# Preparing data ----------------------------------------------------------

# After sourcing the script for loading data we get a dataset "sgData"
source("scripts/loading_50sg_data.R")

# For the purposes of this example we use only one year and one form of reporting.
data <- sgData %>% filter(god == 2012, forma == "50")
rm(sgData)

# Preparing data set with all data used for calculating efficiency scores
eff_data <-
  data %>%
  
  # Selecting only variables used for further analysis
  filter_sg_data(c(25, 21,  25, 21, 25, 21, 216), 
                 c("G1","G1","G2","G2","G3","G3","G3")) %>% 
  unite(code_var, code, var) %>% 
  spread(code_var, val) %>% 
  rowwise() %>% 
  
  # Calculating extra parameters
  mutate(`601_GC1` = sum(`25_G1`, `21_G1`, na.rm = T),
         `601_GC2` = sum(`25_G2`, `21_G2`, na.rm = T), 
         `601_GC3` = sum(`25_G3`, `21_G3`, na.rm = T)) %>% 
  mutate(`601_in_216_G3` = `601_GC3`/ `216_G3`,
         filterLog = `601_in_216_G3` >= threshold ) %>% 
  select(final_code, god, `601_GC1`, `601_GC2`, `601_GC3`, `601_in_216_G3`, filterLog) %>% 
  
  # Filtering data accordingly
  filter(!is.na(`601_GC1` ), !is.na(`601_GC2`), !is.na(`601_GC3`))%>% 
  filter(filterLog)


# Efficiency analysis ---------------------------------------------

# Efficiency with constant return of scale
eff_crs <-
  dea(
    Y = as.matrix(eff_data$`601_GC2`),
    X = as.matrix(eff_data %>% select(`601_GC1`, `601_GC3`)),
    RTS = "crs",
    ORIENTATION = "in", 
    SLACK = TRUE
  )

# Efficiency with variable return of scale
eff_vrs <-
  dea(
    Y = as.matrix(eff_data$`601_GC2`),
    X = as.matrix(eff_data %>% select(`601_GC1`, `601_GC3`)),
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


# Saving graphical output -------------------------------------------------

# Building destributions of the efficiencies

# CRS
png(
  filename = paste0(folder, "eff_hist_CRS.png"),
  width = 600,
  height = 400
)
hist(eff_data$eff_crs,
     main = "",
     ylab = "Частота",
     xlab = "Ефективність підприємств зі сталим ефектом масштабу")
dev.off()

# VRS
png(
  filename = paste0(folder, "eff_hist_VRS.png"),
  width = 600,
  height = 400
)
hist(eff_data$eff_vrs,
     main = "",
     ylab = "Частота",
     xlab = "Ефективність підприємств зі змінним ефектом масштабу")
dev.off()

# Building categories specific histograms with the yields distributions
eff_yields_data <- 
  eff_data %>%
  mutate(
    Yields = `601_GC2` / `601_GC1`,
    `Yields group` =
      ifelse(
        eff_vrs >= 0.65,
        "1. eff >= 65%",
        ifelse(eff_vrs >= 0.3 , "2. 30% >= eff < 65%", "3. eff < 30%")
      ))

# Saving data into the file
write.csv(
  x = eff_yields_data,
  file =  paste0(folder, "most_eff_firms.csv"),
  row.names = FALSE
)

# Plotting grouped yields
png(
  filename = paste0(folder, "Density of yields by groups.png"),
  width = 600,
  height = 400
)
ggplot(eff_yields_data, aes(x = Yields, fill = `Yields group`)) +
  geom_density(alpha = .3)
dev.off()

# Constructing frontier with the VRS with production vs area
png(
  filename = paste0(folder, "VRS Frontier production vs area.png"),
  width = 600,
  height = 400
)
dea.plot.frontier(
  y = as.matrix(eff_data$`601_GC2` / 10000),
  x = as.matrix(eff_data$`601_GC1` / 1000),
  # txt = as.matrix(eff_data$final_code),
  RTS = "vrs",
  ylab = "Виробництво пшениці, 1000 тонн",
  xlab = "Посівні площі пшениці, 1000 га"
)
dev.off()

# Constructing frontier with the VRS with production vs area
png(
  filename = paste0(folder, "VRS Frontier production vs costs.png"),
  width = 600,
  height = 400
)
dea.plot.frontier(
  y = as.matrix(eff_data$`601_GC2` / 10000),
  x = as.matrix(eff_data$`601_GC3` / 1000),
  # txt = as.matrix(eff_data$final_code),
  RTS = "vrs",
  ylab = "Виробництво пшениці, 1000 тонн",
  xlab = "Витрати виробництва, млн. грн."
)
dev.off()

# Constructing frontier with the VRS with production vs area
png(
  filename = paste0(folder, "VRS Frontier production vs sum of costs and area.png"),
  width = 600,
  height = 400
)
dea.plot.frontier(
  y = as.matrix(eff_data$`601_GC2` / 10000),
  x = as.matrix(eff_data$`601_GC3` / 1000, eff_data$`601_GC1` / 1000),
  # txt = as.matrix(eff_data$final_code),
  RTS = "vrs",
  ylab = "Виробництво пшениці, 1000 тонн",
  xlab = "1000 га + витрати млн грн"
)
dev.off()

# Constructing frontier with the VRS with production vs area
png(
  filename = paste0(folder, "Share of costs of wheat in total costs.png"),
  width = 600,
  height = 400
)
hist(eff_data$`601_in_216_G3`, 
     main = "",
   xlab = "Доля собівартості пшениці у загальних витратах")
dev.off()
