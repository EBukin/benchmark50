

# In this code we do the analysis of efficiency of one crop produciton in the
#     Agricultural etnerprices of ukraine based on the data about all enterprises 
#     and data envelopment analysis with the second stage.


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
library(Hmisc)
library(PerformanceAnalytics)

# Loading data
source("R/join_sg_names.R")
source("R/calc_sg_val.R")
source("R/get_sg_val.R")
source("R/simpl_sg_expr.R")
source("R/filter_sg_data.R")
source("R/parallel_setup.R")

# Loading data to the environment 
load("./data/all_forms.Rdata")


# Setting the problem -----------------------------------------------------

# The problem is to estimate the tectincal efficiency of Ukrainian enterprises
# Perform the second state analysis of the efficiencies using ANOVA.

#   We focus on all enterprises, which have a non zero production of wheat.

# We analyse efficiency for one crop - Wheat in 
#     601 - code of wheat
# For inputs we are using 
#     601 G1 - Land
#     601 G3 - Seeds costs
#     601 G4 - Fertilizers costs
#     601 G5 - Oil and gasoline
#     601 G6 - Services
#     601 G8 - Labour costs
#     601 G10  Depresiation - Capital

# Path to the folder with the output data and results.
folder <- "output/kneu_prez_september_2016/"

# Preparing data ----------------------------------------------------------

# After sourcing the script for loading data we get a dataset "sgData"
source("scripts/loading_50sg_data.R")

# For the purposes of this example we use:
#   Only those enterprises, which ploduces some wheat in one year
data <-
  sgData %>%
  filter(forma == "50",
         god == 2012) %>%
  mutate(wheat_prod =
           ifelse(code %in% c(25, 21) &
                    var == "G2" &
                    val > 0 ,
                  TRUE,
                  FALSE)) %>%
  group_by(god, final_code) %>%
  filter(any(wheat_prod)) %>% 
  select(-wheat_prod)
  
rm(sgData)

# Preparing data set with all data used for calculating efficiency scores
eff_data <-
  
  data %>%
  
  # Selecting only variables used for further analysis
  filter_sg_data(c( 25,  25,   25,   25,   21,   21,   21,   21,   10,  216,  10,  216,  601, 601, 601, 601, 601, 601), 
                 c("G1", "G2", "G3", "G7", "G1", "G2", "G3", "G7", "G3", "G3","G7", "G7","G3","G4","G5","G6","G8","G10")) %>% 
  unite(code_var, code, var) %>% 
  spread(code_var, val) %>% 
  rowwise() %>% 
  
  # Calculatin total wheat production 
  mutate(`601_GC1` = sum(`25_G1`, `21_G1`, na.rm = T),
         `601_GC2` = sum(`25_G2`, `21_G2`, na.rm = T), 
         `601_GC3` = sum(`25_G3`, `21_G3`, na.rm = T), 
         `601_GC7` = sum(`25_G7`, `21_G7`, na.rm = T)) %>%
  
  # Share of wheat costs in total costs 
  # Share of crops costs in total costs 
  # Share of wheat revenues in total revenues 
  # Share of crops revenues in total revenues 
  mutate(`601_in_216_G3` = `601_GC3`/ `216_G3`,
         `601_in_216_G7` = `601_GC7`/ `216_G7`,
         `601_in_10_G3` = `601_GC3`/ `10_G3`,
         `601_in_10_G7` = `601_GC7`/ `10_G7`,
         `10_in_216_G3` = `10_G3`/ `216_G3`,
         `10_in_216_G7` = `10_G7`/ `216_G7`) %>% 
  ungroup() %>% 
  filter((`601_GC1`) != 0)

# Cleaning data 
eff_data[is.na(eff_data)] <- 0

# Efficiency analysis ---------------------------------------------

# Efficiency with constant return of scale
# eff_crs <-
#   dea(
#     Y = as.matrix(eff_data$`601_GC2`),
#     X = as.matrix(eff_data %>% select(`601_GC1`, `601_G3`, `601_G4`, `601_G5`, `601_G6`, `601_G8`, `601_G10`)),
#     RTS = "crs",
#     ORIENTATION = "in", 
#     SLACK = F
#   )
# 
# # Creating variable with the efficiency analysis results.
# eff_res <- 
#   bind_cols(eff_data,
#             data.frame(eff_crs = eff_crs$eff))
# rm(eff_crs)

# Efficiency with constant return of scale
eff_vrs <-
  dea(
    Y = as.matrix(eff_data$`601_GC2`),
    X = as.matrix(eff_data %>% select(`601_GC1`, `601_G3`, `601_G4`, `601_G5`, `601_G6`, `601_G8`, `601_G10`)),
    RTS = "vrs",
    ORIENTATION = "in", 
    SLACK = F
  )

# Creating variable with the efficiency analysis results.
eff_res <- 
  bind_cols(eff_data,
            data.frame(eff_vrs = eff_vrs$eff))
# rm(eff_vrs)

# Calculating scale efficiency
# eff_res$eff_se <- eff_res$eff_crs / eff_res$eff_vrs 


### Second stage analysis -----------------------------------------


# Duplicating initial data object
duplicate <- eff_res

# Selecting variables used for regression.
s_stage_data <- 
  eff_res %>% 
  mutate(area = `601_GC1`,
         prod = `601_GC2`,
         totCosts = `601_GC3`,
         seeds = `601_G3`, 
         fert = `601_G4`,
         oil = `601_G5`, 
         services = `601_G6`, 
         labour = `601_G8`, 
         depres = `601_G10`,
         specialization = `10_in_216_G3`,
         focus = `601_in_10_G3`,
         area_totCosts = area * totCosts) %>% 
  select(final_code, god, eff_vrs, 
         totCosts, seeds, fert, oil, services, labour, depres,
         area, specialization, focus, area_totCosts, prod) %>% 
  filter(!is.infinite(totCosts), !is.infinite(seeds), !is.infinite(fert), 
         !is.infinite(oil), !is.infinite(services), !is.infinite(labour), 
         !is.infinite(depres), !is.infinite(area))

s_stage_data$yields <- s_stage_data$prod / s_stage_data$area
s_stage_data$costs_ha <- s_stage_data$totCosts / s_stage_data$area


# Building a correlation matrix before making any regression analysis
png(
  filename = paste0(folder, "correlogram.png"),
  width = 20,
  height = 14,
  units = "cm", 
  res = 300
)
chart.Correlation(s_stage_data %>%
                    select(eff_vrs, area, yields),
                  histogram=TRUE,
                  pch=12,
                  main = "Correlogram of the independent variables")
dev.off()


# Linear regression -------------------------------------------------------

fit <- lm(eff_vrs ~ yields*area, data = s_stage_data)

summary(s_stage_data$area)

summary(fit)

png(
  filename = paste0(folder, "Diagnostic plots regression.png"),
  width = 20,
  height = 14,
  units = "cm", 
  res = 300
)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots
layout(matrix(c(1),1,1)) # optional layout 
dev.off()


# Plot with efficiency groups -------------------

# Building categories specific histograms with the yields distributions
eff_yields_data <- 
  s_stage_data %>%
  mutate(
    `Efficiency group` =
      ifelse(
        eff_vrs >= 0.65,
        "1. eff >= 65%",
        ifelse(eff_vrs >= 0.3 , "2. 30% >= eff < 65%", "3. eff < 30%")
      ))

# Plotting grouped efficiecnt by yield
png(
  filename = paste0(folder, "Density of yields by groups.png"),
  width = 20,
  height = 14,
  units = "cm", 
  res = 300
)
ggplot(eff_yields_data, aes(x = yields, fill = `Efficiency group`)) +
  geom_density(alpha = .3) + 
  scale_x_continuous( limits = c(0,100))
dev.off()

# Plotting Produciton vs Arable lands
# Constructing frontier with the VRS with production vs area
png(
  filename = paste0(folder, "VRS Frontier production vs area.png"),
  width = 20,
  height = 14,
  units = "cm", 
  res = 300
)


dea.plot.frontier(
  y = as.matrix(eff_data$`601_GC2` / 10000),
  x = as.matrix(eff_data$`601_GC1` / 1000),
  # txt = as.matrix(eff_data$final_code),
  RTS = "vrs",
  ylab = "Wheat production, 1000 tons",
  xlab = "Harvested area, 1000 ha"
)


dev.off()

# Table with the most efficient enterprises
most_eff <-
  eff_yields_data %>% 
  filter(eff_vrs > 0.75) #%>% 

write.csv(most_eff, paste0(folder, "effitincy_summary.csv"))
