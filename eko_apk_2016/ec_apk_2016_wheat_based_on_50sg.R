
# 24/10/2016
# Here we prepare results for the publications in the Economica APK
# We produced the pictures for the Skripnik - Bukin joint reserch 

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

# The problem is to prepare data and graps for the publication. demonstrate the ways of calculating the 
#   DEA efficiency scores for the simplets one year data example and only 
#   for one output - wheat production. 

#   We focus only on those enterprises, which has spend on whet more than 30% 
#       of all costs.  
threshold = 0.30

# Path to the folder with the output data and results.
folder <- "eko_apk_2016/output/"

# Preparing data ----------------------------------------------------------
# ██████╗  █████╗ ████████╗ █████╗ 
# ██╔══██╗██╔══██╗╚══██╔══╝██╔══██╗
# ██║  ██║███████║   ██║   ███████║
# ██║  ██║██╔══██║   ██║   ██╔══██║
# ██████╔╝██║  ██║   ██║   ██║  ██║
# ╚═════╝ ╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝


# After sourcing the script for loading data we get a dataset "sgData"
source("vignettes/load_50sg_work.R")
source("vignettes/load_50sg_home.R")

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
# ███████╗███████╗███████╗     █████╗ ███╗   ██╗   
# ██╔════╝██╔════╝██╔════╝    ██╔══██╗████╗  ██║   
# █████╗  █████╗  █████╗      ███████║██╔██╗ ██║   
# ██╔══╝  ██╔══╝  ██╔══╝      ██╔══██║██║╚██╗██║   
# ███████╗██║     ██║         ██║  ██║██║ ╚████║██╗
# ╚══════╝╚═╝     ╚═╝         ╚═╝  ╚═╝╚═╝  ╚═══╝╚═╝


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
        eff_vrs > 0.65,
        "3. eff > 65%",
        ifelse(eff_vrs > 0.3 , "2. 30% > eff <= 65%", "1. eff <= 30%")
      ))





# Tables -----------------------------------------------------------------
# ████████╗ █████╗ ██████╗ ██╗     ███████╗███████╗
# ╚══██╔══╝██╔══██╗██╔══██╗██║     ██╔════╝██╔════╝
#    ██║   ███████║██████╔╝██║     █████╗  ███████╗
#    ██║   ██╔══██║██╔══██╗██║     ██╔══╝  ╚════██║  
#    ██║   ██║  ██║██████╔╝███████╗███████╗███████║ 
#    ╚═╝   ╚═╝  ╚═╝╚═════╝ ╚══════╝╚══════╝╚══════╝

# Saving a table with the farms, whos efficiecncy is greater than 65%
write.csv(
  x = eff_data %>% 
    filter(eff_vrs >= 0.65) %>% 
    arrange(Area),
  file =  paste0(folder, "firms_eff_vrs_more_65.csv"),
  row.names = FALSE
)


# Table of histogram summary ---
# Mean Mode Mediane of efficiency
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

ddply(eff_data,
      .(`Efficiency group`),
      function(x) {
        data.frame(Mean = mean(x$eff_vrs, na.rm = TRUE),
                   Mode = getmode(x$eff_vrs),
                   Median = median(x$eff_vrs, na.rm = TRUE))
      }) %>% 
  bind_rows(
    data_frame(
      `Efficiency group` = "All",
      Mean = mean(eff_data$eff_vrs, na.rm = TRUE),
      Mode = getmode(eff_data$eff_vrs),
      Median = median(eff_data$eff_vrs, na.rm = TRUE))
    ) %>% 
  write.csv(file =  paste0(folder, "mean_mode_yields_group.csv"))


# Top 10 accordding to the size -------
eff_data %>% 
  ungroup() %>% 
  mutate(Rank =  seq(1,nrow(.),1)) %>% 
  arrange(desc(Area)) %>% 
  slice(1:10) %>% 
  select(Rank, Area, Production, Total_costs, eff_vrs, eff_crs, Yields, `Efficiency group`) %>% 
  write.csv(file =  paste0(folder, "top_10_size.csv"))


# Share of efficient and inefficient recource use -------
eff_data %>% 
  ungroup %>% 
  select(Area, Production, Total_costs, `Efficiency group`) %>% 
  group_by(`Efficiency group`) %>% 
  summarise_each(funs(sum), Area, Production, Total_costs) %>% 
  ungroup() %>% 
  gather(Var, Value, 2:4) %>% 
  group_by(Var) %>% 
  mutate(sum_var = sum(Value)) %>% 
  rowwise() %>% 
  mutate(share = Value / sum_var * 100) %>% 
  ungroup() %>% 
  write.csv(file =  paste0(folder, "Area_prod_costs_shres_by_eff.csv"), row.names = FALSE)


# Saving graphical output -------------------------------------------------
# ██████╗ ██████╗  █████╗  ██████╗ ██╗  ██╗███████╗
# ██╔════╝ ██╔══██╗██╔══██╗██╔══██╗██║  ██║██╔════╝
# ██║  ███╗██████╔╝███████║██████╔╝███████║███████╗
# ██║   ██║██╔══██╗██╔══██║██╔═══╝ ██╔══██║╚════██║
# ╚██████╔╝██║  ██║██║  ██║██║     ██║  ██║███████║
# ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝╚══════╝


# 2. Frontier with the VRS with production vs area ----------------
png(
  filename = paste0(folder, "Fig. 2 VRS front production vs area.png"),
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
  ylab = "Виробництво пшениі, 1000 тон",
  xlab = "Площа задіяна для виробництва пшениці, 1000 га"
)

dev.off()

# NOT USED Frontier with the VRS with production vs area
# png(
#   filename = paste0(folder, "fig_2 VRS front production vs costs.png"),
#   width = 20,
#   height = 14,
#   units = "cm", 
#   res = 300
# )
# dea.plot.frontier(
#   y = as.matrix(eff_data$Production / 10000),
#   x = as.matrix(eff_data$Total_costs / 1000),
#   # txt = as.matrix(eff_data$final_code),
#   RTS = "vrs",
#   ylab = "Виробництво пшениі, 1000 тон",
#   xlab = "Wheat costs, million UAH"
# )
# dev.off()


# 3. Histogram of the efficiency scores destribution ----------------
png(
  filename = paste0(folder, "Fig 3. Hitogram of VRS efficiency distribution.png"),
  width = 20,
  height = 14,
  units = "cm", 
  res = 300
)

hist(eff_data$eff_vrs, 
     main = "", 
     xlab = "Ефективність підприємств із зміним ефектом масштабу",
     ylab = "Частота")

dev.off()

# 4. Destributions of yields based on efficiencies -------------------
png(
  filename = paste0(folder, "Fig 4. Yields distribution based on efficiecny group.png"),
  width = 20,
  height = 14,
  units = "cm", 
  res = 300
)

ggplot(eff_data, aes(x = Yields, fill = `Efficiency group`)) +
  geom_density(alpha = .3) +
  scale_x_continuous( limits = c(0,90)) + 
  theme_minimal() +
  xlab("Урожайність, ц/га") +
  ylab("Щильність розподілу") 

dev.off()


