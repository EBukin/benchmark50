library(plyr)
library(dplyr)
library(tidyr)
library(Benchmarking)

# Here we are making a DEA example for Chapter 6 Example 1.
# 

# Example one - page 165 --------------------------------------------------

# Завантаження необхідних бібліотек із функціями
library(dplyr)        # Необхідно для маніпуляції данними
library(Benchmarking) # Необхідно для аналізу ефективності

# Ініціалізація даних
data <- data.frame(firms = c(LETTERS[1:7]),
                   output = c(1, 1, 2, 3, 2, 1, 4),
                   input1 = c(2, 3, 2, 6, 6, 8, 20),
                   input2 = c(5, 2, 4, 6, 2, 2, 4),
                   stringsAsFactors = FALSE)

# Створення нових змінних
data$input1_per_output <- data$input1 / data$output
data$input2_per_output <- data$input2 / data$output



# Calculating simplest efficiency example -------------------------------

eff <-
  dea(
    Y = as.matrix(data$output),
    X = as.matrix(data[, c("input1", "input2")]),
    RTS = "crs",
    ORIENTATION = "in", 
    SLACK = TRUE
  )

# Output of the dea function is an extensive one
str(eff)

# Obtaining separateley exxiciency scores
eff$eff
eff(eff)
print(eff)

# Summarising efficiency scores
summary(eff)

# Obtaining lambda scores
eff$lambda
lambda(eff)

# Obtain slacks
eff$sx

# Constructing the COELLI output table from the page 167 (table 6.2)
data %>% 
  select(firms, output) %>% 
  bind_cols(data.frame(eff = eff(eff)),
            as.data.frame(eff$lambda),
            data.frame(slack = eff$sx))

# Example 2 ---------------------------------------------------------

ex2data <-
  data.frame(firms = c(letters[1:5]),
             output = c(1,2,3,4,5),
             input1 = c(2,4,3,5,6),
             stringsAsFactors = FALSE)


# Caclculating scale efficiency -------------------------------------

ex2effCRS <-
  dea(
    Y = as.matrix(ex2data$output),
    X = as.matrix(ex2data[, c("input1")]),
    RTS = "crs",
    ORIENTATION = "in", 
    SLACK = TRUE
  )

ex2effVRS <-
  dea(
    Y = as.matrix(ex2data$output),
    X = as.matrix(ex2data[, c("input1")]),
    RTS = "vrs",
    ORIENTATION = "in", 
    SLACK = TRUE
  )

# Combining efficiency scores and calculating the return to scale.
ex2data %>% 
  select(firms, output) %>% 
  bind_cols(data.frame(eff_crs = eff(ex2effCRS),
                       eff_vsr = eff(ex2effVRS),
                       se = eff(ex2effCRS) / eff(ex2effVRS))) 

# Trying additional functionality of the Benchmarking package ----------
dea.plot(
  y = as.matrix(data$output),
  x = as.matrix(data[, c("input1", "input2")]),
  txt = as.matrix(data$firms),
  RTS = "crs",
  ORIENTATION = "in"
)

dea.plot.frontier(
  y = as.matrix(data$output),
  x = as.matrix(data[, c("input1", "input2")]),
  txt = as.matrix(data$firms),
  RTS = "vrs"
)

dea.plot.frontier(
  y = as.matrix(data$output),
  x = as.matrix(data[, c("input1", "input2")]),
  txt = as.matrix(data$firms),
  RTS = "crs"
)

dea.plot.isoquant(
  x1 = as.matrix(data[, c("input2")]),
  x2 = as.matrix(data[, c("input1")]),
  txt = as.matrix(data$firms),
  RTS = "crs"
)

dea.plot.transform(
  y1 = as.matrix(data[, c("input2")]),
  y2 = as.matrix(data[, c("input1")]),
  txt = as.matrix(data$firms),
  RTS = "crs"
)



