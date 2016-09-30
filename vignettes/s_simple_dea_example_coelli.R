library(plyr)
library(dplyr)
library(tidyr)
library(Benchmarking)

# Here we are reproducing simple examples of DEA frim COELLI book.

# Example one - page 165 --------------------------------------------------

# Initializing basic data
ex1data <-
  data.frame(firms = c(letters[1:5]),
             output = c(1,2,3,1,2),
             input1 = c(2,2,6,3,6),
             input2 = c(5,4,6,2,2),
             stringsAsFactors = FALSE) %>% 
  mutate(input1_per_output = input1 / output, 
         input2_per_output = input2 / output)

# Plotting initialized data
plot(x = ex1data$input1_per_output,
     y = ex1data$input2_per_output,
     type = "p",
     main = "Firms plot",
     xlab = "input 1 peroutput",
     ylab = "input 2 peroutput")


# Calculating simplest efficiency example -------------------------------
ex1eff <-
  dea(
    Y = as.matrix(ex1data$output),
    X = as.matrix(ex1data[, c("input1", "input2")]),
    RTS = "crs",
    ORIENTATION = "in", 
    SLACK = TRUE
  )

# Output of the dea function is an extensive one
str(ex1eff)

# Obtaining separateley exxiciency scores
ex1eff$eff
eff(ex1eff)
print(ex1eff)

# Summarising efficiency scores
summary(ex1eff)

# Obtaining lambda scores
ex1eff$lambda
lambda(ex1eff)

# Obtain slacks
ex1eff$sx

# Constructing the COELLI output table from the page 167 (table 6.2)
ex1data %>% 
  select(firms, output) %>% 
  bind_cols(data.frame(eff = eff(ex1eff)),
            as.data.frame(lambda(ex1eff)),
            data.frame(slack = ex1eff$sx))

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
  y = as.matrix(ex1data$output),
  x = as.matrix(ex1data[, c("input1", "input2")]),
  txt = as.matrix(ex1data$firms),
  RTS = "crs",
  ORIENTATION = "in"
)

dea.plot.frontier(
  y = as.matrix(ex1data$output),
  x = as.matrix(ex1data[, c("input1", "input2")]),
  txt = as.matrix(ex1data$firms),
  RTS = "vrs"
)

dea.plot.frontier(
  y = as.matrix(ex1data$output),
  x = as.matrix(ex1data[, c("input1", "input2")]),
  txt = as.matrix(ex1data$firms),
  RTS = "crs"
)

dea.plot.isoquant(
  x1 = as.matrix(ex1data[, c("input2")]),
  x2 = as.matrix(ex1data[, c("input1")]),
  txt = as.matrix(ex1data$firms),
  RTS = "crs"
)

dea.plot.transform(
  y1 = as.matrix(ex1data[, c("input2")]),
  y2 = as.matrix(ex1data[, c("input1")]),
  txt = as.matrix(ex1data$firms),
  RTS = "crs"
)



