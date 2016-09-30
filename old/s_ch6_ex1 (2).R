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

# Ініціалізація даних у вигляді дата фрейму (data frame)
data <- data.frame(firms = c(LETTERS[1:7]),
                   Q = c(1, 1, 2, 3, 2, 1, 2),
                   X1 = c(2, 3, 2, 6, 6, 8, 10),
                   X2 = c(5, 2, 4, 6, 2, 2, 2),
                   stringsAsFactors = FALSE)

# Розрахунок нови змінних
data$input1_per_output <- data$X1 / data$Q
data$input2_per_output <- data$X2 / data$Q

# Простий приклад ефективності із згенерованих даних
# Для того, щоб функція `dea()`, яку ми будемо використовувати, 
#     парцювала, нам необхідно подати вхідні параметри у вигляді
#     матриці. Тому, для параметрів `Y` та `X` ми перетворюємо 
#     данні із дата фрейму у матрицю.
#   За допомогою параметру `RTS` ми визначаємо вплив ефекту 
#     масштабу на ефективність досліджуваних підприємтсв. 
#   Параметр `ORIENTATION` визначає тип ефективності, яку ми
#     шукаємо. Значення цього параметру "in" означає, що ми  
#     шукаємо вхід-орієнтовану ефективність.
#   Параметр `SLACK` визначає ви необхідно у резульатах 
#     виконання функції розраховувати слак. Значення  
#     параметру `TRUE` означає, що значення слаків будуть
#     розраховані у результатах функції.
eff <-
  dea(
    Y = as.matrix(data$Q),
    X = as.matrix(data[, c("X1", "X2")]),
    RTS = "crs",
    ORIENTATION = "in", 
    SLACK = TRUE
  )

# За результатами використання функції `dea()`, ми створили
#     об'єкт `eff`, який містить усі результуючи атрибути функції.
#   Для того, щоб побачити, якими є ці атрибути, ми використаємо
#     функцію `str()`. Вона показую усе із чого складається об'єкт `eff`.
str(eff)

#   Із наведеного, нас цікавлять атрибути `eff` та `lambda`.
#   eff     -   це ефективності вірм наведені у тому порядку, в якому
#     вони знаходилися у вхідних даних
#   lambda  -   це значення скалярні значення лінійних комбінацій 
#     входів у вигляді дата фрейму.
#   sx      -   це значення слаків для кожног підприємства та кожного входу
#     у вигляді дата фрейму.
#   Для того, щоб подивитися значення кожного із окремих атрибутів, ми 
#     використаємо наступні команди
eff$eff
eff$lambda
eff$sx


# Побудуємо результуючу таблицю ефективностей та слаків.
eff_summary <- 
  bind_cols(data,
            data.frame(eff = eff$eff),
            data.frame(eff$lambda),
            data.frame(eff$sx))

eff_summary <- eff_summary[, c("firms", "eff", 
                               "L1", "L2", "L3", 
                               "L4", "L5", "L6", 
                               "L7", "sx1", "sx2")]


# END ---------------------------------------------------------------------


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



