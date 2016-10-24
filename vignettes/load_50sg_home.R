# Завантаження даних за формою 50 СГ із бази даних до середовища R.
# Попередньо, ми вже завантажили ці данні у R File.
# Now we will only load data to the environment and proceed with it as it is.


# DESCRIPTION OF THE AGRICULTURAL FORMS IS ON THIS WEBSITE http://buhgalter911.com/Res/Blanks/Stat/statselxoz.aspx

# Libraries 
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Loading data to the environment 
load("E:/Google Drive/R_Code/50SG/data/all_forms.Rdata")



# Exploring data ---------------------------------------------------------------

# str(sgData)

# Here we explain main forms used in the data, which we are investigating.
# sgData$forma %>% unique()

# Data contains the follwing forms:
# "21"        - Форма 21 - РЕАЛІЗАЦІЯ СІЛЬСЬКОГОСПОДАРСЬКОЇ ПРОДУКЦІЇ річна
# "24"        - Форма 24 - СТАН ТВАРИННИЦТВА річна
# "50"        - Форма 50сг - ОСНОВНІ ЕКОНОМІЧНІ ПОКАЗНИКИ РОБОТИ СІЛЬСЬКОГОСПОДАРСЬКИХ ПІДПРИЄМСТВ
# "finance_b" - ????
# "finance_s" - ????

# Years coverage of data is 
# sgData$god %>% unique()

# Each form has form specific codes
# sgData$code %>% unique()

# In addition, since data is demanded in the wide format, each code may correspond 
#   to one or more variables (Columns)
# sgData$var %>% unique()


# Selecting only data from the form 50 sg and only for one year to --------

# data <- sgData %>% filter(god == 2012, forma == "50")
# rm(sgData)