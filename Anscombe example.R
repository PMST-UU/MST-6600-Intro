# Example 1: Load the Anscombe data set

library(readxl)
Anscombe <- read_excel("~/Google Drive/UU PMST/MST 6600 - Advanced Statistical Techniques/NIST Engineering Statistics Handbook 1-1-6 EDA Graphics Example/data/Anscombe.xlsx")
View(Anscombe)



# Where we are going next week with ggplot2
# Example 2: Slide:ology Chapter 4 - Displaying Data 

library(readxl)
truth <- read_excel("~/Google Drive/UU PMST/MST 6600 - Advanced Statistical Techniques/Displaying Data Examples.xlsx")
View(truth)

# Need to make the data "tidy"
library(tidyverse)

truth.tidy <- gather(truth, 'R&D', 'Sales', 'Management', 'Accounting', key = "Department", value = "USD_Millions")
View(truth.tidy)

ggplot(data = truth.tidy) +
  geom_col(mapping = aes(x = Category , y = USD_Millions))

           