library(tidyverse)

# Example 1: The Anscombe data set

library(readxl)
Anscombe <- read_excel("~/Google Drive/UU PMST/MST 6600 - Advanced Statistical Techniques/NIST Engineering Statistics Handbook 1-1-6 EDA Graphics Example/data/Anscombe.xlsx")
View(Anscombe)

# create a simple scatter plot of X1 vs Y1
plot(Anscombe$X1, Anscombe$Y1)

A1.mod <- lm(Anscombe$Y1 ~ Anscombe$X1)
summary(A1.mod)
plot(A1.mod)


# create a simple scatter plot of X2 vs Y2
plot(Anscombe$X2, Anscombe$Y2)

A2.mod <- lm(Anscombe$Y2 ~ Anscombe$X2)
summary(A2.mod)
plot(A2.mod)

# create a single page of all values
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(Anscombe$X1, Anscombe$Y1)
plot(Anscombe$X2, Anscombe$Y2)
plot(Anscombe$X3, Anscombe$Y3)
plot(Anscombe$X4, Anscombe$Y4)
par(mfrow=c(1,1)) # Change back to 1 x 1

# Making changes to graphs
# create a simple scatter plot of X1 vs Y1 with some lables
plot(Anscombe$X1, Anscombe$Y1, main="DATA SET 1", xlab="X1", ylab="Y1")

##############################################

# Where we are going next week with ggplot2
# Example 2. The Anscobe Data set with ggpot2
# I need to tidy up the data.
Anscombe.tidy.xy1 <- Anscombe %>% select("X1", "Y1") %>%
  gather('X1', 'Y1', key = "Coordinate", value = "value") %>% 
  mutate(dataset = "DataSet1")

Anscombe.tidy.xy2 <- Anscombe %>% select("X2", "Y2") %>%
  gather('X2', 'Y2', key = "Coordinate", value = "value") %>% 
  mutate(dataset = "DataSet2")

Anscombe.tidy.xy3 <- Anscombe %>% select("X3", "Y3") %>%
  gather('X3', 'Y3', key = "Coordinate", value = "value") %>% 
  mutate(dataset = "DataSet3")

Anscombe.tidy.xy4 <- Anscombe %>% select("X4", "Y4") %>%
  gather('X4', 'Y4', key = "Coordinate", value = "value") %>% 
  mutate(dataset = "DataSet4")

# combine the tables
Anscombe.tidy.data <- 
  bind_rows(Anscombe.tidy.xy1, Anscombe.tidy.xy2, Anscombe.tidy.xy3, Anscombe.tidy.xy4) %>%
  mutate(XY = )
  


# Example 3: Slide:ology Chapter 4 - Displaying Data 

# library(readxl)
truth <- read_excel("~/Google Drive/UU PMST/MST 6600 - Advanced Statistical Techniques/Displaying Data Examples.xlsx")
View(truth)

# I need to tidy up the data.
truth.tidy <- gather(truth, 'R&D', 'Sales', 'Management', 'Accounting', key = "Department", value = "USD_Millions")
View(truth.tidy)

# A very clean, informative set of charts
ggplot(data = truth.tidy) +
  geom_col(mapping = aes(x = Category , y = USD_Millions)) +
  coord_flip() +
  facet_grid(Department ~ .) +
  theme_bw()

# simplified by department
ggplot(data = truth.tidy) +
  geom_col(mapping = aes(x = Department , y = USD_Millions)) +
  coord_flip() +
  theme_bw()

#simplified by category
ggplot(data = truth.tidy) +
  geom_col(mapping = aes(x = Category , y = USD_Millions)) +
  coord_flip() +
  theme_bw()

# Now to get it in a better order!
ggplot(data = truth.tidy) +
  geom_col(mapping = aes(x = reorder(Category, USD_Millions) , y = USD_Millions)) +
  coord_flip() +
  theme_bw()
