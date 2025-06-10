### Part I working alone

# This is a .R file, used for writing code in R
# Lines that begin with a "#" are comments, and can contain non-code sentences.

# library() is a function to load in a set of R functions contained in the library
# The tidyverse library loads many using packages into memory.
# The broom library contains functions to convert output into tidy tables
# If this is your first time loading these libraries, you will first need to install them.
# Do this using by un-commenting the `install.packages()` lines of code and executing them. 
# You only ever need to install an R package once (or when you re-install R).
# You need to re-load a library whenever you start a new R session.

#install.packages("tidyverse")
library(tidyverse)
#install.packages("broom")
library(broom)

# Data import
le_data <- read_csv("./data/Life-expectancy-by-state-long.csv")

# Run a regression model with time coded using a linear term
mod1 <- lm(LE ~ year, dat = le_data %>% filter(state == "California", race == "black", sex == "Female"))

# Regression output
tidy(mod1)

# Plot the results
ggplot(data = le_data %>% filter(state == "California", race == "black", sex == "Female"),
       aes(x = year, y = LE)) + 
  geom_point() +
  theme_bw() +
  geom_abline(intercept = -232.4959533, slope = 0.1544538)

# Save the plot
ggsave(plot = last_plot(), filename = "./images/ca-black-women-LE.png",
       width = 5, height = 5,device = "png"
        )