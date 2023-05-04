

setwd("C:/Users/Hyewon Kwon")

execution <- read.csv('execution_clean.csv')
literacy <- read.csv("literacy_clean.csv")

execution <- na.omit(execution)
literacy <- na.omit(literacy)

democracy <- read.csv('democracy_clean.csv')
fragile <- read.csv('fragile.csv')

execution <- execution[, c('country', 'region', 'year', 'population', 'latest.execution', 'executed')]


library(tidyverse)
my_df <- execution %>%
        full_join(literacy) %>%
        full_join(democracy) %>%
        full_join(fragile)

my_df

# Generalized Linear Model (GLM)
library(stargazer)
model1 <- glm(executed ~ fragile.index + electoral.democracy + literacy.rate, data = my_df)
stargazer(model1, type = 'latex', out = 'model1.html', intercept.bottom = F, intercept.top=T,
          omit.stat = c('bic'), no.space = T)


# Fixed effect Linear Model
library(plm)
model2 <- plm(executed ~ fragile.index + electoral.democracy + literacy.rate, data = my_df, model = 'within')
stargazer(model2, type = 'latex', out = 'model2.html', intercept.bottom = F, intercept.top=T,
          omit.stat = c('bic'), no.space = T)
