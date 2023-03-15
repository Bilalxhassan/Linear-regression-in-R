getwd()
setwd("C:/Users/Bilal/OneDrive/Desktop/Study folder/Data Analytics/Rassignment 2")
install.packages("spdep")
install.packages("fastDummies")
install.packages("olsrr")
install.packages("modelr")

library(tidyverse)
library(openxlsx)
library(readxl)
library(spdep)
library(fastDummies)
library(olsrr)
library(modelr)
library(knitr)

data <- read.xlsx("Airbnb_listings.xlsx")
data <- data %>% select(price, beds,host_neighbourhood,
                        latitude, longitude, neighbourhood_group_cleansed)
data <- data %>% mutate(price=gsub("\\$","",price),
                        price=gsub("\\,","",price),
                        price=as.numeric(price))
  
data <- data %>% na.omit()

data <- data %>% dummy_cols(select_columns="host_neighbourhood")

data <- data %>% na.omit()
data%>%head


set.seed(25)
new.data <- data %>% sample_n(size=1000)

model <- price~beds + host_neighbourhood_
reg.results <- new.data %>% lm(formula=model)

reg.results %>% ols_vif_tol()

reg.results %>% ols_plot_cooksd_chart()

to remove potential outliers



new.data <- new.data %>% slice(-522,-810,-936)
reg.results.out <- new.data %>% lm(formula=model)
reg.results.out %>% ols_plot_cooksd_chart()




new.data <- new.data %>% mutate(price= ifelse(price==0, 1, price))


model <- log(price)~beds+host_neighbourhood_
reg.results.out <- new.data %>% lm(formula=model)
reg.results.out %>% ols_test_normality()

reg.results.out %>% ols_test_breusch_pagan()

model <- log(price) ~ beds^2+host_neighbourhood 
reg.results.out <- new.data %>% lm(formula=model)
reg.results.out %>% ols_test_breusch_pagan()


new.data <- new.data %>% add_residuals(reg.results.out, var="Residuals")
coords <- new.data %>% select(longitude, latitude)
coordinates(coords) <- ~ longitude + latitude
neig <-knearneigh(coords,5,longlat = T)
neig <- knn2nb(neig)
neig <- nb2listw(neig,zero.policy=T)

new.data %>% pull(Residuals) %>%
  moran.test(listw=neig, zero.policy = T, na.action = na.omit)

new.data <- new.data %>% mutate(beds.lag=lag.listw(var=beds,x=neig,zero.policy = T))
model <- log(price)~beds^2+host_neighbourhood+ beds.lag
reg.results.out <- new.data %>% lm(formula=model)
new.data <- new.data %>% add_residuals(reg.results.out, var="Residuals2")

  new.data %>% pull(Residuals2) %>%
    moran.test(listw=neig, zero.policy = T, na.action = na.omit)




new.data[new.data$price ==0] <- 1
