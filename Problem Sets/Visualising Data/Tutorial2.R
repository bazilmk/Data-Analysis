##################################################
# R Script: Tutorial2.R
# Project: Studio 2

# Author: Bazil Muzaffar Kotriwala
# Date: 23-Aug-2017

# Purpose: Tutorial 2 Solution

##################################################

# Q1 (Loading Library Lattice & Installing ggplot2)

library(lattice)
install.packages("ggplot2")
library(ggplot2)

# Q2a (Taking a random sample of the data)

set.seed(9999)
dsmall = diamonds[sample(nrow(diamonds), 1000), ]

# Q2b (Diamond Price is the dependent variable - Determing effect of 4 C's on diamond price)

attach(dsmall)

# Non - Linear Model

qplot(carat, price, data = dsmall, xlab = "Carat", ylab = "Diamond Price", color = color, size = clarity, alpha = cut)

# Linear Model (Use Logs on numeric variables)

qplot(log(carat), log(price), data = dsmall, xlab = "Carat", ylab = "Diamond Price", color = color, size = clarity, alpha = cut)

# Q3a

body_ds = read.csv("C:/Users/Bazil Muzaffar/Google Drive/Semester 5 July 2017 Course Material/FIT3152 - Data Analytics/Week 2/body.dat.csv")
attach(body_ds)

# Predicting height using all other body variables, categorised gender wise

by(body_ds, body_ds[25], function(ds) 
  round(cor(ds[1:23], ds[24]), digits = 2)) 

# Q3b

# Making a copy to remove the weight column

body_ds_copy = body_ds
body_ds_copy[23] = NULL

# Predicting weight using all other body variables, categorised gender wise

by(body_ds_copy, body_ds[25], function(ds)
  round(cor(body_ds_copy[1:23], body_ds[23]), digits = 2))

# Q3c (Finding highly correlated variables)

by(body_ds, body_ds[25], function(ds)
  round(cor(body_ds[1:24]), digits = 2))

# Q3d (Showing difference b/w men and women graphically - visualisation)

plot = qplot(ShoulderWidth, data = body_ds, geom = "histogram", facets = Gender ~ .)
plot = plot + ggtitle("Human Body Data") + theme(plot.title = element_text(hjust = 0.5))

# Q4 (Evaluating time between visits and amount spent)

# Shows duration between their visits and amount spent on each visit

dunhumby = read.csv("C:/Users/Bazil Muzaffar/Google Drive/Semester 5 July 2017 Course Material/FIT3152 - Data Analytics/Week 2/Dunhumby1-20.csv")
attach(dunhumby)
plot = qplot(visit_delta, visit_spend, data = dunhumby, geom = "point", facets = customer_id ~ .)
plot = plot + facet_wrap(~ customer_id, ncol = 4)