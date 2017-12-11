##################################################
# R script: Tutorial1.R
# Project: Studio 1

# Date: 21/Aug/2017
# Author: Bazil Muzaffar Kotriwala

# Purpose: Tutorial 1 Solution

##################################################

# Q2 (Summary, Boxplot & Hypothesis Testing)

pacific_ocean = c(209, 48, 169, 138, 64, 97, 161, 95, 145, 90, 121, 80, 56, 64, 209, 64, 72, 288, 322)
tasman_sea = c(76, 64, 68, 64, 37, 32, 32, 51, 56, 40, 64, 56, 80, 121, 177, 56, 80, 35, 72, 72, 108, 48)

summary(pacific_ocean)
summary(tasman_sea)

boxplot(pacific_ocean, tasman_sea)
t.test(pacific_ocean, tasman_sea, alternative = 'greater', conf.level = 0.99)

#Q3 (Scatterplot, Regression Model, Line of best fit)

MetaCarp = c(45, 51, 39, 41, 48, 49, 46, 43, 47)
Stature = c(171, 178, 157, 163, 172, 183, 173, 175, 173)
plot(MetaCarp, Stature)
fitted = lm(Stature ~ MetaCarp)
abline(fitted)
summary(fitted)

#Q4 (Reading data directly, mean, sd, histogram)

kiama = read.table("http://www.statsci.org/data/oz/kiama.txt", header = TRUE)
View(kiama)
kiama_col = kiama$Interval
mean(kiama_col, na.rm = TRUE)
sd(kiama_col, na.rm = TRUE)
hist(kiama_col, main = "Histrogram for Kiama", xlab = "Time Interval", col = "Green", border = "Blue", xlim = c(0, 200), ylim = c(0,30), las = 1)

#Q5a (Predictor of rigidity)

timber = read.table("http://www.statsci.org/data/oz/timber.txt", header = TRUE)
View(timber)
cor(timber)

#Q5b (Regression equation, scatterplot, fitted model)

attach(timber)
fitted = lm(Rigid ~ Dens)
plot(Dens, Rigid)
abline(fitted)
summary(fitted)

#Q5c (Multiple Regression equation)

fitted = lm(Rigid ~ Dens + Elast)
summary(fitted)

#Q6 (Boxplot, aggregate func)

InvestA = read.csv("C:/Users/Bazil Muzaffar/Google Drive/Semester 5 July 2017 Course Material/FIT3152 - Data Analytics/Week 1/InvestA.csv")
attach(InvestA)
View(InvestA)
boxplot(Group, FV, las = 2, names = c("Group", "FV"))
aggregate(InvestA[2], InvestA[1], mean)

#Q7 (Time series analysis)

retail_turnover = read.table(file = "clipboard", sep = '\t', header = TRUE)
turnoverTS = ts(retail_turnover[2], frequency = 12, start = c(2000, 1), end = c(2010, 12))
turnoverTS
plot(turnoverTS)
decomposeTurnover = decompose(turnoverTS)
decomposeTurnover
plot(decomposeTurnover)