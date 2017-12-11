##############################################
# R Script: Assignment # 1
# Project: WebForum (Social and linguistic dynamics of an online community)

# Authors: Bazil Muzaffar Kotriwala, Siddharth Anil Shinde
# Timestamp: 3:00PM 05-Sep-2017

# Purpose: Cleaning, manipulation, visualisation and analysis of data

##############################################

# Installing packages

install.packages("ggplot2")
library("ggplot2")
install.packages("plotly")
library(plotly)

# Read dataset
#setwd("C:/Users/Siddharth/Desktop/Uni/3152")
#webforum = read.csv("webforum.csv")
webforum = read.csv("C:/Users/Bazil Muzaffar/Google Drive/Semester 5 July 2017 Course Material/FIT3152 - Data Analytics/Assignments/Assignment 1/webforum.csv")
View(webforum)

# Making a copy of dataset for manipulation, so original dataset remains unaffected

webforum_copy = webforum
View(webforum_copy)

# 1. Preliminiary Analysis

ncol(webforum_copy)
str(webforum_copy)
summary(webforum_copy)

# 2. Cleaning Data

# 2.0 Removing all posts with wordcount = 0 from dataset, since they are not useful to us (as they are just images/videos)

webforum_clean_WC = webforum_copy[(webforum_copy$WC != 0),]
write.csv(webforum_clean_WC, file = "webforum_clean_WC.csv")
webforum_WC_0 = webforum_copy[(webforum_copy$WC == 0),]
head(webforum_WC_0)
nrow(webforum_WC_0)
nrow(webforum_clean_WC) 

# 2.1 Removing all the data collected of anonymous authors, (author_id = -1) Not useful to us, since we don't know whose interacting

webforum_clean_WC_anon = webforum_clean_WC[(webforum_clean_WC$AuthorID != -1),]
write.csv(webforum_clean_WC_anon, file = "webforum_clean_WC_anon.csv")
webforum_anon = webforum_clean_WC[(webforum_clean_WC$AuthorID == -1),]
head(webforum_anon)
nrow(webforum_clean_WC_anon)
nrow(webforum_anon)

# 3.0 Similar Language Analysis

# 3.1 Find all threads with their total word counts

thread_wc_tot = aggregate(webforum_clean_WC_anon[6], webforum_clean_WC_anon[2], sum)
write.csv(thread_wc_tot, file = "thread_wc_tot.csv")
View(thread_wc_tot)

# 3.2 Extracting the data for top four word count threads

thread_wc_tot_top4 = thread_wc_tot[order(thread_wc_tot$WC, decreasing = TRUE),]
thread_wc_tot_top4 = head(thread_wc_tot_top4, 4)
write.csv(thread_wc_tot_top4, file = "thread_wc_tot_top4.csv")

# 3.3 We construct a subset of all the data of the 4 specific threads with the max number of words

top4_WC_data = function(thread_wc_tot_top4){
  thread_max_wc_ds = webforum_clean_WC_anon[(webforum_clean_WC_anon$ThreadID == thread_wc_tot_top4[1,1]),]
  for (i in 2:length(thread_wc_tot_top4[,2]))
    thread_max_wc_ds <- rbind(thread_max_wc_ds, webforum_clean_WC_anon[(webforum_clean_WC_anon$ThreadID == thread_wc_tot_top4[i,1]),])
  return(thread_max_wc_ds)
}

thread_max_wc_data = top4_WC_data(thread_wc_tot_top4) 
View(top4_WC_data(thread_wc_tot_top4))
write.csv(thread_max_wc_data, file = "thread_max_wc_data.csv")

# 3.4 Taking out the mean of all columns each author in that specific thread

thread_max_wc_data_mean = aggregate(thread_max_wc_data[6:32], thread_max_wc_data[2:3], mean)
attach(thread_max_wc_data_mean)

# 3.5 Renaming all the columns in the dataset once the mean is found

rename_col = function(thread_max_wc_data_mean){
  for (i in 3:ncol(thread_max_wc_data_mean)){
    colnames(thread_max_wc_data_mean)[colnames(thread_max_wc_data_mean)== colnames(thread_max_wc_data_mean[i])] = paste(colnames(thread_max_wc_data_mean[i]), "Mean", sep = " ")
    }

  return(thread_max_wc_data_mean)

}

thread_max_wc_data_mean = rename_col(thread_max_wc_data_mean)
head(thread_max_wc_data_mean)
write.csv(thread_max_wc_data_mean, file = "thread_max_wc_data_mean.csv")


# 3.5 Creating a barchart for the top four threads with each author's analytical mean plotted

attach(thread_max_wc_data_mean)
View(thread_max_wc_data_mean)
g <- ggplot(data=thread_max_wc_data_mean, aes(x=AuthorID, y = `Analytic Mean`)) +
  geom_bar(stat="identity", color="blue", position=position_dodge(width = 2)) +
  theme_minimal() + facet_wrap(~ThreadID)
g

# 3.6 Finding the thread with the max word count

thread_wc_tot_no1 = thread_wc_tot_top4[which.max(thread_wc_tot_top4[,2]),]

# 3.7 Creating subset of all the data of the thread with the highest word count

thread_max_wc_data_no1_mean = thread_max_wc_data_mean[(thread_max_wc_data_mean$ThreadID == thread_wc_tot_no1$ThreadID),]
write.csv(thread_max_wc_data_no1_mean, file = 'thread_max_wc_tot_no1_mean.csv')
head(thread_max_wc_data_no1_mean)
View(thread_max_wc_data_no1_mean)

# 3.8 Creating a multivariate graph for the thread with the highest word count 
# Using plot_ly to create the multivariate graphs
# One graph for negemo, posemo and affect
# One graph for anger and anxiety

attach(thread_max_wc_data_no1_mean)

anger_anx <- plot_ly(thread_max_wc_data_no1_mean, y = ~`anger Mean`, type = 'scatter', mode = 'lines', name = 'anger') %>%
  add_trace(y = ~`anx Mean`, name = 'anx', mode = 'lines+markers') %>%
  layout(yaxis = list(title = 'Mean %'), xaxis = list(title = 'Author Count'), barmode = 'stack')
anger_anx

aff_pos_neg <- plot_ly(thread_max_wc_data_no1_mean, y = ~`posemo Mean`, type = 'bar', mode = 'lines', name = 'posemo') %>%
  add_trace(y = ~`negemo Mean`, name = 'negemo', mode = 'lines+markers') %>%
  layout(yaxis = list(title = 'affect %'), xaxis = list(title = 'Author Count'), barmode = 'stack')
aff_pos_neg

##############
# Pre-processing for Time Series Graphs

# Installing required packages

install.packages("ggseas")
install.packages("seasonal")
library(seasonal)
library(ggseas)

# Yearly Time Series Graph

# Compiling top 2 max word count threads with coninciding timelines

ts_thread_127115_145223 = thread_wc_tot_top4[3:4,]
TS_thread_127115_145223 = c()
for (i in 1:length(ts_thread_127115_145223[,2]))
  TS_thread_127115_145223 <- rbind(TS_thread_127115_145223, webforum_clean_WC_anon[(webforum_clean_WC_anon$ThreadID == ts_thread_127115_145223[i,1]),])

# Converting Date column format to Date format

TS_thread_127115_145223$Date <- as.Date(TS_thread_127115_145223$Date, "%Y-%m-%d")

# Display head and tail of the subset

head(TS_thread_127115_145223)
tail(TS_thread_127115_145223)

# Plotting a time series graph for the paired threads

gg <- ggplot(TS_thread_127115_145223, aes(Date, Clout, color = "blue")) +
  geom_line()+ geom_point(aes(color = "red"))  +theme_minimal() + facet_wrap(~ThreadID)
gg


# Weekly Time Series Graph

# Comiling top 2 max word count threds with coinciding timelines 

ts_thread_252620_254138 = thread_wc_tot_top4[1:2,]

TS_thread_252620_254138 = c()
for (i in 1:length(ts_thread_252620_254138[,2]))
  TS_thread_252620_254138 <- rbind(TS_thread_252620_254138, webforum_clean_WC_anon[(webforum_clean_WC_anon$ThreadID == ts_thread_252620_254138[i,1]),])

# Converting Date column format to Date

TS_thread_252620_254138$Date <- as.Date(TS_thread_252620_254138$Date, "%Y-%m-%d")

# Removing extreme dates from the subset so as to ease plotting

TS_thread_252620_254138 <- TS_thread_252620_254138[!(as.Date(TS_thread_252620_254138$Date) == as.Date("2006-12-20") | as.Date(TS_thread_252620_254138$Date) == as.Date("2006-12-18")),]

# Display head and tail of the subset

head(TS_thread_252620_254138)
tail(TS_thread_252620_254138)

# Plotting a time series graph for the paired threads

gg <- ggplot(TS_thread_252620_254138, aes(Date, Clout, colour = "blue")) +
  geom_line() + geom_point(aes(color = "red")) +  facet_wrap(~ThreadID)
gg


# Decomposing Time Series Graph

# Creating a subset with the longest time line(Thread 127115) for decomposition

decompose_thread_127115 = webforum_clean_WC_anon[(webforum_clean_WC_anon$ThreadID == thread_wc_tot_top4[3,1]),]

# Finding the aggregate mean of posts falling on the same day

decomp_127115 = aggregate(decompose_thread_127115$Clout, by = list(decompose_thread_127115$Date), mean)

# Renaming columns

colnames(decomp_127115)[1] <- "Date"
colnames(decomp_127115)[2] <- "Mean clout"

# View head of new subset

head(decomp_127115)

# Creating a time series graph plot with a monthly frequency

decomp_ts_127115 <- tsdf(ts(decomp_127115$`Mean clout`, frequency = 12, start = c(2004,04,14), end = c(2011,11,11)))

# Plotting a decomposed graph from the above subset

g <- ggsdc(decomp_ts_127115, aes(x = x, y = y), method = "seas") + geom_line() + xlab("Date") +ylab("Clout") + geom_point(aes(color="red"))
g
