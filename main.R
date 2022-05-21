require(stringr)
# Required for melt() and cast() function
require("reshape2")
require("reshape")
require("dplyr")

library("ggplot2")
library("gridExtra")
library("gapminder")

#
library(stringr)
library(reshape2)
library(reshape)
library(dplyr)

library(ggplot2)
library(gridExtra)
library(gapminder)

setwd('./')
# Load Data
# Data detail on https://www.kaggle.com/code/harshitshankhdhar/eda-on-imdb-movies-dataset/notebook
#imdbData_URL = 'https://storage.googleapis.com/kagglesdsdata/datasets/1131493/1898721/imdb_top_1000.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20220408%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20220408T101610Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=83aa8be0b4dc3b486679aa19574752dd85333fac39622230b32346cb4d1d1b6659a4c4e67eb73c408c27599f6fdc5bafe94c3f2d935c3713581a0d3b93c4d7ccbc4646e4f16dc0c09320c9dc4b5bf87e9976174215db3e3ba7ac2ec6dec7558bed9bde0f7b1d8bb8d8e8a4e6602c430497cdda38db5ba76d8dd90b99f8eaa8ff34ae75377331970f4f4216ce28123649cbcfb7eebc8d22bf0f11de701da02e3defcb3cc32e6378c47609f20ecb5af4686f34d971548da0505f7f49e2c721e24d0ccf0415e5766a4e475066bfcd381f51e63a29a9b59b985cfb74d1abe8f3dca11a685081906ebe875b8b26cac2c0042e4819876d6c1b5f02a32d01ffe23ea254'
cat("Load data from csv", '\n')
imdbData = read.csv('./data/imdb_top_1000.csv', header = TRUE)
dim(imdbData)
# filter data
cat("Filter columns of data", '\n')
columns_1 = names(imdbData)
#View(imdbData)
# reshape data
avail_names = c("Series_Title", "Released_Year", "Certificate", "Runtime", "Genre", "IMDB_Rating", "Meta_score", "No_of_Votes", "Gross")
imdb_filter <- imdbData[, avail_names]

# reshape data of Genre colum
genre_list = str_split_fixed(imdb_filter$Genre, ", ", 3)
colnames(genre_list) <- c("Genre_1", "Genre_2", "Genre_3")
cat("List new columns \n", colnames(genre_list), "\n")
df1 = cbind(imdb_filter, genre_list)
cat("Gross is numeric", is.numeric(df1$Gross), '\n')
# change Gross to numberic
cat("Change value of Gross to numeric", '\n')
df1$Gross = strtoi(str_remove_all(df1$Gross, ","))
cat("Gross is numeric", is.numeric(df1$Gross), '\n')
# sort data
imdb_sorted <- df1[order(df1$Released_Year),]
# View(imdb_sorted)
melt_data <- melt(imdb_sorted, id = c("Series_Title", "Released_Year", "Certificate", "Runtime", "Genre", "IMDB_Rating", "Meta_score", "No_of_Votes", "Gross"), value.name = "Value")
melt_data <- melt_data[order(melt_data$Released_Year),]
# View(melt_data)

##
# Analysis of the relation between Genre and Gross
#
##
# Gross_by_Genre <- aggregate(melt_data$Gross, list(melt_data$value), FUN = sum, na.rm = TRUE, na.action = NULL)
# Gross_by_Genre$Group.1[Gross_by_Genre$Group.1 == ""] <- "Others"
melt_data$sum <- melt_data$Gross / 10^6
boxplot(melt_data$sum ~ melt_data$value, melt_data, main = "Movie grosses by genre", boxwex = 0.5, col = c("orange", "yellow"), xlab = "Genre", ylab = "Movie grosses", ylim(min(melt_data$sum), max(melt_data$sum)))

## data for the last 3 years
last3Year = tail(unique(sort(as.numeric(melt_data$Released_Year))), 3)
last_3_year_data = filter(melt_data, melt_data$Released_Year %in% last3Year)
money_by_genre <- aggregate(last_3_year_data$sum, list(last_3_year_data$value), FUN = sum, na.rm = TRUE)
money_by_genre <- money_by_genre[order(money_by_genre$x),]
top_3_Genre = tail(money_by_genre$Group.1, 3)
##
top_genre_3_year_data = filter(last_3_year_data, last_3_year_data$value %in% top_3_Genre)

p1 <- ggplot(top_genre_3_year_data, aes(x = value, y = sum, fill = factor(value)), xlab(Genre)) +
  facet_wrap(~Released_Year) +
  geom_boxplot() +
  xlab("Genre") +
  ylab("Money")

p2 <- ggplot(data = top_genre_3_year_data, mapping = aes(x = value, y = sum, color = Released_Year)) +
  geom_point() +
  geom_smooth(method = "gam", size = 2) +
  xlab("Genre") +
  ylab("Money")

p3 <- ggplot(data = top_genre_3_year_data, mapping = aes(x = value, y = sum, fill = value)) +
  facet_wrap(~Released_Year) +
  geom_histogram(stat = 'identity') +
  xlab("Genre") +
  ylab("Money")

grid.arrange(p1, p2, p3, nrow = 2)