##########################################################################################################################
# Script for Data Science Capstone Movielens Project (HarvardX PH125.9x)
# Author: Ravi Jha
# Date: 1 Dec 2019
# github: https://github.com/jha-r/Harvardx-PH125.9x-Capstone-MovieLens
##########################################################################################################################

##########################################################################################################################
#### Summary: 
# This script is a part of project, prepared to fulfill the completion requirement of 
# the HavardX PH125.9x Data Science Capstone course. This script is consistent with the MovieLens_Capstone_Report.Rmd

# Following are the Key code blocks in this script

# Importing the dataset and setup
# Spliting datasets
# Data wrangling
# Exploratory data analysis and visualization 
# Creating and tuning predective models
# Evaluating models based on validation dataset
# Reporting results 

##########################################################################################################################

###### START OF SCRIPT ###########


############################################################################################################
#### Data Import and Initial Setup ####
## Comments: This block consists of code to import and setup 10M movielens datase
############################################################################################################


# Install requested packages if not found
if(!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) 
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) 
  install.packages("dplyr")
if(!require(tidyr)) 
  install.packages("tidyr")


# Load libraries
library(tidyverse)
library(caret)
library(hexbin)
library(lubridate)
library(knitr)
library(kableExtra)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

datafile <- "MovieLens.RData"

# Check if datafile is already downloaded
if(!file.exists("MovieLens.RData"))
{
  temp <- tempfile()
  fileURL <- "http://files.grouplens.org/datasets/movielens/ml-10m.zip"
  
  destfile_ratings <- "./ml-10M100K/ratings.dat"
  destfile_movies <- "./ml-10M100K/movies.dat"
  
  # Execute only if destination data files - ratings.dat and movies.dat 
  # are not found on the current directory folder
  if(!file.exists(destfile_ratings) && !file.exists(destfile_movies))
  {
    # Download file from url to temp file
    download.file(fileURL, temp)  
    print("Downloading file to a temporary file")
    
    # Files to be extracted from the zip
    unzip_files= c("ml-10M100K/ratings.dat", "ml-10M100K/movies.dat")
    
    # Unzip files - ratings.data and movies.dat  
    unzip(temp, files=unzip_files)
    print("Unzipping temporary file to destination files")
    
    # Remove temp file
    unlink(temp)
  }
  
  
  # create dataset object from .dat files in split into defined columns
  ratings <- fread(text = gsub("::", "\t", readLines(destfile_ratings)),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  
  movies <- str_split_fixed(readLines(destfile_movies), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  
  # convert columns using mutate function 
  movies <- as.data.frame(movies) %>% 
    mutate(movieId = as.numeric(levels(movieId))[movieId],
           title = as.character(title), genres = as.character(genres))
  
  # join the both datasets into a new dataset by movieId
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Save movielens object to datafile
  save(movielens, file = datafile)
  
  # Remove unused objects from the memory
  rm(ratings, movies, temp)
  
} else {
  # Load the datafile if it already exists
  load(datafile)
}


##########################################################################################################
#### Spliting dataset: Training and Validation Sets ####
## Comments:
# In this code block, the dataset will first be split into edx and validation set (10%), 
# and then the edx set will then be further split into a train_set and test_set

##########################################################################################################

## to split datasets into edx, validation, train_set, and test_set

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead

# Partition the data set into edx and validation dataset. 
# The Validation set will be 10% of MovieLens data
test_index <- createDataPartition(y = movielens$rating, 
                                  times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
edx <- sample_n(edx, 1000) ## TBD

# Remove unused objects from the memory
rm(test_index, movielens, removed)

# Now, further split the edx into train_set and test_set
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, 
                                  times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test_set are also in train_set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test_set back into train_set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)    

# Remove unused objects from the memory    
rm(removed, temp, test_index) 


##########################################################################################################
#### Data Wrangling ####
# Comments:
# This block contains code for a brief data review and data wrangling as per the necessity.
##########################################################################################################

## Display first few rows to understand the data structure
head(train_set, 5)%>%
  # to format table with theme
  # to format table with theme
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed", "scale_down"))%>%
  row_spec(0, bold = T)




## Unique Movies, Users, and Genres

# To display unique values for movies, users, and genres columns
train_set %>% 
  summarise(uniq_movies = n_distinct(movieId),
            uniq_users = n_distinct(userId), 
            uniq_genres = n_distinct(genres))%>% 
  
  # to format table with theme
  # to format table with theme
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed"))%>%
  row_spec(0, bold = T)




## Summary and Average Rating

# Summary of the train_set dataset
summary(train_set)%>%
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed", "scale_down"))%>%
  row_spec(0, bold = T)



# to calculate ratings Mean mu
mu <- mean(train_set$rating)
print(mu)


## Ratings

# to list different rating scores 
uniq_ratings <- unique(train_set$rating)
uniq_ratings <- sort(uniq_ratings)
uniq_ratings



## Missing Data

# To find NA values in the dataset
anyNA(train_set)


#### Factorizing Variables

# to display class of userId and movieId column in a tabular form
cls <- matrix(c(class(train_set$userId), class(train_set$movieId)), ncol=2, byrow=TRUE)
colnames(cls) <- c("userId","movieId")
rownames(cls) <- c("Class")
cls%>%
  # to format table with theme    
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed"))%>%
  row_spec(0, bold = T)


## Tidying data ##


## Extracting Movie Release Year

# Extract year_released with brackets    
year_released <- str_extract(train_set$title, "\\(\\d{4}\\)$") 
# Remove brackets    
year_released <- str_remove_all(year_released, "[()]")  
# Save year as numeric
train_set$year_released <- as.numeric(year_released)
# to display first few rows with newly added columns
head(train_set, 5)%>%
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed", "scale_down")) %>%
  row_spec(0, angle = 45, bold = T)



## Date Rated and Year Rated - Converting 'timestamp' to date

# Convert timestamp to date_rated and extrat year as year_rated columns
train_set <- train_set %>% mutate(date_rated = as_datetime(timestamp), 
                                  year_rated = year(as_datetime(timestamp)))

# create a temporary dataset and remove timestamps column for displaying
train_set_dr <- subset(train_set, select = -c(timestamp) )
# to re-check the first few lines of the dataset
head(train_set_dr, 5)%>%
  # to display as a formatted table with the theme
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed", "scale_down")) %>%
  row_spec(0, angle = 45, bold = T) %>%
  footnote(general = "Column 'timestamp' is removed to accomodate 
                        table width on the page.")



## Movie Current Age and Age at the time of Review

# create movie_age by substracing current year to the year movie was released
# create age_at_review by substrating year the movie was rated and it was released
train_set <- train_set %>% mutate(movie_age = year(Sys.Date())-year_released, 
                                  age_at_review = year_rated - year_released)

# create a temporary dataset from training dataset
train_set_ma <- train_set

# Removing columns timestamp and date_rated from temporary dataset train_set_ma 
train_set_ma <- subset(train_set_ma, select = -c(timestamp, date_rated) )

# to display first few lines of the dataset after changes
head(train_set_ma, 5)%>%
  
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed", "scale_down"))  %>%
  row_spec(0, angle = 45, bold = T) %>%
  # to add foot note to the table
  footnote(general = "Columns 'timestamp' and 'date_rated' 
                        are removed to accomodate table width on the page.")



#######################################################################################
####  Exploratory Data Analysis and Visualization ####
# Comments:
# The block of code is to perform exploratory data analysis on the tidy data by utilizing visualization techniques.
#######################################################################################

## Ratings Distribution

# to plot rating against ratings count
train_set %>%
  # map rating variable to aesthetics of ggplot function
  ggplot(aes(rating)) +
  
  # to create a histogram
  geom_histogram(fill = "#00AFBB", binwidth = 0.25, color = "#00AFBB") +
  
  # scale axis to custom range and intervels
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  
  # to provide a title, x, and y axis labels
  xlab("Rating Star- 0.5 to 5.0") + 
  ylab("No. of ratings per Star Rating") +
  ggtitle("Rating distributions - Rating Star vs Number of Ratings ")



## Half-star Ratings

# plot to determine when the half-star raintgs started
train_set %>%
  # map date_rated and rating variables to aesthetics of ggplot function
  ggplot(aes(x = date_rated, y = rating)) +
  
  # to plot scattered data using geom_point
  geom_point(alpha = 0.5, col = "#00AFBB") +
  
  # scale axis to custom range and intervels
  scale_y_discrete(limits = c(seq(0.5,5,0.5)))  +
  
  # to provide a title, x, and y axis labels
  labs(title = "Year Rated vs Movie Rating", x = "Year Rated", y = "Movie Rating")



## Average Movie Rating variation over years

train_set %>% 
  # to plot graph for mean rating for individual year movies were rated
  group_by(year_rated) %>% 
  summarise(avg_rating = mean(rating), count = n()) %>% 
  
  # to map year_rated and mean rating variables to aesthetics of ggplot function
  ggplot(aes(x = year_rated, y = avg_rating)) +
  
  # to plot scattered data using geom_point
  geom_point(aes(size = count), alpha = 0.5, col = "#00AFBB") + 
  
  # to draw a smooth conditional mean line 
  geom_smooth(se = FALSE, colour = "#FC4E07", size = 1) +
  
  # to draw a horizontal line incepting y-axis at the mean rating 
  geom_hline(yintercept = mu, linetype = "dashed", color="#1380A1", size=1) + 
  
  # scale axis to custom range and intervels  
  scale_y_discrete(limits = c(seq(0.5,5,0.5)))  +
  
  # to provide a title, x, and y axis labels
  xlab("Year Rated") + 
  ylab("Average rating") + 
  labs(title = "Average Movie Rating Overtime") 



## Movie current age vs its average rating

train_set %>%
  # to plot graph for mean rating for individual year movies were rated
  group_by(movie_age) %>% 
  summarize(count = n(), avg_rating_by_age = mean(rating)) %>%
  
  # to map movie_age and mean rating by movie age variables to aesthetics of ggplot function
  ggplot(aes(movie_age, avg_rating_by_age)) +
  
  # to plot scattered data using geom_point
  geom_point(aes(size = count), alpha = 0.5, col = "#00AFBB") +
  
  # to draw a horizontal line incepting y-axis at the mean rating   
  geom_hline(yintercept = mu, linetype = "dashed", color="#1380A1", size=1) +
  
  # to draw a smooth conditional mean line   
  geom_smooth(se = FALSE, colour = "#FC4E07", size = 1) +
  
  # scale axis to custom range and intervels
  scale_y_discrete(limits = c(seq(0.5,5,0.5)))  +
  
  # to provide a title, x, and y axis labels
  xlab("Current Age of the movie") + 
  ylab("Movie average rating") +
  ggtitle("Movie Age vs Average Movie Rating")




## Movie current age vs No. of ratings received

# to plot Movie current age vs No. of ratings received
train_set %>%
  # to plot graph for mean rating for individual year movies were rated
  group_by(movie_age) %>% 
  summarize(count = n()) %>%
  
  # to map movie_age and count variables to aesthetics of ggplot function
  ggplot(aes(movie_age, count)) +
  
  # to plot scattered data using geom_point
  geom_point(alpha = 0.5, col = "#00AFBB", size = 2) +
  
  # scale axis to show non-scientific values (without exponential)
  scale_y_continuous(labels = scales::comma) +
  
  # to draw a smooth conditional mean line     
  geom_smooth(se = FALSE, colour = "#FC4E07", size = 1) +
  
  # to provide a title, x, and y axis labels
  xlab("Current Age of the movie") + 
  ylab("No. of rating") +
  ggtitle("Movie Age vs No. of Ratings")




## Users vs average rating given by the user

# userId vs average movie rating
train_set %>%
  
  # to plot graph for mean rating for individual user
  group_by(userId) %>% 
  summarize(avg_user_rating = mean(rating)) %>%
  
  # to map movie_age and mean rating by movie age variables to aesthetics of ggplot function
  ggplot(aes(userId, avg_user_rating, color = avg_user_rating)) +
  
  # to plot scattered data using geom_point
  geom_point(alpha = 0.5, size = 1) +
  
  # to draw a horizontal line incepting y-axis at the mean rating   
  geom_hline(yintercept = mu, linetype = "dashed", color="#1380A1", size=1) +  
  
  # scale axis to custom range and intervels  
  scale_y_discrete(limits = c(seq(0.5,5,0.5)))  +
  
  # to draw a smooth conditional mean line     
  geom_smooth(se = FALSE, colour = "#FC4E07", size = 1) +
  
  # to provide a title, x, and y axis labels  
  xlab("User") + 
  ylab("Average User Rating") +  
  ggtitle("User vs Average User Rating")




## Users vs No. of movies rated by user

# to plot a graph userId against average movie rating
train_set %>%
  
  # to plot graph for count of movies rated for individual userId
  group_by(userId) %>% 
  summarize(count = n()) %>%
  
  # to map userId and count variables to aesthetics of ggplot function
  ggplot(aes(userId, count)) +
  
  # to plot scattered data using geom_point
  geom_point(alpha = 0.5, col = "#00AFBB", size = 1) +
  
  # scale axis to show non-scientific values (without exponential)
  scale_y_continuous(labels = scales::comma) +
  
  # to draw a smooth conditional mean line   
  geom_smooth(se = FALSE, colour = "#FC4E07", size = 1) +
  
  # to provide a title, x, and y axis labels
  xlab("Users (User ID)") + 
  ylab("Movies rated by user") +  
  ggtitle("Users vs movies rated by user")




## Movie Age at the time of review vs Average Rating
# to plot a graph showing movie's age at the time of the revoew and its average rating    
train_set %>%
  
  # to plot mean rating graph for movies with age at review in years
  group_by(age_at_review) %>% 
  summarize(count = n(), avg_by_rating_age = mean(rating)) %>%
  
  # to map age at review and mean rating by age at review + 
  # variables to aesthetics of ggplot function
  ggplot(aes(age_at_review, avg_by_rating_age)) +
  
  # to plot scattered data using geom_point
  geom_point(aes(size = count), alpha = 0.5, col = "#00AFBB") +  
  
  # to draw a horizontal line incepting y-axis at the mean rating     
  geom_hline(yintercept = mu, linetype = "dashed", color="#1380A1", size=1) +  
  
  #scale axis to custom range and intervels
  scale_y_discrete(limits = c(seq(0.5,5,0.5)))  +  
  
  # to draw a smooth conditional mean line   
  geom_smooth(se = FALSE, colour = "#FC4E07", size = 1) +
  
  # to provide a title, x, and y axis labels
  xlab("Movie age at the time of review") + 
  ylab("Average Rating") +    
  ggtitle("Movie age at the time of review vs Average Rating")




## Movie Age at the time of review vs No. of Ratings

# to plot age at review vs rating count    
train_set %>%
  
  # to plot graph for rating count for movies' age at the time of the review
  group_by(age_at_review) %>% 
  summarize(count = n()) %>%
  
  # to map age at review and movie count variables to aesthetics of ggplot function    
  ggplot(aes(age_at_review, count)) +
  
  # to plot scattered data using geom_point
  geom_point(alpha = 0.5, col = "#00AFBB") +  
  
  # scale axis to show non-scientific values (without exponential)
  scale_y_continuous(labels = scales::comma) +
  
  # to draw a smooth conditional mean line       
  geom_smooth(se = FALSE, colour = "#FC4E07", size = 1) +
  
  # to provide a title, x, and y axis labels
  xlab("Movie age at the time of review") + 
  ylab("No. of Ratings") +    
  ggtitle("Movie age at the time of review vs number of times the movie rated")




## Movie Year Released vs its average rating

# to plot year released against average ratings      
train_set %>%
  
  # to plot graph for mean rating for individual year movies were released
  group_by(year_released) %>% 
  summarize(count = n(), avg_by_year_rel = mean(rating)) %>%
  
  # to map year released and mean rating by the year released +
  # variables to aesthetics of ggplot function
  ggplot(aes(year_released, avg_by_year_rel)) +
  
  # to plot scattered data using geom_point
  geom_point(aes(size = count), alpha = 0.5, col = "#00AFBB") +
  
  # to draw a horizontal line incepting y-axis at the mean rating   
  geom_hline(yintercept = mu, linetype = "dashed", color="#1380A1", size=1) +  
  
  #scale axis to custom range and intervels
  scale_y_discrete(limits = c(seq(0.5,5,0.5)))  +  
  
  # to draw a smooth conditional mean line   
  geom_smooth(se = FALSE, colour = "#FC4E07", size = 1) +
  
  # to provide a title, x, and y axis labels
  xlab("Year Movie Released") + 
  ylab("Movie Average Rating") +  
  ggtitle("Year Movie Released vs Average Rating")




## Movie Year Released vs number of ratings

# to plot year_released against count of the ratings      
train_set %>%
  
  # to plot graph for rating count against individual year movies were released
  group_by(year_released) %>% 
  summarize(count = n()) %>%
  
  # to map year released and movie count variables to aesthetics of ggplot function
  ggplot(aes(year_released, count)) +
  
  # to plot scattered data using geom_point
  geom_point(alpha = 0.5, col = "#00AFBB", size = 2) +
  
  # scale axis to show non-scientific values (without exponential)
  scale_y_continuous(labels = scales::comma) +
  
  # to draw a smooth conditional mean line       
  geom_smooth(se = FALSE, colour = "#FC4E07", size = 1) +
  
  # to provide a title, x, and y axis labels    
  xlab("Year Movie Released") + 
  ylab("No. of Ratings") +   
  ggtitle("Year Movie Released vs No. of Ratings")



## Movies Distribution

# to plot movie and the count of ratings recieved by a specific movie       
train_set %>%
  
  # to map movies count variables to aesthetics of ggplot function
  count(movieId) %>% 
  ggplot(aes(n)) + 
  
  # to plot graph as histogram with bin size of 30
  geom_histogram(fill = "#00AFBB", bins = 30, color = "#FC4E07") + 
  
  # scale axis to log 10    
  scale_x_log10() + 
  
  # to provide a title, x, and y axis labels
  xlab("No. of Movies (log 10 scale)") + 
  ylab("Number of ratings received") +
  ggtitle("Movies Distribution - Number of times a movie is rated")




# filter out obscure outliers movies which was rated once and with 
# extreme ratings (rating less than 2 or more than 4)     

# create a temporary dataset from training set
train_set_working <- train_set %>% 
  
  # filter outliers movie with extreme ratings 
  filter(rating < 2 | rating > 4) %>% 
  
  # calculate count of filtered uniqueu movies
  group_by(title) %>% 
  summarise(rating = mean(rating), count = n()) %>% 
  
  # filter out movies rated only once
  filter(count == 1) 

# to display first 10 rows
slice(train_set_working, 1:10)%>%
  
  # to apply theme to the table  
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed"))%>%
  row_spec(0, bold = T)




## Movie Popularity

# to plot review count and average ratings received by a specific movie
train_set %>% 
  
  # to plot graph for mean rating and count for individual movies
  group_by(movieId) %>% 
  summarise(count = n(), avg_rating = mean(rating)) %>% 
  
  # to map movie count and mean rating by movie variables to aesthetics of ggplot function
  ggplot(aes(x = count, y = avg_rating)) +
  
  # to plot scattered data using geom_point
  geom_point(alpha = 0.5, col = "#00AFBB") +
  
  # to draw a horizontal line incepting y-axis at the mean rating   
  geom_hline(yintercept = mu, linetype = "dashed", color="#1380A1", size=1) +
  
  #scale axis to custom range and intervels
  scale_x_continuous(labels = scales::comma)  +  
  scale_y_discrete(limits = c(seq(0.5,5,0.5)))  +  
  
  # to draw a smooth conditional mean line   
  geom_smooth(se = FALSE, colour = "#FC4E07", size = 1) +
  
  # to provide a title, x, and y axis labels
  xlab("No. of Reviews received") + 
  ylab("Average Rating") +  
  labs(title = "Movie Popularity") 




## Users Distribution

# to plot number of ratings given by a specific user
train_set %>%
  count(userId) %>%
  
  # to map users count variables to aesthetics of ggplot function
  ggplot(aes(n)) +
  
  # to plot graph as histogram with bin size of 30
  geom_histogram(fill = "#00AFBB", bins = 30, color = "#FC4E07") +
  
  # scale axis to log 10        
  scale_y_log10() +
  
  # to provide a title, x, and y axis labels
  xlab("Number of ratings given by users") + 
  ylab("Number of users (log 10 scale)") +
  ggtitle("User Rating Activity Distribution")





# to display a tabulated data of number of ratins given by most active, least active, and average users
# to create a temporary dataset
train_set_usr <- train_set %>%
  
  # to find out user if count 
  group_by(userId) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# to create a table to display details
usr <- matrix(c(max(train_set_usr$count),min(train_set_usr$count),round(mean(train_set_usr$count))),ncol=3,byrow=TRUE)
colnames(usr) <- c("Most active user rated","Least active user rated","Average user rated")
rownames(usr) <- c("Number of ratings")
usr%>%

  # to apply theme to the table  
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed"))%>%
  row_spec(0, bold = T)





# "Mean movie ratings given by users     
train_set %>%
  
  # to plot graph for bias for individual user who rated atleast 100 movies
  group_by(userId) %>%
  filter(n() >= 1) %>% ## TBD make it 100
  summarize(b_u = mean(rating)) %>%
  
  # to map user bias variable to aesthetics of ggplot function
  ggplot(aes(b_u)) +
  
  # to plot histogram
  geom_histogram(fill = "#00AFBB", bins = 30, color = "#FC4E07") +
  
  # to draw a vertical line incepting x-axis at the mean rating       
  geom_vline(xintercept = mu, linetype = "dashed", color="#1380A1", size=1) +
  
  # to provide a title, x, and y axis labels
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Mean movie ratings given by users") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) 




## Number of movies rated per year

# to plot number of movies rated per year            
train_set %>%
  
  select(year_rated, movieId) %>%
  group_by(year_rated) %>%
  summarise(count = n_distinct(movieId)) %>%
  
  # to map year rated and number of movies variables to aesthetics of ggplot function
  ggplot(aes(x = year_rated, y = count)) +
  
  # to plot scattered data using geom_point
  geom_point(alpha = 0.5, col = "#00AFBB", size = 2) +
  
  # scale axis to show non-scientific values (without exponential)
  scale_y_continuous(labels = scales::comma) +
  
  # to draw a smooth conditional mean line       
  geom_smooth(se = FALSE, colour = "#FC4E07", size = 1) +
  
  # to provide a title, x, and y axis labels
  xlab("Year Rated") +
  ylab("Number of Movies Rated") +
  ggtitle("Movies Rated per Year") 



## Number of active users per year

# to plot number of active users per year      
train_set %>%
  
  # to select columns
  select(year_rated, userId) %>%
  
  # to plot specific users per year rated
  group_by(year_rated) %>%
  summarise(count = n_distinct(userId)) %>%
  
  # to map year_rated and active users per year variables to aesthetics of ggplot function      
  ggplot(aes(x = year_rated, y = count)) +
  
  # to plot scattered data using geom_point
  geom_point(alpha = 0.5, col = "#00AFBB", size = 2) +
  
  # scale axis to show non-scientific values (without exponential)
  scale_y_continuous(labels = scales::comma) +
  
  # to draw a smooth conditional mean line       
  geom_smooth(se = FALSE, colour = "#FC4E07", size = 1) +
  
  # to provide a title, x, and y axis labels
  xlab("Year Rated") +
  ylab("Number of active users") +
  ggtitle("Active Users per Year") 





#######################################################################################
#### Modeling Approach  ####
# Comments: This section of code modeling approacha and insights gained from them.
#######################################################################################


## Model 1: Basic Rating Mean Model 

###############################      
# to find basic naive mean RMSE
###############################
rmse_mean <- RMSE(test_set$rating, mu)
rmse_mean


# to display RMSE results in a tabular form
rmse_results <- tibble(Model = "Rating Mean Naive Model", 
                       Dataset = "test_set", RMSE = round(rmse_mean, digits = 5))
rmse_results %>% 
  # to apply theme to the table
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
  
  # to group rows in various catagories
  pack_rows("Basic Prediction Model", 1, 1) %>%
  row_spec(0, bold = T) %>%
  row_spec(1:1, bold = T, color = "white", background = "#D7261E")



## Model 2: Movie Effect Model

# to plot movie bias density graph
mu <- mean(train_set$rating)
train_set %>% 
  
  # to plot movie bias  
  group_by(movieId) %>% 
  summarise(movie_bias = mean(rating - mu)) %>% 
  # to plot a geom_density plot
  ggplot() +
  geom_density(aes(x = movie_bias), fill = "#00AFBB") +
  # to provide a title, x, and y axis labels
  xlab("Movie Bias") +
  ylab("Density") +
  ggtitle("Movie Bias (b_i) Density") 


##################################################      
# model taking into account the movie effects, b_i
##################################################

# to find movie bias for all movies with unique movieId
movie_bias <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu))

# to calculate predicted ratings using mean + movie bias in test_set set for the movieId      
predicted_ratings <- mu + test_set %>%
  left_join(movie_bias, by='movieId') %>%
  pull(b_i)

# to calculate RMSE using the RMSE function by passing predicted and true ratings
rmse_movie <- RMSE(predicted_ratings, test_set$rating)
rmse_movie

# append results to the table      
rmse_results <- bind_rows(rmse_results, 
                          tibble(Model="Movie Effect Model", 
                                 Dataset = "test_set", RMSE = round(rmse_movie, digits = 5)))
rmse_results %>% 
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
  
  # to group rows in various catagories  
  pack_rows("Basic Prediction Model", 1, 1) %>%
  pack_rows("Single Predictor Model", 2, 2) %>%
  row_spec(0, bold = T) %>%
  # to highlight the last row
  row_spec(2:2, bold = T, color = "white", background = "#D7261E")




## Model 3: User Effect Model

# to plot user bias density graph
mu <- mean(train_set$rating)
train_set %>% 
  # to plot user bias    
  group_by(userId) %>% 
  summarise(user_bias = mean(rating - mu)) %>% 
  # to plot a geom_density plot
  ggplot() +
  geom_density(aes(x = user_bias), fill = "#00AFBB") +
  
  # to provide a title, x, and y axis labels
  xlab("User Bias") +
  ylab("Density") +
  ggtitle("User Bias (b_u) Density")


##################################################
# model taking into account the user effects, b_u
##################################################

# to find user bias for all users with unique userId
user_bias <- train_set %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu))

# to calculate predicted ratings using mean + user bias in test_set for the movieId      
predicted_ratings <- test_set %>%
  left_join(user_bias, by='userId') %>%
  mutate(pred = mu + b_u) %>%
  # pull /select predicted value from the joined dataaset
  pull(pred)

# to calculate RMSE using the RMSE function by passing predicted and true ratings
rmse_usr <- RMSE(predicted_ratings, test_set$rating)
rmse_usr

# append results to the table            
rmse_results <- bind_rows(rmse_results, 
                          tibble(Model="User Effect Model",
                                 Dataset = "test_set", RMSE = round(rmse_usr, digits = 5)))
rmse_results %>% 
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
  
  # to group rows in various catagories  
  pack_rows("Basic Prediction Model", 1, 1)  %>%
  pack_rows("Single Predictor Model", 2, 3) %>%
  row_spec(0, bold = T) %>%
  # to highlight the last row
  row_spec(3:3, bold = T, color = "white", background = "#D7261E")




## Model 4: Year Released Effect Model

#################################################################
# model taking into account the year released effects, b_p (bias)
#################################################################

# to find bias for all movies released in a particular year
YRel_avgs <- train_set %>%
  group_by(year_released) %>%
  summarise(b_p = mean(rating - mu))

# to calculate predicted ratings using mean and year_released bias in test_set set 
# year_released in not available in the test_set dataset, +
# so extrat it with the same logic used to extrat train_set column
predicted_ratings <- test_set %>%
  mutate(year_released=as.numeric(str_remove_all(str_extract(title, 
                                                             "\\(\\d{4}\\)$"), "[()]"))) %>%
  
  # perform a left join by year released mutate predicted value and 
  # pull it to be passsed into predicted ratings dataset
  left_join(YRel_avgs, by='year_released') %>%
  mutate(pred = mu + b_p) %>%
  pull(pred)

# to calculate RMSE using the RMSE function by passing predicted and true ratings
rmse_YRel <- RMSE(predicted_ratings, test_set$rating)
rmse_YRel

# append results to the table      
rmse_results <- bind_rows(rmse_results, 
                          tibble(Model="Year Released Effect Model",
                                 Dataset = "test_set", RMSE = round(rmse_YRel, digits = 5)))
rmse_results %>% 
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
  
  # to group rows in various catagories  
  pack_rows("Basic Prediction Model", 1, 1) %>%
  pack_rows("Single Predictor Model", 2, 4) %>%
  row_spec(0, bold = T)  %>%
  # to highlight the last row  
  row_spec(4:4, bold = T, color = "white", background = "#D7261E")





## Model 5: Year Rated Effect Model

##########################################################################
# model taking into account the year a movie was rated effects, b_r (bias)
##########################################################################

# to find bias for all movies released in a particular year
YRat_avgs <- train_set %>%
  group_by(year_rated) %>%
  summarise(b_r = mean(rating - mu))

# to calculate predicted ratings using mean in test_set for the year rated      
# year_released in not available in the test_set dataset, +
# so extrat it with the same logic used to extrat train_set column
predicted_ratings <- test_set %>%
  mutate(year_rated = year(as_datetime(timestamp))) %>%
  
  # perform a left join by year released mutate predicted value and 
  # pull it to be passsed into predicted ratings dataset
  left_join(YRat_avgs, by='year_rated') %>%
  mutate(pred = mu + b_r) %>%
  pull(pred)

# to calculate RMSE using the RMSE function by passing predicted and true ratings  
rmse_YRat <- RMSE(predicted_ratings, test_set$rating)
rmse_YRat

# append results to the table            
rmse_results <- bind_rows(rmse_results, 
                          tibble(Model="Year Rated Effect Model",
                                 Dataset = "test_set", RMSE = round(rmse_YRat, digits = 5)))
rmse_results %>% 
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
  
  # to group rows in various catagories    
  pack_rows("Basic Prediction Model", 1, 1) %>%
  pack_rows("Single Predictor Model", 2, 5) %>%
  row_spec(0, bold = T)  %>%
  # to highlight the last row  
  row_spec(5:5, bold = T, color = "white", background = "#D7261E")





## Model 6: Movie and User Effects Model (Multiple Predictors Model)

#########################################
## the model taking Movie and User bias
#########################################

# to find bias for all movies released in a particular year
user_bias <- train_set %>%
  left_join(movie_bias, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))

# to calculate predicted ratings using mean, user bias and movie bias in test_set 
predicted_ratings <- test_set %>%
  
  # first perform a left join by movieId then by userId, mutate predicted value
  # pull it to be passsed into predicted ratings dataset  
  left_join(movie_bias, by='movieId') %>%
  left_join(user_bias, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# to calculate RMSE using the RMSE function by passing predicted and true ratings
rmse_Mov_Usr <- RMSE(predicted_ratings, test_set$rating)
rmse_Mov_Usr



# append results to the table    
rmse_results <- bind_rows(rmse_results, 
                          tibble(Model="Movie & User Effect Model",
                                 Dataset = "test_set", RMSE = round(rmse_Mov_Usr, digits = 5)))
rmse_results %>% 
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
  
  # to group rows in various catagories  
  pack_rows("Basic Prediction Model", 1, 1) %>%
  pack_rows("Single Predictor Model", 2, 5)  %>%
  pack_rows("Multipe Predictors Model", 6, 6) %>%
  row_spec(0, bold = T)  %>%
  # to highlight the last row  
  row_spec(6:6, bold = T, color = "white", background = "#D7261E")





## Model 7: Regularized Movie and User bias Prediction Model

##########################################################
# Predict via regularisation, movie and user effect model
##########################################################

# lambdas vetror contains values from 0 to 10 with an interverl of 0.25
lambdas <- seq(0, 10, 0.25)

# sapply apply various values of lambda from vector lambdas to the + 
# function to calcuate all possible RMSE and stores in rmses
rmses <- sapply(lambdas, function(lambda){
  
  # calcuate mean
  mu <- mean(train_set$rating)
  
  # calculate movie bias using lambda
  movie_bias <- train_set %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(n()+lambda))
  
  # calculate user bias using lambda              
  user_bias <- train_set %>%
    left_join(movie_bias, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu)/(n()+lambda))
  
  # first left join test_set with movie bias by movieId and +
  # then join again with user bias using userId
  predicted_ratings <- test_set %>%
    left_join(movie_bias, by = "movieId") %>%
    left_join(user_bias, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  # returns the value of calculated RMSE on train_set set with the passed lambda argument
  return(RMSE(predicted_ratings, test_set$rating))
  
})



# plot a graph lambdas against generated rmses  
qplot(lambdas, rmses, geom=c("point", "line")) 


# to find the minimum value of lambda from lambdas for which RMSE is minimum
lambda <- lambdas[which.min(rmses)]
lambda

cat("The optimal lambda is ",  toString(lambda))




# to find the RMSE for this model using optimal lambda
rmse_reg = min(rmses)

# append results to the table      
rmse_results <- bind_rows(rmse_results, 
                          tibble(Model="Regularized Movie & User Effect Model",
                                 Dataset = "test_set   |   lambda : 5", 
                                 RMSE = round(rmse_reg, digits = 5)))
rmse_results %>% 
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
  
  # to group rows in various catagories    
  pack_rows("Basic Prediction Model", 1, 1) %>%
  pack_rows("Single Predictor Model", 2, 5)  %>%
  pack_rows("Multipe Predictors Model", 6, 6) %>%
  pack_rows("Regularized Model", 7, 7) %>%
  row_spec(0, bold = T)  %>%
  # to highlight the last row  
  row_spec(7:7, bold = T, color = "white", background = "#D7261E")





## Evaluating the selected model on validation dataset

############################################################################
# Predict via regularisation, movie and user effect model on validation set
############################################################################

# previously selected optimal lambda
lambda <- 5

# calcuate mean on edx set
mu_edx <- mean(edx$rating)

# calculate movie bias using lambda
movie_bias <- edx %>%
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu_edx)/(n()+lambda))

# calculate user bias using lambda              
user_bias <- validation %>%
  left_join(movie_bias, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - b_i - mu_edx)/(n()+lambda))

# first left join validations with movie bias by movieId and then 
# join again with user bias using userId

# edx_with_prediction is a temporary dataset to store predicted ratings along with other edx colums
edx_with_predictions <- validation %>%
  left_join(movie_bias, by = "movieId") %>%
  left_join(user_bias, by = "userId") %>%
  mutate(pred = mu_edx + b_i + b_u)

# to pull predicted ratings for RMSE calculation
predicted_ratings <- edx_with_predictions %>%
  pull(pred)

# calculate RMSE on validation set
model_reg_val <- RMSE(validation$rating, predicted_ratings)

# append results to the table      
rmse_results <- bind_rows(rmse_results, 
                          tibble(Model="Regularized Movie & User Effect Model",
                                 Dataset = "Validation Set   |   lambda : 5", 
                                 RMSE = round(model_reg_val, digits = 5)))
rmse_results %>% 
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
  
  # to group rows in various catagories    
  pack_rows("Basic Prediction Model", 1, 1) %>%
  pack_rows("Single Predictor Model", 2, 5)  %>%
  pack_rows("Multipe Predictors Model", 6, 6) %>%
  pack_rows("Regularized Model", 7, 8) %>%
  row_spec(0, bold = T) %>%
  # to highlight the last row  
  row_spec(8:8, bold = T, color = "white", background = "#D7261E")




#######################################################################################
#### Modeling Results  ####
# Comments:
#
#######################################################################################

# to display result tables with RMSE model results 
rmse_results %>% 
  kable("latex", booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
  pack_rows("Basic Prediction Model", 1, 1) %>%
  pack_rows("Single Predictor Model", 2, 5)  %>%
  pack_rows("Multipe Predictors Model", 6, 6) %>%
  pack_rows("Regularized Model", 7, 8) %>%
  row_spec(0, bold = T) %>%
  row_spec(8:8, bold = T, color = "white", background = "#3DDB48")



################# Final Results #######################

cat("Final RMSE: ", toString(round(model_reg_val, digits = 5)))

# Please Note: As per the course requirement the final RMSE is printed; however, detailed result table is displayed in the 'Viewer' pane.
# Also, as predicted raitings contains 10M columns it is not feasible to display on the screen, 
# Hence, it is are stored along with other columns in a new datase 'edx_with_prediction' which can be viewd from Environment pane.


## to clear-up memory
# Remove objects    
rm(train_set_dr, train_set_ma, train_set_usr, year_released, cls, usr, mu)  
# Call Garbage Collector
gc()

