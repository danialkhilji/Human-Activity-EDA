library(tidyverse)
library(dplyr)

#Basic information before data analysis
#Readings were taken from gyroscope and accelerometer
#They are taken while Walking (1), Walking upstairs (2), walking downstairs (3), standing (4), 
#sitting (4) and lying (5). Numbers in brackets shows the respective encoding in the dataset.
#Gyroscope readings were measured in three axis (x, y, and z components)
#Fourier transforms are used on the timing readings to obtain frequency readings
#For all base readings, their, mean, max, mad, sma, arcoefficient, etc are calculated
#Link to original dataset: https://www.kaggle.com/uciml/human-activity-recognition-with-smartphones

human_data <- read_csv('...Human Activity Recognition Using Smartphones/train.csv') #add data path here
dim(human_data)
head(human_data)
#There are total 563 features and total readings are 7352

#-----------------------------------funModelling---------------------------------------
library(funModeling)

j_min <- 1
j_max <- 100
for (i in 1:6)
{
  j_min
  j_max
  df_status(human_data[,j_min:j_max])
  j_min <- j_max
  j_max <- j_max + 100
  if (j_max == 500)
  {
    df_status(human_data[,j_max:j_max+63])
  }
}
#To analyze missing values, data types and unique values

human_data_mean <- human_data[, grepl('mean', names(human_data))]
colnames(human_data_mean)
head(human_data_mean)
#There are huge number of features which can be reduced before further analysis
#In order to keep analysis, only columns including mean values will be used

activity_list <- human_data[, grepl('Activity', names(human_data))]
#Adding activity column at the end of the new data frame

unique(activity_list)
#To see unique names in column to encode them in next step


human_data_mean <- data.frame(human_data_mean, activity = c(activity_list))
dim(human_data_mean)
human_data_mean[10:20, 45:47]
head(human_data_mean)
#Rename last column name

for(i in 1:nrow(human_data_mean)){
  if(human_data_mean$Activity[i]=='STANDING') {human_data_mean$Activity[i] <- 1}
  else if (human_data_mean$Activity[i]=='SITTING') {human_data_mean$Activity[i] <- 2}
  else if (human_data_mean$Activity[i]=='LAYING') {human_data_mean$Activity[i] <- 3}
  else if (human_data_mean$Activity[i]=='WALKING') {human_data_mean$Activity[i] <- 4}
  else if (human_data_mean$Activity[i]=='WALKING_DOWNSTAIRS') {human_data_mean$Activity[i] <- 5}
  
  else {human_data_mean$Activity[i] <- 6}}
#To encode activity in numerical column
#1=STANDING          
#2=SITTING           
#3=LAYING            
#4=WALKING           
#5=WALKING_DOWNSTAIRS
#6=WALKING_UPSTAIRS 

p_min <- 1
p_max <- 9
for (i in 1:5)
{ p_min
  p_max
  plot_num(human_data_mean[, p_min:p_max])
  p_min <- p_max
  p_max <- p_max + 9}
#plot_num() can give distribution of all variables but it is impossible to visualize 
#46 variables distribution together in one plot
#Many histograms show high value at 0 but it is not removed since it might have 
#a meaning that body is at fixed position/not moving at all

profiling_num(human_data_mean %>% select_if(is.numeric))
#To calculate statistical results (numeric condition is used to ignore activity column)
#This looks within optimum range since only mean columns are used

crl <- correlation_table(human_data_mean, 'Activity')
#To calculate correlations between only numerical against target variable
#It only uses numerical variables

all_corr <- var_rank_info(human_data_mean, 'Activity')
#To calculate correlation using all variables (numerical and categorical)
#Takes considerably long time as there are more than 7000 data points

plotar(data = human_data_mean, target = 'Activity', plot_type = 'histdens')
#Density histogram of mean body linear acceleration in euclidean norm
#From above graph it is noticed that moving body has density peaks >2.3 where as 
#static body has density peaks <1.5

plotar(data = human_data_mean, input = c('tBodyAcc.mean...X', 'tBodyAcc.mean...Y', 'tBodyAcc.mean...Z'),  
       target = 'Activity', plot_type = 'boxplot')
#Magnitude of acceleration with activity
#The movement in Y-axis is seen to be least because whenever a person moves left or right,
#they mostly rotate their body as well and since X-axis is the axis always facing the face
#so moving forward and backward always make changes in X-axis and Y-axis movement is least

#Similarly movement in Z-axis is the movement along sea level. So it shows the most change
#while moving down stairs or going up stairs.

plotar(data = human_data_mean, input = 'tBodyAccMag.mean..',  target = 'Activity', plot_type = 'boxplot')
#To visualize difference between no movement or movement
#Basic observations from above box plot:
#If tBodyAccMag.mean is <-0.8 then it means body is either standing, sitting or lying
#If tBodyAccMag.mean is >-0.3 then body is either walking, walking down stairs or walking up stairs
#If tBodyAccMag.mean is >0.0 then body is walking on plane


#-----------------------------------DataExplorer---------------------------------------
library(DataExplorer)
#In most of the functions used below, both original data frame and transformed
#data frame are used to test the performance of each function with respect of number of features

t(introduce(human_data))
t(introduce(human_data_mean))
#To analyze basic information from the dataset
#t() is to change columns in rows

t(introduce(human_data)) - t(introduce(human_data_mean))
#In order to see the difference between number of data points after transformation of
#data frame, the difference of above two lines can be used. Rows with integer values
#means that there was a change and the specific value shows the total change

plot_str(human_data)
plot_str(human_data_mean)
#To visualize the structure of the dataset (useful for complicated datasets)

plot_str(human_data, type = 'r')
plot_str(human_data_mean, type = 'r')
#To plot in a radial manner (easier to understand)

plot_intro(human_data, title = 'Basic Information of original data frame', ggtheme = theme_linedraw())
plot_intro(human_data_mean, title = 'Basic Information of transformed data frame', ggtheme = theme_linedraw())
#To visualize the above information

plot_histogram(human_data_mean)
#Histograms to visualization of continuous features
#histograms are also used to visualize the distribution of variables
#Since there are huge number of features in original data frame, plot_histogram(human_data)
#took roughly 30 minutes
#plot_histogram(human_data)

plot_density(human_data_mean)
#Another way to visualize distribution of continuous variables is by plotting density
#It shows a more general trend of variables in each graph as compared to plot_histogram
#which shows discrete representation

plot_qq(human_data_mean)
#To visualize the deviation from probability distribution
#This is to ensure that each mean calculated in the data frame is valid and has high confidence
#ideally points must be on the straight line

#General trend of the above plot is that all the mean frequency components are very
#close to the probability distribution line. Rest of the graphs have points close to the 
#line only for points near 0

plot_prcomp(na.omit(human_data_mean), nrow = 1L, ncol = 2L)
#PCA