#In this script I'm trying to write functions to automate the plotting 
#process.

#cumsum takes in a vector x of numeric values and returns a vector y
#which stores the cumulative sum; y[i] is the cumulative value of x[1],x[2]
#,...,x[i]
cumsum <- function(x)
{
  y <- vector(length = length(x))
  for (i in 1:length(y))
  {
    y[i] <- sum(x[1:i])
  }
  return(y)
}


#read in the data
va_data <- readRDS("va_data")
va_data_male <- subset(va_data,level=="Male")
va_data_female <- subset(va_data,level=="Female")

#make a pie chart
count_male <- sum(va_data_male$count)
count_female <- sum(va_data_female$count)
pie(c(count_male,count_female),labels = c("Male","Female"), main = "Total Cases by Gender starting from 06/15/2020", col = c("red","blue"))

#plotGender_by_month takes in the dataframe va_data, a month, and a district and returns plots. 
#For now it will return a simple pie-chart for male and female cases over a month for a district
plotGender_by_month <- function(X,month,district)
{
  date <- X$date
  split_date <- str_split(date,"-")
  dimX <- dim(X)
  n <- dimX[1]
  Y <- data.frame()
  for (i in 1:n)
  {
    if (split_date[[i]][2] == month)
    {
      Y <- rbind(Y,X[i,])
    }
  }
  Y_male <- subset(Y,level == "Male")
  Y_male <- subset(Y_male,health_district == district)
  Y_female <- subset(Y,level == "Female")
  Y_female <- subset(Y_female,health_district == district)
  count_male <- sum(Y_male$count)
  count_female <- sum(Y_female$count)
  pie(c(count_male,count_female),labels = c("Male","Female"), main = "Total Cases", col = c("red","blue"))
}

plotGender_by_month(va_data,"07",district = "Alexandria")

#this function takes in a health district, gender, and va_data dataframe
#and returns a line plot of non cumulative cases
plot_byGender_healthdistrict <- function(X,district,gender)
{
  X_district <- subset(X,health_district == district)
  X_district <- subset(X_district,level == gender)
  plot(x = seq(1,dim(X_district)[1]),y = X_district$count, xlab = "Data Point", ylab = "Cases", main = paste("Cases For",district,"For",gender,sep=" "))
  lines(x = seq(1,dim(X_district)[1]),y = X_district$count,type = "o")
  plot(x = seq(1,dim(X_district)[1]),y = cumsum(X_district$count), xlab = "Data Point", ylab = "Cumulative Cases", main = paste("Cases For",district,"For",gender,sep=" "))
  lines(x = seq(1,dim(X_district)[1]),y = cumsum(X_district$count),type = "o")
}

plot_byGender_healthdistrict(va_data,"Alexandria",gender = "Female")

