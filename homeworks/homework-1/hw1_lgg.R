# HW1
# import packages
library(readr)
## Part A
rain_df <- read.table('homeworks/homework-1/data/rnf6080.dat', header=FALSE)
#### I used the base R read.table command

## Part B
dim(rain_df)
#### The rainfall df has 5070 rows and 27 columns

## Part C
colnames(rain_df)
#### I used the 'colnames()' command to obtain the column names of rain_df.
### The column names are "V1", "V2",..."V27"

## Part D
value<-rain_df[2,4]
print(value)
#### I used df indexing command to extract the value at row 2, column 4.
#### The value is 0.

## Part E
second_row_values<-rain_df[2,]
print(second_row_values)
#### To display the whole second row, I use dthe indexing command, with nothing
#### in the column index  to extract the entire row.
#### The second row includes V1 = 60, V2 = 4, V3 = 2, and the 
#### rest of the columns are zeroes.

## Part F
names(rain_df)<-c("year", "month", "day", seq(0,23))
print(names(rain_df))
#### The above command adds column names to rain_df

## Part G
rain_df$daily_rain_fall<-rowSums(rain_df[, "0":"23"])
print(rain_df$daily_rain_fall)

##Part H
hist(rain_df$daily_rain_fall)
#pdf("homeworks/homework-1/daily_rain_fall.pdf")
#dev.off()

## Part I
#### The histogram cannot possibly be right because one column includes
#### negative daily rain fall, which is non-physical

## Part J

library(dplyr)

filtered_rain_df <- rain_df %>% filter(daily_rain_fall >= 0)

## Part K
hist(filtered_rain_df$daily_rain_fall)
#### This histogram is more reasonable because all daily rainfall values are
#### physically plausible values

# Question 2

## Part A
x<-c("5", "12", "7")
#### For the vector assignment, this may be an error because you are supplying 
##### numbers as strings, when you would most likely prefer numeric datatypes
max(x)
#### This is an error because teh maximum value in x should be 12, but because 
#### the numebrs are supplied as strings, it picks the last input as the maximum 
#### of x, "7".
sort(x)
#### This is an error. The command sorts the strings by the first digit which
#### appears

sum(x)
#### This is an error. You cannot sum strings.

## Part B

y<-c("5", 7, 12)
#### This y includes mixed data types (a string and numeric daattypes).
#### This will likely cause errors.

y[2] +y[3]
#### The inclusion of the string in the vector reuslts in an error when trying
#### to sum the numeric entries

## Part C

z<-data.frame(z1 = "5", z2 = 7, z3 = 12)
z[1,2]+z[1,3]
#### This works because dataframes can handle mixed data types. So, you are able
#### to sum the two numeric entries.

# Question 3
## Part A
#### The point of reprodcuible code is so others (and your future self) can
#### understand what your code does and re-create it themselves (or yourself).

## Part B

#### Making your code rpeorducible is valaunel in this class and moving forward
#### because you could reference your code at a later date, such as when 
#### rveiewing for exams or working on a research project, and you can quickly
#### and easily understand what you did in the past.

## Part C
#### This assignment was a 3 in difficulty. I had to look some commands up, but
#### they were quick to incorporate once I knew them.
