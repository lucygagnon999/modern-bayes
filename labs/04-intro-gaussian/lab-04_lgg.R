# lab-04_lgg.R
## don't use data in package, use data on lab assignment #
library(plyr)
library(ggplot2)
library(dplyr)
library(xtable)
library(reshape)


set.seed(123)
# input data
# spurters
x = c(18, 40, 15, 17, 20, 44, 38)
# control group
y = c(-4, 0, -19, 24, 19, 10, 5, 10,
      29, 13, -9, -8, 20, -1, 12, 21,
      -7, 14, 13, 20, 11, 16, 15, 27,
      23, 36, -33, 34, 13, 11, -19, 21,
      6, 25, 30,22, -28, 15, 26, -1, -2,
      43, 23, 22, 25, 16, 10, 29)
# store data in data frame 
iqData = data.frame(Treatment = c(rep("Spurters", length(x)), 
                                  rep("Controls", length(y))),
                    Gain = c(x, y))
### Task 1
xLimits = seq(min(iqData$Gain) - (min(iqData$Gain) %% 5),
              max(iqData$Gain) + (max(iqData$Gain) %% 5),
              by = 5)

ggplot(data = iqData, aes(x = Gain, fill = Treatment, colour = I("black"))) + 
  geom_histogram(position = "dodge", alpha = 0.5, breaks = xLimits, closed = "left")+
  scale_x_continuous(breaks = xLimits, 
                     expand = c(0,0))+ 
  scale_y_continuous(expand = c(0,0), 
                     breaks = seq(0, 10, by = 1))+
  ggtitle("Histogram of Change in IQ Scores") + labs(x = "Change in IQ Score", 
                                                     fill = "Group") + 
  theme(plot.title = element_text(hjust = 0.5)) 
#### The change in IQ score for the control group spans -35 to 45, while the change 
#### in IQ score spans 17.5 - 45 for spurters. So, we see positive, 0, and negative
#### changes in IQ score in the control group, while we only observe increases
#### in IQ score for the spurters group.


