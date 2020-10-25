#####################################
##PROJECT I: DESCRIPTIVE ANALYTICS###
############JAVIER LOZANO############
##########DR. STEVE LEVKOFF##########
###############ECON 494##############

#Set working directory
setwd("~/USD/2020-2021/Fall 2020/ECON 494 - Intro to Business Analytics/Project I")

#Use this option to import data sets from the internet
#Apple:
AAPL<-read.csv('https://raw.githubusercontent.com/jlozano21/Project-I-Data-Sets/master/AAPL.csv')
#Microsoft:
MSFT<-read.csv('https://raw.githubusercontent.com/jlozano21/Project-I-Data-Sets/master/MSFT.csv')
#Facebook:
FB<-read.csv('https://raw.githubusercontent.com/jlozano21/Project-I-Data-Sets/master/FB.csv')
#S&P 500 Index:
SP500<-read.csv('https://raw.githubusercontent.com/jlozano21/Project-I-Data-Sets/master/%5EGSPC.csv')

#Use this option to import locally
#Apple:
AAPL<-read.csv('AAPL.csv')
#Microsoft:
MSFT<-read.csv('MSFT.csv')
#Facebook:
FB<-read.csv('FB.csv')
#S&P 500 Index
SP500<-read.csv('^GSPC.csv')

#View each data set to identify variables and their number of observations
#Apple:
View(AAPL)
#Microsoft:
View(MSFT)
#Facebook:
View(FB)
#S&P 500 Index:
View(SP500)

#Generate summary statistics for each data set
#Apple:
summary(AAPL)
#Microsoft:
summary(MSFT)
#Facebook:
summary(FB)
#S&P 500 Index:
summary(SP500)

#Look into summary of adjusted close for each data set to identify the trends for each 
#company and the index to see if and how COVID-19 impacted them
#Apple:
summary(AAPL$Adj.Close)
#Microsoft:
summary(MSFT$Adj.Close)
#Facebook:
summary(FB$Adj.Close)
#S&P 500 Index:
summary(SP500$Adj.Close)

#Look into summary of volume for each data set to identify the shares sold for each 
#company and the index to see if and how COVID-19 impacted them
#Apple:
summary(AAPL$Volume)
#Microsoft:
summary(MSFT$Volume)
#Facebook:
summary(FB$Volume)
#S&P 500 Index:
summary(SP500$Volume)

#Add histogram for adjusted close and volume variables and add a supplemental
#calibrated normal density curve with different colors for each data set to identify them clearly.
#The colors will be as following: Apple=red, Microsoft=green, Facebook=darkblue, S&P 500 Index=yellow

#Apple:
hist(AAPL$Adj.Close, prob = TRUE)
curve(dnorm(x, mean = mean(AAPL$Adj.Close), sd = sd(AAPL$Adj.Close)), col = "red", lwd = 2, add = TRUE)
hist(AAPL$Volume, prob = TRUE)
curve(dnorm(x, mean = mean(AAPL$Volume), sd = sd(AAPL$Volume)), col = "red", lwd = 2, add = TRUE)
#Microsoft:
hist(MSFT$Adj.Close, prob = TRUE)
curve(dnorm(x, mean = mean(MSFT$Adj.Close), sd = sd(MSFT$Adj.Close)), col = "green", lwd = 2, add = TRUE)
hist(MSFT$Volume, prob = TRUE)
curve(dnorm(x, mean = mean(MSFT$Volume), sd = sd(MSFT$Volume)), col = "green", lwd = 2, add = TRUE)
#Facebook:
hist(FB$Adj.Close, prob = TRUE)
curve(dnorm(x, mean = mean(FB$Adj.Close), sd = sd(FB$Adj.Close)), col = "darkblue", lwd = 2, add = TRUE)
hist(FB$Volume, prob = TRUE)
curve(dnorm(x, mean = mean(FB$Volume), sd = sd(FB$Volume)), col = "darkblue", lwd = 2, add = TRUE)
#S&P 500 Index:
hist(SP500$Adj.Close, prob = TRUE)
curve(dnorm(x, mean = mean(SP500$Adj.Close), sd = sd(SP500$Adj.Close)), col = "yellow", lwd = 2, add = TRUE)
hist(SP500$Volume, prob = TRUE)
curve(dnorm(x, mean = mean(SP500$Volume), sd = sd(SP500$Volume)), col = "yellow", lwd = 2, add = TRUE)

#INSTALL THE GGPLOT2 PACKAGE (MUST BE DONE ONCE)
install.packages('ggplot2')
library(ggplot2)

#After installing, create scatter plot for each data and rendering the data using the geom point with corresponding colors
#Apple:
ggplot(AAPL, aes(Adj.Close, Volume)) + geom_point(color = "red")
#Microsoft:
ggplot(MSFT, aes(Adj.Close, Volume)) + geom_point(color = "green")
#Facebook:
ggplot(FB, aes(Adj.Close, Volume)) + geom_point(color = "darkblue")
#S&P 500 Index:
ggplot(SP500, aes(Adj.Close, Volume)) + geom_point(color = "yellow")

#Add facet_wrap() function to plot the object
#Apple:
ggplot(AAPL, aes(Adj.Close, Volume)) + geom_point(color = "red") + facet_wrap(~COVID.19)
#Microsoft:
ggplot(MSFT, aes(Adj.Close, Volume)) + geom_point(color = "green") + facet_wrap(~COVID.19)
#Facebook:
ggplot(FB, aes(Adj.Close, Volume)) + geom_point(color = "darkblue") + facet_wrap(~COVID.19)
#S&P 500 Index:
ggplot(SP500, aes(Adj.Close, Volume)) + geom_point(color = "yellow") + facet_wrap(~COVID.19)

#Add geom_line() function to trace the plotted infromation
#Apple:
ggplot(AAPL, aes(Adj.Close, Volume)) + geom_point(color = "red") + facet_wrap(~COVID.19) + geom_line()
#Microsoft:
ggplot(MSFT, aes(Adj.Close, Volume)) + geom_point(color = "green") + facet_wrap(~COVID.19) + geom_line()
#Facebook:
ggplot(FB, aes(Adj.Close, Volume)) + geom_point(color = "darkblue") + facet_wrap(~COVID.19) + geom_line()
#S&P 500 Index:
ggplot(SP500, aes(Adj.Close, Volume)) + geom_point(color = "yellow") + facet_wrap(~COVID.19) + geom_line()

#Violin plot to present an alternate perspective
#Apple:
ggplot(AAPL, aes(Adj.Close, Volume)) + geom_violin(fill='red')
#Microsoft:
ggplot(MSFT, aes(Adj.Close, Volume)) + geom_violin(fill='green')
#Facebook:
ggplot(FB, aes(Adj.Close, Volume)) + geom_violin(fill='darkblue')
#S&P 500 Index:
ggplot(SP500, aes(Adj.Close, Volume)) + geom_violin(fill='yellow')

#Violin plot with the facet_wrap function
#Apple:
ggplot(AAPL, aes(Adj.Close, Volume)) + geom_violin() + facet_wrap(~COVID.19)
#Microsoft:
ggplot(MSFT, aes(Adj.Close, Volume)) + geom_violin() + facet_wrap(~COVID.19)
#Facebook:
ggplot(FB, aes(Adj.Close, Volume)) + geom_violin() + facet_wrap(~COVID.19)
#S&P 500 Index:
ggplot(SP500, aes(Adj.Close, Volume)) + geom_violin() + facet_wrap(~COVID.19)

#Box and whiskers plot with the facet_wrap function
#Apple:
ggplot(AAPL, aes(Adj.Close)) + geom_boxplot() + facet_wrap(~COVID.19)
#Microsoft:
ggplot(MSFT, aes(Adj.Close)) + geom_boxplot() + facet_wrap(~COVID.19)
#Facebook:
ggplot(FB, aes(Adj.Close)) + geom_boxplot() + facet_wrap(~COVID.19)
#S&P 500 Index:
ggplot(SP500, aes(Adj.Close)) + geom_boxplot() + facet_wrap(~COVID.19)

#The polygon histogram allows us to identify the overall adjusted close prices 
#Apple:
ggplot(AAPL, aes(Adj.Close)) + geom_freqpoly()
#Microsoft:
ggplot(MSFT, aes(Adj.Close)) + geom_freqpoly()
#Facebook:
ggplot(FB, aes(Adj.Close)) + geom_freqpoly()
#S&P 500 Index:
ggplot(SP500, aes(Adj.Close)) + geom_freqpoly()

#To illustrate the TIDY data set the final step would be to layer a density curve over a bar graph demonstrating the relationship
#between the Adjusted CLose and the percentage points that it changes when the value increases or decreases
#Apple:
ggplot(AAPL,aes(x=Adj.Close)) + geom_histogram(aes(y=..density..), position = "identity", binwidth=2) + geom_density() + geom_vline(xintercept = mean(AAPL$Adj.Close))
#Microsoft:
ggplot(MSFT,aes(x=Adj.Close)) + geom_histogram(aes(y=..density..), position = "identity", binwidth=3) + geom_density() + geom_vline(xintercept = mean(MSFT$Adj.Close))
#Facebook:
ggplot(FB,aes(x=Adj.Close)) + geom_histogram(aes(y=..density..), position = "identity", binwidth=3) + geom_density() + geom_vline(xintercept = mean(FB$Adj.Close))
#S&P 500 Index:
ggplot(SP500,aes(x=Adj.Close)) + geom_histogram(aes(y=..density..), position = "identity", binwidth=20) + geom_density() + geom_vline(xintercept = mean(SP500$Adj.Close))
