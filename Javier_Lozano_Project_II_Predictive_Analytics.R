#####################################
##PROJECT II: PREDICTIVE ANALYTICS###
############JAVIER LOZANO############
##########DR. STEVE LEVKOFF##########
###############ECON 494##############

#Import data sets from the internet
#Apple:
AAPL<-read.csv('https://raw.githubusercontent.com/jlozano21/Project-I-Data-Sets/master/AAPL.csv')
#Microsoft:
MSFT<-read.csv('https://raw.githubusercontent.com/jlozano21/Project-I-Data-Sets/master/MSFT.csv')
#Facebook:
FB<-read.csv('https://raw.githubusercontent.com/jlozano21/Project-I-Data-Sets/master/FB.csv')
#S&P 500 Index:
SP500<-read.csv('https://raw.githubusercontent.com/jlozano21/Project-I-Data-Sets/master/%5EGSPC.csv')

#View each data set to identify variables and their number of observations
#Apple:
View(AAPL)
#Microsoft:
View(MSFT)
#Facebook:
View(FB)
#S&P 500 Index:
View(SP500)

#LOAD THE GGPLOT2, PLYR, and TSERIES LIBRARIES
library(ggplot2) #for ggplot system and preloaded datasets
library(plyr) #for ddply()
library(tseries) #for the J-B test

#To illustrate the TIDY data set, layer a density curve over a bar graph demonstrating the relationship
#between the Adjusted Close and the percentage points that it changes when the value increases or decreases
#Apple:
ggplot(AAPL,aes(x=Adj.Close)) + geom_histogram(aes(y=..density..), position = "identity", binwidth=2) + geom_density() + geom_vline(xintercept = mean(AAPL$Adj.Close))
#Microsoft:
ggplot(MSFT,aes(x=Adj.Close)) + geom_histogram(aes(y=..density..), position = "identity", binwidth=3) + geom_density() + geom_vline(xintercept = mean(MSFT$Adj.Close))
#Facebook:
ggplot(FB,aes(x=Adj.Close)) + geom_histogram(aes(y=..density..), position = "identity", binwidth=3) + geom_density() + geom_vline(xintercept = mean(FB$Adj.Close))
#S&P 500 Index:
ggplot(SP500,aes(x=Adj.Close)) + geom_histogram(aes(y=..density..), position = "identity", binwidth=20) + geom_density() + geom_vline(xintercept = mean(SP500$Adj.Close))

#LET'S BUILD THE FIRST MODEL:  Volume = B0 + B1*Adj.Close+u
MODEL1<-lm(Adj.Close ~ Volume, AAPL)
summary(MODEL1)

#QUESTION:  ARE THE RESIDUALS NORMAL?
jarque.bera.test(MODEL1$residuals) #TEST FOR NORMLAITY!

ggplot(AAPL, aes(Volume, Adj.Close)) + 
  geom_point() +
  geom_smooth(method ='lm')

#QUESTION: HOW DOES COVID-19 AFFECT VOLUME?
AAPL$COVID.19<- as.factor(AAPL$COVID.19) #CONVERTS TO FACTOR VARIABLE

##VISUALIZING THE RELATIONSHIP BY COVID PRESENCE
ggplot(AAPL, aes(x = Volume, y = Adj.Close, color = COVID.19)) + 
  geom_point() +
  geom_smooth(method ='lm')

##A LAYERED EXPLORATORY PLOT
mean_mpg <- ddply(AAPL, "COVID.19", summarise, meanmpg=mean(Adj.Close)) #SUMMARIZES MEANS OF Adj.Close BY COVID.19 presence
ggplot(AAPL, aes(x = Adj.Close, fill = COVID.19)) + 
  geom_histogram(aes(y=..density..), position = "identity") + 
  geom_density(alpha = 1/2) +
  geom_vline(data = mean_mpg, aes(xintercept=meanmpg,  col=factor(COVID.19)), linetype="dashed", size=1, show.legend = FALSE)

#LET'S BUILD A DUMMY VARIABLE MODEL:
MODEL2 <- lm(Adj.Close ~ COVID.19, AAPL)
summary(MODEL2)

mean_displ <- ddply(AAPL, "COVID.19", summarise, meandispl=mean(Adj.Close)) #SUMMARIZES MEANS OF Adj.Close BY COVID-19 Presence
ggplot(AAPL, aes(x = Adj.Close, fill=COVID.19)) + 
  geom_histogram(aes(y=..density..), position = "identity") +
  facet_wrap(~COVID.19)+
  geom_vline(data = mean_displ, aes(xintercept=meandispl,  col=factor(COVID.19)), linetype="dashed", size=1, show.legend = FALSE) +
  geom_density(alpha = 1/2)

#LET'S BUILD A MODEL WITH MULTIPLE INDEPENDENT VARIABLES:
MODEL3 <- lm(Adj.Close ~ Volume + COVID.19, AAPL)
summary(MODEL3)

#INCORPORATING NONLINEAR TRANSFORMATIONS (A QUADRATIC EXAMPLE)

AAPL$Volume2<-AAPL$Volume^2 #CREATE A NEW VARIABLE THAT IS THE SQUARE OF THE OLD VARIABLE

#BUILD MODEL:  hwy = B0 + B1*displ + B2*displ^2 + u
MODEL4 <- lm(Adj.Close ~ Volume + Volume2, AAPL)
summary(MODEL4)

#View each data set to identify variables and their number of observations
#Apple:
View(AAPL)

#############################
####PARTITIONING THE DATA####
#############################

#fraction of sample to be used for training
p<-.7 #use 70% of the data to train/build the model

#count number of observations (rows) in the dataframe
obs_count<-dim(AAPL)[1]
obs_count

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size
#set the seed to make your partition reproducible
set.seed(1234)
#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)
View(train_ind)

Training <- AAPL[train_ind, ] #pulls random rows for training
Testing <- AAPL[-train_ind, ] #pulls random rows for testing

#CHECKING THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)

#BUILDING THE MODEL FROM THE TRAINING DATA
M1 <- lm(Adj.Close ~ Volume, Training)
summary(M1) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_1_IN <- predict(M1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(M1$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(M1, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$Adj.Close)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$Adj.Close)^2)/length(PRED_1_OUT)) #computes out-of-sample 

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING M1
x_grid <- seq(0,10e+09,1e+09) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M1, list(Volume=x_grid))
plot(Training$Adj.Close ~ Training$Volume, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Adj.Close ~ Testing$Volume, col='red', pch=3)

#BUILDING THE QUADRATIC MODEL FROM THE TRAINING DATA
M2 <- lm(Adj.Close ~ COVID.19, Training)
summary(M2) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_2_IN <- predict(M2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)
View(M2$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_2_OUT <- predict(M2, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$Adj.Close)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$Adj.Close)^2)/length(PRED_2_OUT)) #computes out-of-sample 

RMSE_2_IN #IN-SAMPLE ERROR
RMSE_2_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING M2

x_grid <- seq(0,10e+09,1e+09) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M2, list(COVID.19=x_grid))
plot(Training$Adj.Close ~ Training$COVID.19, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Adj.Close ~ Testing$COVID.19, col='red', pch=3)

#BUILDING THE CUBIC MODEL FROM THE TRAINING DATA
M3 <- lm(Adj.Close ~ Volume + COVID.19, Training)
summary(M3) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_3_IN <- predict(M3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)
View(M3$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_3_OUT <- predict(M3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$Adj.Close)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$Adj.Close)^2)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_3_IN #IN-SAMPLE ERROR
RMSE_3_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING M3

x_grid <- seq(0,10e+09,1e+09) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M3, list(Volume=x_grid, COVID.19=x_grid^2))
plot(Training$Adj.Close ~ Training$Volume, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Adj.Close ~ Testing$Volume, col='red', pch=3)

#BUILDING THE LOGARITHMIC MODEL FROM THE TRAINING DATA
M4 <- lm(Adj.Close ~ Volume + Volume2, Training)
summary(M4) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_4_IN <- predict(M4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)
View(M4$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_4_OUT <- predict(M4, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$Adj.Close)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$Adj.Close)^2)/length(PRED_4_OUT)) #computes out-of-sample 

RMSE_4_IN #IN-SAMPLE ERROR
RMSE_4_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING M4

x_grid <- seq(0,10e+09,1e+09) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M4, list(Volume=x_grid, Volume2=x_grid^2))
plot(Training$Adj.Close ~ Training$Volume, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Adj.Close ~ Testing$Volume, col='red', pch=3)

######################################
###########MODEL COMPARISON###########
######################################

#COMPARISON OF IN-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_IN #MODEL WITH ONLY LINEAR TERM (VOLUME)
RMSE_2_IN #MODEL WITH ONLY LINEAR TERM (COVID.19)
RMSE_3_IN #MODEL WITH TWO LINEAR TERMS (VOLUME & COVID.19)
RMSE_4_IN #MODEL WITH LINEAR AND QUADRATIC TERM (VOLUME & VOLUME^2)

#COMPARISON OF OUT-OF-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_OUT #MODEL WITH ONLY LINEAR TERM (VOLUME)
RMSE_2_OUT #MODEL WITH ONLY LINEAR TERM (COVID.19)
RMSE_3_OUT #MODEL WITH TWO LINEAR TERMS (VOLUME & COVID.19)
RMSE_4_OUT #MODEL WITH LINEAR AND QUADRATIC TERM (VOLUME & VOLUME^2)
