#####################################
##PROJECT I: DESCRIPTIVE ANALYTICS###
############JAVIER LOZANO############
##########DR. STEVE LEVKOFF##########
###############ECON 494##############

#Set working directory
setwd("~/USD/2020-2021/Fall 2020/ECON 494 - Intro to Business Analytics/Project I")

#Use this option to import from the internet
AAPL <- read.csv('https://raw.githubusercontent.com/jlozano21/ECON-494---PROJECT-I/master/AAPL.csv')
MSFT <- read.csv('https://raw.githubusercontent.com/jlozano21/ECON-494---PROJECT-I/master/MSFT.csv')
SNE <- read.csv('https://raw.githubusercontent.com/jlozano21/ECON-494---PROJECT-I/master/SNE.csv')
S&P500 Index <- read.csv('https://raw.githubusercontent.com/jlozano21/ECON-494---PROJECT-I/master/%5EGSPC%20(S%26P%20500%20Index).csv')

#View each data set
View(AAPL)
View(MSFT)
View(SNE)