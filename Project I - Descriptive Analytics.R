#####################################
##PROJECT I: DESCRIPTIVE ANALYTICS###
############JAVIER LOZANO############
##########DR. STEVE LEVKOFF##########
###############ECON 494##############

#Set working directory
setwd("~/USD/2020-2021/Fall 2020/ECON 494 - Intro to Business Analytics/Project I")

#Import Excel data for each company
AAPL <- read.csv("AAPL.csv")
MSFT <- read.csv("MSFT.csv")
SNE <- read.csv("SNE.csv")

#Use this option to import from the internet

#View each separate data set
View(AAPL)
View(MSFT)
View(SNE)