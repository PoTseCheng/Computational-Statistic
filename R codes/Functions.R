#setting up the environment
install.packages(c("partykit", "RWeka", "rpart", "truncnorm", "remotes"))

remotes::install_github("jhilaire/guidr")

#general tools in need
library(truncnorm)
library(dplyr)
library(partykit)

#This is for CART algorithm
library(rpart)

#This is for C 4.5
library(rweka)

#This is for GUILD
library(remotes)









