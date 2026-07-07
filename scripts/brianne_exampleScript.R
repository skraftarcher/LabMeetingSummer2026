# This script is an example to learn how R and scripts work

# Brianne Du Clos -- created 30 June 2026

# load packages----
library(tidyverse)

# lines of code have functions and arguments
# c = a list function, list items (the arguments!) go in parens
# can try to read lines of code like a sentence for understanding (and debugging!)

# example objects----
x<-1 # value type: variable
x2<-c(1,2,3,4,5) # value type: numerical list
x3<-c("1","2","3","4","5") # value type: character list -- no NAs in data; 
# NAs will make the whole object character type!

x4<-data.frame(var1=seq(1,10,1),var2=("a"),var3=c(1.2,3.4)) # data type: data frame
# there are other ways to store data, like a list
# don't typically create data frames in R to use data; generally
# are downloading them from the internet or loading them in
# from your machine