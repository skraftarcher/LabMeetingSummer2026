#this script is an example to learn how R and scripts work

#Jasmyn Brignac - 6/30/2026

#load packages----
library(package = "tidyverse") #this line loads the package tidyverse

#example objects----

x<-1
x2<-c(1,2,3,4) #shows up as numbers
x3<-c("1","2","3","4") #shows up as characters, cannot do math like this

x4<-data.frame(var1=seq(1,10,1),var2=("a"),var3=c(1.2,3.4))
#c is the function for lists of numbers, "x, x2, x3, and x4 are all objects