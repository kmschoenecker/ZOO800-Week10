# Elizabeth Version of HW 10 #



######################################
#    Getting our data 
######################################

#I am going to bum Nathan's beautiful code htat takes it directly from the EDIS archive, in the meantime I just downloaded it manually 

ticks = read.csv("tick_rawdata.csv", header = TRUE)
d <- read.csv("name.csv", header = TRUE, row.names = 1)
#this says your header is a header for first row and first column is also a header 
