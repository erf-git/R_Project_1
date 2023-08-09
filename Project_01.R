# Ethan R Feldman
# Project 1
# STAT 511

??Duncan

# The Duncan dataset was not working for me, 
# so I had to manually download it
#install.packages("car")
#install.packages("carData")
#install.packages("Duncan")
#data("Duncan")

# I had to download the dataset manually and read it
setwd("../Documents/R Files")
Duncan = read.csv("datasets/dataset-duncan.csv", header = TRUE)
head(Duncan)


#1. 
pdf(file = "figures/project2.pdf")
plot(1:100, 1:100, type='n', main='Duncan Data', xlab='Education', ylab='Income')

condition1 = Duncan$prestige <= 80
points(Duncan[condition1,"education"], Duncan[condition1,"income"], pch=16)

condition2 = Duncan$prestige > 80
points(Duncan[condition2,"education"], Duncan[condition2,"income"], pch=1, col='blue')


#2.
legend("topleft", 
       legend = c("Prestige <= 80", "Prestige > 80"),
       pch = c(16, 1), 
       col = c("black", "blue")
       )

# I put it in a figures folder
dev.off()


#3. 
Duncan$TypeCode = 1
Duncan[Duncan$type == 'wc', 'TypeCode'] = 2
Duncan[Duncan$type == 'bc', 'TypeCode'] = 3
Duncan


#4. 
write.table(x = Duncan, file = "datasets/newduncanoutput.txt")


#5. 
condition3 = (Duncan$income < 20) & (Duncan$education < 20) & (Duncan$prestige < 20)
Duncan[condition3,]
#which(condition3)


#6.
write.csv(x = Duncan[condition3,], file = "datasets/lessthan20.csv")
