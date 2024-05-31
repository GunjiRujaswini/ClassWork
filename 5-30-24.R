library(ggplot2)
all <- read.csv(file.choose()) #open ThreeClusters.csv
head(all)
qplot(data=all, x=V1, y=V2) # kind of Obvious!!!
#convert to a matrix as clustering works better on a matrix
mat <- cbind( all$V1, all$V2)
head(mat)
kmeans(mat,centers=3) # All that output!!!
# lets draw it
km = kmeans(mat,centers=3)$cluster # vector of cluster belonging
all$cl <- factor( km)
head(all)
summary(all)
qplot(data=all, x=V1,y=V2, color=cl)


all <- read.csv(file.choose()) #open Demographics.csv
head(all)
qplot(data=all, y=Internet.users) # All over the place!!
kmeans(all$Internet.users,centers=3) # divide into 3 groups based on Internet Usage
# lets draw it
km = kmeans(all$Internet.users,centers=3)$cluster # vector of cluster belonging
all$cl <- factor( km)
qplot(data=all, y=Internet.users, color=Income.Group)
qplot(data=all, y=Internet.users, color=cl)
qplot(data=all, y=Birth.rate) # All over the place!!
kmeans(all$Birth.rate,centers=3) # divide into 3 groups based on Internet Usage
# lets draw it
km = kmeans(all$Birth.rate,centers=3)$cluster # vector of cluster belonging
all$cl <- factor( km)
qplot(data=all, y=Birth.rate, color=Income.Group)
qplot(data=all, y=Birth.rate, color=cl)

all <- read.csv(file.choose()) #open grades_km_input.csv
head(all)
qplot(data=all, y=English)
qplot(data=all, y=Math)
qplot(data=all, y=Science)
mat <- cbind(all$English, all$Math, all$Science)
head(mat)
kmeans(mat,centers=3) # divide into 3 groups
# lets draw it
km = kmeans(mat,centers=3)$cluster # vector of cluster belonging
all$cl <- factor( km)
# A bit hard to draw 4 columns
qplot(data=all, y=English, color=cl)
qplot(data=all, y=Math, color=cl)
qplot(data=all, y=Science, color=cl)
qplot(data=all, x=Science,y=English, color=cl)
qplot(data=all, x=Science,y=Math, color=cl)
qplot(data=all, x=Math, y=Science, color=cl)

all <- read.csv(file.choose()) #open grades_km_input.csv
head(all)
install.packages("plot3D") # This is a cool library to draw 3D plots!
library("plot3D")
scatter3D(all$English,all$Math,all$Science)
mat <- cbind(all$English, all$Math, all$Science, col=NULL)
head(mat)
kmeans(mat,centers=3) # divide into 3 groups
# lets draw it
km = kmeans(mat,centers=3)$cluster # vector of cluster belonging
all$cl <- km
scatter3D(all$English,all$Math,all$Science, colvar=all$cl)