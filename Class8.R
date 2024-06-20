# ---
#  title: "Anomaly"


library("ggplot2")
products <- read.csv(file.choose()) # Load your CSV file

p <- ggplot(data=products)

a <- geom_point(aes(x=seq, y=Day.1, color="Day 1"), alpha=0.3)
b <- geom_point(aes(x=seq, y=Day.2, color="Day 2"), alpha=0.3)
c <- geom_point(aes(x=seq, y=Day.3, color="Day 3"), alpha=0.3)
d <- geom_point(aes(x=seq, y=Day.4, color="Day 4"), alpha=0.3)
e <- geom_point(aes(x=seq, y=Day.5, color="Day 5"), alpha=0.3)
f <- geom_point(aes(x=seq, y=Day.6, color="Day 6"), alpha=0.3)
g <- geom_point(aes(x=seq, y=Day.7, color="Day 7"), alpha=0.3)
h <- geom_point(aes(x=seq, y=Day.8, color="Day 8"), alpha=0.3)
i <- geom_point(aes(x=seq, y=Day.9, color="Day 9"), alpha=0.3)
j <- geom_point(aes(x=seq, y=Day.10, color="Day 10"), alpha=0.3)

p + a + b + c + d + e + f + g + h + i + j + ylab("Traffic")

aa <- geom_smooth(aes(x=seq, y=Day.1, color="Day 1"))
bb <- geom_smooth(aes(x=seq, y=Day.2, color="Day 2"))
cc <- geom_smooth(aes(x=seq, y=Day.3, color="Day 3"))
dd <- geom_smooth(aes(x=seq, y=Day.4, color="Day 4"))
ee <- geom_smooth(aes(x=seq, y=Day.5, color="Day 5"))
ff <- geom_smooth(aes(x=seq, y=Day.6, color="Day 6"))
gg <- geom_smooth(aes(x=seq, y=Day.7, color="Day 7"))
hh <- geom_smooth(aes(x=seq, y=Day.8, color="Day 8"))
ii <- geom_smooth(aes(x=seq, y=Day.9, color="Day 9"))
jj <- geom_smooth(aes(x=seq, y=Day.10, color="Day 10"))

p + aa + bb + cc + dd + ee + ff + gg + hh + ii + jj + ylab("Traffic")

p + aa + a + ylab("Traffic Day 1")
p + bb + b + ylab("Traffic Day 2")
p + cc + c + ylab("Traffic Day 3")
p + dd + d + ylab("Traffic Day 4")
p + ee + e + ylab("Traffic Day 5")
p + ff + f + ylab("Traffic Day 6")
p + gg + g + ylab("Traffic Day 7")
p + hh + h + ylab("Traffic Day 8")
p + ii + i + ylab("Traffic Day 9")
p + jj + j + ylab("Traffic Day 10")


#####

#---
#  title: "Worldcup"
#author: "Nikshep A Kulli"
#date: "2022-11-05"
#output: 
#  html_document:
#  toc: True
#code_folding: hide
#---
#  # Worldcup data
#  ```{r}
library("ggplot2") 
products  <- read.csv(file.choose()) #nettraffic

p <- ggplot(data=products)

# plot all the datapoints, mess!

a <-  geom_point(aes(x=seq, y = Day.1, color="Day 1" ),alpha=0.3)
b <- geom_point(aes(x=seq, y = Day.2, color="Day 2"),alpha=0.3)
c <-  geom_point(aes(x=seq, y = Day.3, color="Day 3"),alpha=0.3)
d <- geom_point(aes(x=seq, y = Day.4, color="Day 4"),alpha=0.3) 
e <-  geom_point(aes(x=seq, y = Day.5, color="Day 5"),alpha=0.3)
f <-  geom_point(aes(x=seq, y = Day.6, color="Day 6"),alpha=0.3)
g <-  geom_point(aes(x=seq, y = Day.7, color="Day 7"),alpha=0.3)
h <-  geom_point(aes(x=seq, y = Day.8, color="Day 8"),alpha=0.3)
i <-  geom_point(aes(x=seq, y = Day.9, color="Day 9"),alpha=0.3)
j <-  geom_point(aes(x=seq, y = Day.10, color="Day 10"),alpha=0.3)

p +  a+b+c+d+e+f+g+h+i+j  + ylab("Traffic")

# just the regression

aa <-  geom_smooth(aes(x=seq, y = Day.1, color="Day 1" ))
bb <- geom_smooth(aes(x=seq, y = Day.2, color="Day 2"))
cc <-  geom_smooth(aes(x=seq, y = Day.3, color="Day 3"))
dd <- geom_smooth(aes(x=seq, y = Day.4, color="Day 4")) 
ee <-  geom_smooth(aes(x=seq, y = Day.5, color="Day 5"))
ff <-  geom_smooth(aes(x=seq, y = Day.6, color="Day 6"))
gg <-  geom_smooth(aes(x=seq, y = Day.7, color="Day 7"))
hh <-  geom_smooth(aes(x=seq, y = Day.8, color="Day 8"))
ii <-  geom_smooth(aes(x=seq, y = Day.9, color="Day 9"))
jj <-  geom_smooth(aes(x=seq, y = Day.10, color="Day 10"))

p +  aa+bb+cc+dd+ee+ff+gg+hh+ii+jj  + ylab("Traffic")

#now do one by one
## Traffic Day 1
# ```{r plot_3_1, echo=TRUE}
p+aa+a+ ylab("Traffic Day 1")
p+bb+b+ ylab("Traffic Day 2")
p+cc+c+ ylab("Traffic Day 3")
p+dd+d+ ylab("Traffic Day 4")
p+ee+e+ ylab("Traffic Day 5")
p+ff+f+ ylab("Traffic Day 6")
p+gg+g+ ylab("Traffic Day 7")
p+hh+h+ ylab("Traffic Day 8")
p+ii+i+ ylab("Traffic Day 9")
p+jj+j+ ylab("Traffic Day 10")
# ```

a <-  geom_point(aes(x=seq, y = Day.1, color="Day 1" ),alpha=0.3)
a <-  geom_point(aes(x=seq, y = Day.1, color="Day 1" ),alpha=0.3)
a <-  geom_point(aes(x=seq, y = Day.1, color="Day 1" ),alpha=0.3)

