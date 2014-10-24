library(datasets)
data(ToothGrowth)
data <- ToothGrowth
names(data)
sum(is.na(data))
dim(data)
data$dose <- ifelse(data$dose == .5, "low", ifelse(data$dose == 1, "med", "hi"))
for(i in 1:3){if(i!=1) {data[,i] <- as.factor(data[,i])} else {data[,i] <- as.numeric(data[,i])}}
str(data)

attach(data)
library(Hmisc)
mytable <- table(supp, dose, cut2(len, g = 4))
ftable(mytable, dnn = c("Supplement type", "Dose in milligrams", "Tooth length"))


m = ftable(supp, dose, cut2(len, g = 4), row.vars=1:2) # 3-way table of group means
names(attr(m, "row.vars")) = c("Supplement Type","Dose") # Add names for the row variables
names(attr(m, "col.vars")) = "Tooth Length" # ...and the column variable
write.ftable(m, file="", digits=3, quote=FALSE) # Pretty-print the table to a file


library(scatterplot3d)
with(data, {scatterplot3d(len, dose, supp, main = "3-D Scatterplot Example 1")})

library(ggplot2)
ggplot(data, aes(x = len, y = supp, color = dose)) + geom_point(shape = 1)

tapply(len, list(dose), mean)

tapply(len, list(supp), mean)

tapply(len, list(supp, dose), mean)

twoway = aov(len~supp * dose, data=data)
summary(twoway)
twoway = aov(len~supp + dose, data=data)
summary(twoway)


