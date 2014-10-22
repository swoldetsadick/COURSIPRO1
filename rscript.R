# Setting seed
set.seed(123)

# We perform 1000 simulations with 40 samples
n <- 40
lambda <- .2
nosim <- 1000


data <- matrix(rexp(nosim * n, rate = lambda), nosim, n)

Rmeans <- apply(data, 1, mean)

xbar <- round(mean(Rmeans), digits = 2)
true <- 1/lambda

library(ggplot2)
title <- 'Distribution of Sample Means (Theoretical vs Simulated)'
subtitle <- 'Samples drawn from exponential distribution with parameter .2'
m <- ggplot(data.frame(Rmeans), aes(Rmeans)) 
m <- m + geom_histogram(aes(y =..density..), binwidth = (round(max(Rmeans)-min(Rmeans))/40), color = "black", fill = "white")
m <- m + xlab("Sample Averages") + ylab("Density") + ggtitle(bquote(atop(.(title), atop(italic(.(subtitle)), "")))) 
m <- m + geom_density(col="red", alpha=.2, fill="#FF6666") + theme(plot.title = element_text(size=30,lineheight=.8,vjust=-1))
m <- m + stat_function(fun = dnorm, args = list(mean = 1/lambda, sd = (1/lambda/sqrt(n))), color = "blue")
m <- m + geom_vline(aes(xintercept = mean(Rmeans)), color = "red", linetype = "dashed", size = 1)
m <- m + geom_vline(aes(xintercept = (1/lambda)), color = "blue", linetype = "dashed", size = 1)
m <- m + annotate("text", label = "Simulated", x = 3.75, y = .3, size = 4, colour = "red")
m <- m + annotate("text", label = "Theoretical", x = 7, y = .1, size = 4, colour = "blue")
m <- m + annotate('text', label =  paste(paste(paste("E(","bar(X)", sep=""),"):", sep=""),xbar,sep=" "),parse=T, x = 5.75, y = .6, size = 8, colour = "red")
m <- m + annotate('text', label =  paste(paste(paste("mu", ": 1/", sep=""), "lambda", sep=""), " : 5", sep =""),parse=T, x = 4.25, y = .65, size = 8, colour = "blue")
m