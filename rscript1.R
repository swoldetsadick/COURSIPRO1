# Setting seed
set.seed(123)

# We perform 1000 simulations with 40 samples
n <- 40
lambda <- .2
nosim <- 1000


data <- matrix(rexp(nosim * n, rate = lambda), nosim, n)



xbar <- round(mean(apply(data, 1, mean)), digits = 2)
mu <- 1/lambda

library(ggplot2)
Rmeans <- apply(data, 1, mean)
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
#Calculating averages by row, or for the 1000 different simulations over the 40 sample drawn
Ssqr <- round((sd(apply(data, 1, mean))), digits = 2)
#Calculating theoretical mean
mu <- round(((1/lambda)/n), digits = 2)

lambdavals <- seq(.1, .4, by = 0.01)
nosim1 <- 10
n1 <- 10

coverage <- sapply(lambdavals, function(lambd) {
        set.seed(123)
        mu_hats   <- apply(matrix(rexp(n1*nosim1, rate = lambd), nosim1, n1), 1, mean)
        theta_hat <- sd(apply(matrix(rexp(n1*nosim1, rate = lambd), nosim1, n1), 1, mean))
        ll <- mu_hats - qnorm(0.975) * theta_hat
        ul <- mu_hats + qnorm(0.975) * theta_hat
        mean(ll < (1/lambd) & ul > (1/lambd))
        })

# Plot coverage
library(ggplot2)
qplot(lambdavals, coverage) + geom_hline(yintercept=0.95, col=2)