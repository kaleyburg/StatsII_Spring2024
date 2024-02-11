#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#####################
# Problem 1
#####################

#setting seed

set.seed(123)

#creating data

data <- (rcauchy(1000, location = 0, scale = 1))


#creating a function that takes the data as a parameter and does the 
#Kolgomoriv-Smirnoff test using the normal distribution as the reference dist


kolsmir_pval <- function(data) {
  
  set.seed(123)
  
  # create empirical distribution of observed data
  
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  
  #generating the test statistic
  
  D <- max(abs(empiricalCDF - pnorm(data)))
  
  #setting n
  n <- length(data)
  
  #initializing sum portion of the formula to 0
  sum1 <- 0
  
  #for loop for 1 to 1000 (instead of infinity)
  for (k in 1:1000) {
    sum1 <- sum1 + exp((-((2*k-1)^2)*(pi^2))/((8*D^2)))
  }
  
  #then we take the sum we created above and multiply by the other part
  #of the formula
  
  #this gives the overall p value
  p <- (((2*pi)^(1/2))/D)*sum1
  
  
  #making it print like it would in ks.test
  
  cat("D =", D, ", p-value =", p)
  
  return(list(D = D, p = p))
}

kolsmir <- kolsmir_pval(data)
kolsmir

ks_result <- ks.test(data, pnorm)
ks_result




#making sure the p-values are close enough

sprintf("%.30f", 5.652523e-29)

sprintf("%.30f" ,2.2e-16)

#they are both essentially 0






#####################
# Problem 2
#####################
rm(list=ls())

#creating the data
#calling it data2 to avoid confusion with the first problem

set.seed(123)
data2 <- data.frame(x = runif(200, 1, 10))
data2$y <- 0 + 2.75*data2$x + rnorm(200, 0, 1.5)

#creating the linear_likelihood function 

linear_lik <- function(theta, y, X) {
  n <- nrow(X)
  k <- ncol(X)
  beta <- theta[1:k]
  sigma2 <- theta[k + 1]^2
  e <- y - X%*%beta
  logl <- -.5*n*log(2*pi) -.5*n*log(sigma2) - ((t(e) %*% e) / (2 * sigma2) )
  return(-logl)
}


#using the optima function to create a linear_MLE function
#that uses the linear_lik function

linear_MLE <- optim(
  fn = linear_lik,
  par = c(1, 1, 1),
  hessian = TRUE,
  y = data2$y,
  X = cbind(1, data2$x),
  method = "BFGS"
)


#call $par to check if the estimate are the same as using lm

linear_MLE$par

lm_model <- summary(lm(y~x , data2))

lm_model

#stargazer for my function results


# Load the stargazer package
library(stargazer)

# Print the MLE results using stargazer
stargazer(linear_MLE$par, type = "latex", title = "Maximum Likelihood Estimation Results", summary = FALSE)


#the third value is:
#estimate of joint variance, sigma(?) in lm
#but the first two estimates are the same, so function worked


surface <- list()
k <- 0
for (beta in seq(0, 5, 0.1)) {
  for (sigma in seq(0.1, 5, 0.1)) {
    k <- k + 1
    logL <- linear_lik(theta = c(0, beta, sigma), y = data2$y, X = cbind(1, data2$x))
    surface[[k]] <- data.frame(beta = beta, sigma = sigma, logL = -logL)
  }
}

surface <- do.call(rbind, surface)

library(lattice)

png(filename = "plot.png", width = 800, height = 600)
wireframe(logL ~ beta * sigma, surface, shade = TRUE)
dev.off()

(-1.4390716)^2
