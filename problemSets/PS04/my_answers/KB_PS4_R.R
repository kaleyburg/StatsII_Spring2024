##################################
#PS4#
##################################


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

lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)


getwd()
#import data

data(child)

child_surv <- with(child, Surv(enter, exit, event))
#creating km object to use for plots
km <- survfit(child_surv ~ 1, data = child)



summary(km, times = seq(0, 15, 1))

plot(km, main = "Kaplan-Meier Plot", xlab = "Years", ylim = c(0.7, 1))

#plot code 1
plot_km <- autoplot(km)

ggsave("km_survival_plot.png", plot_km)

child$m.age


#this plot didnt work - bc not categorical
km_mage <- survfit(child_surv ~ m.age, data = child)
summary (km_mage)
autoplot(km_mage)

#plot code 2
#but this one did - because categorical
km_sex <- survfit(child_surv ~ sex, data = child)
summary (km_sex)
sex_km_plot <- autoplot(km_sex)

ggsave("km_sex_survival_plot.png", sex_km_plot)

# fit a Cox Proportional Hazard model using mother’s age and 
#infant’s gender
#as covariates. Present and interpret the output.

cox <- coxph(child_surv ~ sex + m.age, data = child)
summary(cox)


drop1(cox, test = "Chisq")


stargazer(cox, type = "latex")

# There is a 0.08 decrease in the expected log of the hazard for female babies compared to 
# male, holding m.age constant. There is a 0.008 increase in the expected log of the hazard
# for babies for every one unit increase in mother's age, holding sex constant.

exp(cox$coefficients)
stargazer(exp(cox$coefficients))

# exponentiate parameter estimates to obtain hazard ratios
exp(-0.082215)
exp(0.007617)
# The hazard ratio of female babies is 0.92 that of male babies, i.e. female babies are less
# likely to die (92 female babies die for every 100 male babies; female deaths are 8% lower, etc.)

# For every one unit increase in mother's age, the hazard ratio 
# increases by 1.01,
# i.e. babies with younger mothers are more
# likely to die 
#(101 babies with older mothers for for every 100 babies with younger mothers

