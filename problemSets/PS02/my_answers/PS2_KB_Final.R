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

lapply(c('ggplot2', 'stargazer'),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#####################
# Problem 1 -------
#####################

# load data

#importing dataset

load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))


#q1 fit an additive model

#first we change the outcome variable to 0 and 1
climateSupport$choice

climateSupport$choice_bin <- ifelse(climateSupport$choice == "Supported", 1, 0)

#run a table to make sure it worked

table(climateSupport$choice, climateSupport$choice_bin)

#it worked

climateSupport$countries <- factor(climateSupport$countries, levels = levels(climateSupport$countries), ordered = F)
climateSupport$sanctions <- factor(climateSupport$sanctions, levels = levels(climateSupport$sanctions), ordered = F)

class(climateSupport$sanctions)
class(climateSupport$countries)


#change the levels to unordered because this has the regression treat 
#the variable as dummy variables
levels(climateSupport$countries) <- gsub(" ", "", levels(climateSupport$countries))
levels(climateSupport$sanctions) <- gsub(" ", "", levels(climateSupport$sanctions))



mod <- glm(choice_bin ~ sanctions + countries, 
           data = climateSupport, 
           family = "binomial")


summary(mod)

stargazer(mod, type = 'latex')

#global null: coefficients equal to 0
#test with full vs reduced likelihood ration test

modnull <- glm(choice_bin ~ 1,
               family = binomial(link = "logit"),
               data = climateSupport)

globalnulltest <- anova(modnull, mod, test = "LRT")

globalnulltest

stargazer(globalnulltest, type = "latex", summary = FALSE, title = "ANOVA Results")

#reject global null, p of <2.2e16

################--------
####Problem 2 -----
#############



#####---- 2a

#For the policy in which nearly all countries participate [160 of 192], 
#how does increasing sanctions from 5% to 15% change the odds that an 
#individual will support the policy? (Interpretation of a coefficient)

#going from 5% to 15%

#can be done 2 ways

#first can set reference category to 5 and we get a log odds
#of -.32510 
#so going from 5 to 15% is associated with a .32510 decrease in log odds
# of supporting the policy



climateSupport$sanctionstest <- relevel(climateSupport$sanctions, ref = "5%")

mod2 <- glm(choice_bin ~ sanctionstest + countries, 
           data = climateSupport, 
           family = "binomial")


summary(mod2)
stargazer(mod2)
#second way is doing it by hand with the ref category at None



#####3 2b ------
#math for estimated probabilities (predicted probabilities)


1/(1 + exp(-(-0.27266 + 0.33636)))

#we get 0.5159191

#can also use plogis for inverse logit

plogis(0.33636 - 0.27266)

#and then we can make a predicted data dataframe with
#all of the predicted values

#i adapted lecture slides code from here
#for some reason it was taking the inverse log of the predictedprob
#so i just used predict to predict log odds of the response variable
#then use plogis to convert these log odds into probabilities

?predict

predicted_data <- with(climateSupport, data.frame(countries, sanctions))
predicted_data$PredictedProb <- plogis(predict(mod, newdata = predicted_data, type = "link"))

#print result 
predicted_data

# Plotting predicted probabilities
png("mod_predprobs.png", width = 400, height = 200)
ggplot(predicted_data, aes(x = sanctions, y = PredictedProb, color = countries)) +
  geom_point(size = 3) +
  labs(x = "Sanctions", y = "Predicted Probability", color = "Country") +
  theme_minimal()
dev.off()

#this makes sense with the math by hand
#we can check again also using this
predval <- predicted_data[predicted_data$sanctions == 'None' & predicted_data$countries == '80of192', ]

#and check it

unique(predval)
unique(predicted_data)
#we get the same as doing it by hand


####2c -----


#mod interaction

mod_interaction <- glm(choice_bin ~ sanctions * countries, 
                       data = climateSupport, 
                       family = "binomial")
summary(mod_interaction)

#making predicted values of the interaction model

predicted_data2 <- with(climateSupport, data.frame(countries, sanctions))
predicted_data2$PredictedProb <- plogis(predict(mod_interaction, newdata = predicted_data2, type = "link"))

#print result 

predicted_data
png("modint_predprobs.png", width = 400, height = 200)
ggplot(predicted_data2, aes(x = sanctions, y = PredictedProb, color = countries)) +
  geom_point(size = 3) +
  labs(x = "Sanctions", y = "Predicted Probability", color = "Country") +
  theme_minimal()
dev.off()
#can look at the predicted values for 80 at no sanctions

predval2 <- predicted_data2[predicted_data2$sanctions == 'None' & predicted_data2$countries == '80of192', ]

#and check it

unique(predval2)
unique(predicted_data2)
#we get the same as doing it by hand

1/(1 + exp(-(-0.27469 + 0.37562)))

#### 2c1 -----
#test to see if interaction is appropriate

int_anova <- anova(mod, mod_interaction, test = "LRT")
stargazer(int_anova)
# p value is 0.3912, so not able to reject the null
#so interaction is not appropriate

exp(-0.2727)


#odds ratios code
odds_ratios <- exp(mod$coefficients)
odds_ratios

stargazer(odds_ratios)

stargazer(unique(predicted_data))
stargazer(unique(predicted_data2))


1-.875
1 - .738
1 - 0.7224531
exp(mod2$coefficients)

exp(-0.32510)
exp(-0.182)
stargazer(mod_interaction)
