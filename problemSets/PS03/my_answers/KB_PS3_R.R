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

lapply(c("nnet", "MASS", "stargazer"),  pkgTest)



# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()



#####################
# Problem 1
#####################



# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)


#subsetting
gdp_data_sub <- gdp_data[,c("GDPWdiff", 'REG', "OIL")]


#creating an ordered factor
gdp_data_sub$GDPWdiff_cut <- ifelse(gdp_data_sub$GDPWdiff < 0, "Negative",
                                    ifelse(gdp_data_sub$GDPWdiff == 0, "NoChange", "Positive"))


gdp_data_sub$GDPWdiff_ord <- factor(gdp_data_sub$GDPWdiff_cut,
                                      levels = c("Negative", "NoChange", "Positive"),
                                      ordered = T)


#creating an unordered factor
gdp_data_sub$GDPWdiff_unord <- factor(gdp_data_sub$GDPWdiff_cut,
                                    levels = c("Negative", "NoChange", "Positive"),
                                    ordered = F)

#unordered model

#releveling with no change as the reference

gdp_data_sub$GDPWdiff_unord <- relevel(gdp_data_sub$GDPWdiff_unord, ref = "NoChange")


# run model
unord_reg <- multinom(GDPWdiff_unord ~ REG + OIL, data = gdp_data_sub)

unord_sum<- summary(unord_reg)

unord_sum

stargazer(unord_reg)

#covverting to odds ratio
unord_OR <- exp(coef(unord_sum))

print(unord_OR)

stargazer(unord_OR)



#ordered model

ord_log <- polr(GDPWdiff_ord ~ REG + OIL, data = gdp_data_sub, Hess = TRUE)


summary(ord_log)

ord_sum <- summary(ord_log)

ord_sum

stargazer(ord_log)


#converting to odds ratios

ord_OR <- exp(cbind(OR = coef(ord_log), confint(ord_log)))

print(ord_OR)

stargazer(ord_OR)

#estimate and compare simplified model


# Iterate over each unique category of the outcome variable
for (i in 1:length(unique(gdp_data_sub$GDPWdiff_cut))) {
  # Create a model for each category
  assign(paste("logit_model_", i, sep=""), 
         glm(ifelse(gdp_data_sub$GDPWdiff_cut == unique(gdp_data_sub$GDPWdiff_cut)[i], 1, 0) ~ OIL + REG,
             data = gdp_data_sub),
         envir = globalenv())
}

#looking at each of the 3 logit models created

print(logit_model_1)
print(logit_model_2)
print(logit_model_3)

head(gdp_data_sub)


# Get predicted probabilities for each category

# we can use predicted probabilities to help our interpretation
pred_probs <- data.frame(fitted(mult_log))
head(data.frame(gdp = gdp_data_sub$GDPWdiff_ord,
                P = pred_probs$Positive,
                NC = pred_probs$NoChange,
                N = pred_probs$Negative))

unique(pred_probs)



#basepredict.polr(ord_log)

#pred_probs2 <- cbind(gdp_data_sub, predict(mult_log, newdata = gdp_data_sub, type = 'probs', se = TRUE))

#pred_probs
#head(pred_probs2)
#unique(head(pred_probs2))



#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")


poisson_mod <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = mexico_elections, family = poisson)


summary(poisson_mod)


stargazer(poisson_mod)

# Create scatterplot
plot(mexico_elections$competitive.district, mexico_elections$PAN.visits.06)
# Extract coefficients
coeffs <- coefficients(poisson_mod)
# Sort x values
xvalues <- sort(mexico_elections$competitive.district)
# Calculate means
means <- exp(coeffs[1] + coeffs[2] * xvalues)
# Add lines to the scatterplot
lines(xvalues, means, lty = 2, col = "red")

#odds ratios

poisson_OR <- exp(cbind(OR = coef(poisson_mod), confint(poisson_mod)))
poisson_OR

stargazer(poisson_OR)

#. Is there evidence that PAN presidential candidates visit
#swing districts more? Provide a test statistic and p-value.

#there is a decrease in the log counts going from a non competitive
#to competitive district (safe to swing state)
#this result is not statistically sig (z = -0.477 p = 0.6336)
#so there is not evidence of this hypo

#) Interpret the marginality.06 and PAN.governor.06 coefficients.

#marg: for every 1 unit increase in marginality, there is, on average,
#a 2.08 decrease in the log counts of the PAN presidential candidate
#visiting a a district
  #associated with a multiplic. change of 0.124 in the odds
  #of them visiting

#pangov: when a state has a PAN-affiliated governor, compared to states
#without pan affil govs, there is an, on average, 0.311 decrease
#in the log counts of the PAN pres cand visiting a district
  #associated with a multiplic. change of 0.732 in the odds
  #of them visiting


#) Provide the estimated mean number of visits from the winning PAN presidential 
#candidate for a hypothetical district that was competitive 
#(competitive.district=1), had an average poverty level 
#(marginality.06 = 0), and a PAN governor (PAN.governor.06=1).

# Extract coefficients
coeffs <- coefficients(poisson_mod)

estimated_mean <- exp(coeffs[1] + coeffs[2]*1 + coeffs[3]*0 + coeffs[4]*1)

stargazer(estimated_mean)
 
#estimated mean number of visits: 0.0149
#estimated variance in number of visits: ?

range(mexico_elections$PAN.visits.06)
range(mexico_elections$competitive.district)
range(mexico_elections$PAN.governor.06)

predicted_values <- cbind(predict(poisson_mod, 
                                  data.frame(competitive.district = seq(0, 35, 5),
                                             marginality.06 = rep(seq(0, 1), each = 8),
                                             PAN.governor.06 = rep(seq(0.1), times = 8)), 
                                  type = "response", 
                                  se.fit = TRUE), 
                          data.frame(competitive.district = seq(0, 35, 5),
                                     marginality.06 = rep(seq(0, 1), each = 8),
                                     PAN.governor.06 = rep(seq(0.1), times = 8)))

predicted_values$lowerBound <- predicted_values$fit - 1.96 * predicted_values$se.fit
predicted_values$upperBound <- predicted_values$fit + 1.96 * predicted_values$se.fit

# Step 3: Put everything into a form you can use in R
predicted_values
plot(predicted_values)


#none of this was useful, delete later ^^
library('MASS')
install.packages('AER')
library('AER')

#had to use AER instead of MASS, idk why
dispersiontest(poisson_mod)

#results not statsig
install.packages('pscl')

library('pscl')

zeroinf_pois <- zeroinfl(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = mexico_elections, family = poisson)
zeroinf_pois

poisson_OR_zi <- exp(cbind(OR = coef(zeroinf_pois), confint(zeroinf_pois)))
poisson_OR_zi


#testing lambda for zip 


coeffs2 <- coefficients(zeroinf_pois)

estimated_mean_zip <- exp(coeffs2[1] + coeffs2[2]*1 + coeffs2[3]*0 + coeffs2[4]*1)
estimated_mean_zip


#main questions: cutoff points
#predicted probs
  #for both ordered - cutoff points?
  #and for unordered, where is the code? slide 14
  #uses the fake data model? 
#line 187 about mean variance
  #that is just to remember that the conditional mean is tied
  #to the condition variance
  
#why is my zip model so different but the test stat stuff
  #says i dont need one
#dont put this on the problem set

#code from lecture slides

