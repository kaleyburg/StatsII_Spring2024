rm(list = ls())

getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(stargazer)
library(Amelia)
library(MASS)
library(ggplot2)
library("nnet")
library("MASS")

#########BEGIN REPLICATION CODE##########

#load dataset
load("data.analysis.RData")
head(data.analysis)

#create subset of secular and unorthodox respondents
#for figures
data.secular <- data.analysis[data.analysis$secular==1,]
data.uo <- data.analysis[data.analysis$uo==1,]

######Figure 1######
uo.respondents_accept_secular <- table(data.uo$secular_accept.character)[c(6,3,4,2,1,7,5)]/sum(table(data.uo$secular_accept.character))
uo.respondents_accept_arabs <- table(data.uo$arab_accept_character)[c(6,3,4,2,1,7,5)]/sum(table(data.uo$arab_accept_character))

secular.respondents_accept_uo <- table(data.secular$uo_accept_secular)[c(6,3,4,2,1,7,5)]/sum(table(data.secular$uo_accept_secular))
secular.respondents_accept_arabs <- table(data.secular$arab_accept_character)[c(6,3,4,2,1,7,5)]/sum(table(data.secular$arab_accept_character))

secular.arab.matrix <- rbind(secular.respondents_accept_uo,secular.respondents_accept_arabs)

uo.arab.matrix <- rbind(uo.respondents_accept_secular,uo.respondents_accept_arabs)

png(file='distance_secular_arabs.png') 
par(mar=c(6.5,5.5,4.1,2.1))
a<-barplot(secular.arab.matrix, beside=T, ylim=c(0,0.6), las=2, axes = FALSE, axisnames = FALSE,
           ylab="Share of respondents", cex.lab=2.2)
legend("topleft", c("Secular toward UO","Secular toward Arabs"), bty="n", 
       fill=c("black", "gray"), cex=2.2)
labs=c("relative", "friend", "neighbor", "coworker", "citizen", "visitor", "none")
text(a[1,], par("usr")[3]- 0.02, labels = labs, srt = 45, adj = 1, xpd = TRUE, cex=2)
axis(2,cex.axis = 1.6)
dev.off()

png(file='distance_uo_arabs.png') 
par(mar=c(6.5,5.5,4.1,2.1))
b <- barplot(uo.arab.matrix, beside=T, ylim=c(0,0.6), axes = FALSE, axisnames = FALSE,
             ylab="Share of respondents", cex.axis = 1.6, cex.lab=1.6)
legend("topleft", c("UO toward Secular","UO toward Arabs"), bty="n", fill=c("black", "gray"),
       cex=2)
labs=c("relative","friend",  "neighbor",  "coworker", "citizen", "visitor", "none")
text(b[1,], par("usr")[3]- 0.02, labels = labs, srt = 45, adj = 1, xpd = TRUE, cex=2)
axis(2,cex.axis = 1.6)
dev.off()

######Table 1######
#without interaction with arabs factor
arab.accept.lm.religiosity <- lm(arab_accept ~ age + foreign_born+ sex + left_right +              
                                   as.factor(religiosity)  + as.factor(education) + as.factor(income) +
                                   as.factor(ethnicity),  
                                 data=data.analysis)
summary(arab.accept.lm.religiosity) 

stargazer(arab.accept.lm.religiosity)


#with interactions factor
arab.accept.lm.religiosity.interactions <- lm(arab_accept ~ age + foreign_born + sex + left_right +              
                                                as.factor(religiosity) + as.factor(education) + as.factor(income) +
                                                as.factor(ethnicity) + as.factor(arab_interactions),  
                                              data=data.analysis)
summary(arab.accept.lm.religiosity.interactions) 

stargazer(arab.accept.lm.religiosity,arab.accept.lm.religiosity.interactions,
          no.space=T, covariate.labels=c("Age", "Foreign Born", "Sex",  "Left-right", 
                                         "Secular", "Traditional", "Ultra Orthodox", 
                                         "High-school", "Primary-school", "Undergrad",
                                         "High income", "Low income", "Very high income", "Very low income",
                                         "Mixed", "Other", "Sephardic", 
                                         "Month", "Never", "Week", "Year",
                                         "Constant"))



######Figure 2######
distance.x <- seq(min(as.numeric(data.analysis$left_right), na.rm=TRUE), max(as.numeric(data.analysis$left_right), na.rm=TRUE),1)

xnew.distance.left_right=list(left_right=distance.x, 
                              religiosity=rep("secular", length(distance.x)),
                              age=rep(median(as.numeric(as.vector(data.analysis$age)), na.rm=TRUE), length(distance.x)), 
                              foreign_born=rep("0", length(distance.x)),
                              sex=rep("1", length(distance.x)), 
                              education=rep("high", length(distance.x)),
                              income=rep("average", length(distance.x)),
                              ethnicity=rep("sep", length(distance.x))
)

pred.distance.left_right <-predict(arab.accept.lm.religiosity, newdata=xnew.distance.left_right, 
                                   interval="confidence", level=0.95)

png(file='predict_left_right.png') 
par(mar=c(7.1,9.5,4.1,2.1))
plot(distance.x, pred.distance.left_right[,1], type="n", ann=FALSE, ylim=c(1,7), axes=FALSE)
axis(2, at=1:7, lab=c("relative", "friend", "neighbor", "coworker", "citizen", "visitor", "none"), 
     las=1, cex.axis=2.5)
axis(1, at=1:7, cex.axis=2.5, tick = FALSE)
lines(distance.x, pred.distance.left_right[,1], lwd=6)
lines(distance.x, pred.distance.left_right[,2], lwd=1, lty=2)
lines(distance.x, pred.distance.left_right[,3], lwd=1, lty=2)
box()
abline(h=c(1:7), lty=3)
dev.off()




#predicting social distance by education
distance.x <- c("primary", "high", "undergrad", "grad")

xnew.distance.education=list(left_right=rep(median(as.numeric(data.analysis$left_right), na.rm=TRUE), length(distance.x)), 
                             religiosity=rep("secular", length(distance.x)),
                             age=rep(median(as.numeric(as.vector(data.analysis$age)), na.rm=TRUE), length(distance.x)), 
                             foreign_born=rep("0", length(distance.x)),
                             sex=rep("1", length(distance.x)),
                             education=distance.x,
                             income=rep("average", length(distance.x)),
                             ethnicity=rep("sep", length(distance.x))
)

pred.distance.education <-predict(arab.accept.lm.religiosity, newdata=xnew.distance.education, tryp="response",
                                  se=TRUE)

plotUpper.win <- pred.distance.education$fit + (1.96 * pred.distance.education$se.fit)
plotLower.win <- pred.distance.education$fit - (1.96 * pred.distance.education$se.fit)

png("predict_education.png")
par(mar=c(7.1,9.5,4.1,2.1))
plot(1:4, pred.distance.education$fit,ann=FALSE, ylim=c(1,7), axes=FALSE, xlim=c(0.5,4.5),
     pch=19, lwd=6)
axis(2, at=1:7, lab=c("relative", "friend", "neighbor", "coworker", "citizen", "visitor", "none"), 
     las=1, cex.axis=2.5)
text(axTicks(1), par("usr")[3]-0.25, srt=45, adj=1,
     labels=c("prim.", "high", "under.", "grad"),
     xpd=T, cex=2.5)
box()
for(x in 1:length(pred.distance.education$fit)) 
  lines(c(x,x),c(plotUpper.win[x],plotLower.win[x]), lwd=6)
abline(h=c(1:7), lty=3)
dev.off()

#predicting social distance by religiosity
distance.x <- c("secular", "trad", "orthodox", "u_orthodox")

xnew.distance.rel=list(left_right=rep(median(as.numeric(data.analysis$left_right), na.rm=TRUE), length(distance.x)), #5
                       religiosity=distance.x,
                       income=rep("average", length(distance.x)),
                       age=rep(median(as.numeric(as.vector(data.analysis$age)), na.rm=TRUE), length(distance.x)), #22
                       sex=rep("1", length(distance.x)), #male
                       foreign_born=rep("0", length(distance.x)),
                       education=rep("high", length(distance.x)),
                       income=rep("average", length(distance.x)),
                       ethnicity=rep("sep", length(distance.x))
)

pred.distance.rel <-predict(arab.accept.lm.religiosity, newdata=xnew.distance.rel, tryp="response",
                            se=TRUE)

plotUpper.win <- pred.distance.rel$fit + (1.96 * pred.distance.rel$se.fit)
plotLower.win <- pred.distance.rel$fit - (1.96 * pred.distance.rel$se.fit)

png("predict_religiosity.png")
par(mar=c(7.1,9.5,4.1,2.1))
plot(1:4, pred.distance.rel$fit, ann=FALSE, ylim=c(1,7), axes=FALSE, xlim=c(0.5,4.5),
     pch=19, lwd=6)
axis(2, at=1:7, lab=c("relative", "friend", "neighbor", "coworker", "citizen", "visitor", "none"), 
     las=1, cex.axis=2.5)
#axis(1, at=1:4, lab=c("secular", "trad.", "religious", "UO"), cex.axis=2)
text(axTicks(1), par("usr")[3]-0.25, srt=45, adj=1,
     labels=c("secular", "trad.", "religious", "UO"),
     xpd=T, cex=2.5)
box()
for(x in 1:length(pred.distance.rel$fit)) lines(c(x,x),c(plotUpper.win[x],plotLower.win[x]), lwd=6)
abline(h=c(1:7), lty=3)
dev.off()




############Table 2##########
arab.coop.glm.distance <- glm(arab_coop ~ arab_reject_binary, 
                              data=data.analysis, family=binomial)
summary(arab.coop.glm.distance) 

arab.coop.glm.distance.covariates <- glm(arab_coop ~ arab_reject_binary + age + 
                                           foreign_born + sex + left_right + 
                                           as.factor(religiosity) + as.factor(education) + as.factor(income) + as.factor(ethnicity),
                                         data=data.analysis, family=binomial)
summary(arab.coop.glm.distance.covariates) 

arab.coop.trust <- glm(arab_coop ~ arab_reject_binary + trust.b, 
                       data=data.analysis, family=binomial)
summary(arab.coop.trust) 

arab.coop.trust.covariates <- glm(arab_coop ~ arab_reject_binary + trust.b +
                                    age + foreign_born + 
                                    sex + left_right +
                                    as.factor(religiosity) + as.factor(education) + as.factor(income) + as.factor(ethnicity), 
                                  data=data.analysis, family=binomial)
summary(arab.coop.trust.covariates) 


stargazer(arab.coop.glm.distance, arab.coop.glm.distance.covariates, arab.coop.trust, arab.coop.trust.covariates,
          no.space=T,  covariate.labels=c("Social Distance (binary)", "Age", "Foreign Born", "Sex", "Left-right", 
                                          "Secular", "Traditional", "Ultra Orthodox", "High-school", "Primary school", "Undergrad",  
                                          "High", "Low", "Very high", "Very low", "Mixed", "Other","Sephardic", "Trust in Arabs", "Constant"))


# Generate LaTeX table
stargazer(
  arab.coop.glm.distance, 
  arab.coop.glm.distance.covariates, 
  arab.coop.trust, 
  arab.coop.trust.covariates,
  type = "text", 
  no.space = TRUE, 
  covariate.labels = c(
    "Social Distance (binary)", "Age", "Foreign Born", "Sex", "Left-right", 
    "Secular", "Traditional", "Ultra Orthodox", "High-school", "Primary school", "Undergrad",  
    "High", "Low", "Very high", "Very low", "Mixed", "Other", "Sephardic", "Trust in Arabs", "Constant"
  ),
  title = "Regression Results"
)


#stargazer fix

## Quick fix for stargazer <= 5.2.3 is.na() issue with long model names in R >= 4.2
# Unload stargazer if loaded
detach("package:stargazer",unload=T)
# Delete it
remove.packages("stargazer")
# Download the source
download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
# Unpack
untar("stargazer_5.2.3.tar.gz")
# Read the sourcefile with .inside.bracket fun
stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
# Move the length check 5 lines up so it precedes is.na(.)
stargazer_src[1990] <- stargazer_src[1995]
stargazer_src[1995] <- ""
# Save back
writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
# Compile and install the patched package
install.packages("stargazer", repos = NULL, type="source")
library(stargazer)
#


# Check for missing values in arab.coop.glm.distance
any(is.na(arab.coop.glm.distance))

# Check for missing values in arab.coop.glm.distance.covariates
any(is.na(arab.coop.glm.distance.covariates))

# Check for missing values in arab.coop.trust
any(is.na(arab.coop.trust))

# Check for missing values in arab.coop.trust.covariates
any(is.na(arab.coop.trust.covariates))





####MY TWIST###### ------

####ORDERED MODEL#### -----

#FIRST WE CHECK WHAT THEY DID


#social distance - ordered (robustness check)




arab.accept.polr <- polr(as.factor(arab_accept) ~ age + foreign_born +
                           as.factor(as.character(religiosity)) + sex + 
                           as.factor(education) + as.factor(income) +
                           left_right + 
                           as.factor(ethnicity),
                         data=data.analysis)

sumOrderedModTHEM <- summary(arab.accept.polr) #substantively similar results to lm

sumOrderedModTHEM

#then just to make sure, we can test it with my own
#variable

data.analysis$arab_acceptTEST <- factor(data.analysis$arab_accept, ordered = TRUE)

class(data.analysis$arab_acceptTEST)

data.analysis$arab_acceptTEST<- factor(data.analysis$arab_accept_character, 
                                          levels = c("relative", "friend", "neighbor", 
                                                     "coworker", "citizen", "visitor", "none"), 
                                          ordered = T)

data.analysis$arab_acceptTEST

arab.accept.polrTEST <- polr(arab_acceptTEST ~ age + foreign_born +
                               as.factor(as.character(religiosity)) + sex + 
                               as.factor(education) + as.factor(income) +
                               left_right + 
                               as.factor(ethnicity),
                             data=data.analysis)
summary(arab.accept.polrTEST) 

stargazer(arab.accept.polrTEST, arab.accept.polr)


#with hess = T

arab.accept.polrTEST_hess <- polr(arab_acceptTEST ~ age + foreign_born +
                               as.factor(as.character(religiosity)) + sex + 
                               as.factor(education) + as.factor(income) +
                               left_right + 
                               as.factor(ethnicity),
                             data=data.analysis, Hess = T)
summary(arab.accept.polrTEST_hess) 


#we get the same otucome when we do this


#now we need to see which categories the levels correspond to
table(data.analysis$arab_accept, data.analysis$arab_accept_character)


#so relative is 1 and none is 7
#relative - 1
#friend - 2
#neighbor - 3
#coworker - 4
#citizen - 5
#visitor - 6
#none - 7

#this matches the variable we made above too

#check further

table(data.analysis$arab_acceptTEST, data.analysis$arab_accept_character)

#so the further you move along the latent variable,
#the more prejudiced you are towards arabs

#checking around with the data

data.analysis$arab_accept_character
data.analysis$arab_accept
data.analysis$education
data.analysis$religiosity
colnames(data.analysis)
class(data.analysis$arab_accept_character)
data.analysis$income


####END ORDERED MULTINOMIAL -------


####TESTS FOR ORDERED MULTINOMIAL -----


#looking at the intercept values

sumOrderedModTHEM$zeta

#now plotting the intercept values to essentially 
#see how far away from each other they are

png(file = 'intercepts.png', width = 800, height = 400)
ggplot(data = data.frame(Zeta = sumOrderedModTHEM$zeta, Index = seq_along(sumOrderedModTHEM$zeta)), aes(x = Zeta, y = Index)) +
  geom_point(color = "blue", size = 3) +  # Change color and size of points
  labs(x = "Zeta", y = "Index") +
  theme_minimal()  # Optionally, use a minimal theme for a cleaner look
dev.off()


#exporting their  model
stargazer(arab.accept.polr)

#now lets change everything to odds ratios - with CIs
ord_OR <- exp(cbind(OR = coef(arab.accept.polr), confint(arab.accept.polr)))

#print these
print(ord_OR)

#export to latex
stargazer(ord_OR)


#confidence intervals for log odds ----
#https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/

#save summary of model
coefs <- (summary(arab.accept.polr))

#these are the coefficients of the omodel
coef(summary(arab.accept.polr))

#make table of values and std errors
ctable <- coef(summary(arab.accept.polr))
ctable


## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
print(p)


## combined table
ctable <- cbind(ctable, "p value" = sprintf("%.20f", p))

#adding p values to the table
ctable

# Obtain confidence intervals for coefficients
ci <- confint(arab.accept.polr)
ci
# Extract coefficients and their confidence intervals
coefficients <- coef(arab.accept.polr)
ci_lower <- ci[, 1]
ci_upper <- ci[, 2]

# Combine coefficients and confidence intervals into a data frame
coef_ci <- data.frame(coefficients, Lower_CI = ci_lower, Upper_CI = ci_upper)
coef_ci

print(coef_ci)
stargazer(coef_ci)


#can now look at this in latex
stargazer(coef_ci)

#so now we can compare the p values and the CIs for both the
#log odds and odds ratio to see if the claim holds that it
#is 'substantively similar' to the linear model



#plot of the cut points show that they are not equidistance apart

#quote from textbook - in general, the results of the LRM only
#correspond to those of the ORM if the thresholds are all
#about the same distance apart - page 118-119


#our plot here implies that this is not the case - first
#indication that maybe LRM is not a good fit



#looking back their linear regression model
summary(arab.accept.lm.religiosity)


#so lets run logit models to check the parallel lines
#assumption

# Iterate over each unique category of the outcome variable
for (i in 1:length(levels(data.analysis$arab_acceptTEST))) {
  # Create a model for each category
  assign(paste("logit_model_", i, sep=""), 
         glm(arab_acceptTEST == levels(arab_acceptTEST)[i] ~ age + foreign_born +
               as.factor(as.character(religiosity)) + sex + 
               as.factor(education) + as.factor(income) +
               left_right + 
               as.factor(ethnicity),
             data = data.analysis, family = binomial),
         envir = globalenv())
}


model_list <- list()



# Iterate over each logit model
for (i in 1:7) {
  # Create a model for each category and store it in the list
  model_name <- paste("logit_model_", i, sep = "")
  model_list[[i]] <- get(model_name)
}

#export it to latex
stargazer(model_list, type = "latex")


#would imply parallel line assumption does not hold - we should
#use an unordered model because of the sign flips




###UNORDERED MODEL #### ------

#releveling with none as the reference

#data.analysis$arab_acc_fact <- relevel(data.analysis$arab_accept_character, ref = "none")

arab.accept.polr <- polr(as.factor(arab_accept) ~ age + foreign_born +
                           as.factor(as.character(religiosity)) + sex + 
                           as.factor(education) + as.factor(income) +
                           left_right + 
                           as.factor(ethnicity),
                         data=data.analysis)

arabAcceptUnord3 <- multinom(arab_acc_fact_unord ~ age + foreign_born +
                           as.factor(as.character(religiosity)) + sex + 
                           as.factor(education) + as.factor(income) +
                           left_right + 
                           as.factor(ethnicity),
                         data=data.analysis)


arabAcceptUnord2 <- multinom(arab_acc_fact_unord ~ age + foreign_born +
                              as.factor(as.character(religiosity)) + sex + 
                              as.factor(education) + as.factor(income) +
                              left_right + 
                              as.factor(ethnicity),
                            data=data.analysis)


summary(arabAcceptUnord2)
summary(arabAcceptUnord)
summary(arabAcceptUnord3)

as.factor(as.character(data.analysis$religiosity))

table(data.analysis$religiosity)
class(data.analysis$religiosity)
as.factor(as.character(data.analysis$religiosity))

as.factor(data.analysis$income)

data.analysis$arab_acc_fact_unord <- factor(data.analysis$arab_accept_character, 
                                            levels = c("relative", "friend", "neighbor", 
                                                       "coworker", "citizen", "visitor", "none"), 
                                            ordered = F)

# checking to make sure i converted this correctly
arab_acc_fact_char <- as.character(data.analysis$arab_acc_fact_unor)
arab_accept_character_char <- as.character(data.analysis$arab_accept_character)

# Compare character vectors
mismatches <- which(arab_acc_fact_char != arab_accept_character_char)

mismatches

#no mismatches
table(data.analysis$arab_acc_fact_unord, data.analysis$arab_accept)

#now relevel with reference as none
data.analysis$arab_acc_fact_unord <- relevel(data.analysis$arab_acc_fact_unord, ref = "none")



# run model
arabAcceptUnord <- multinom(arab_acc_fact_unord~ age + foreign_born+ sex + left_right +              
                              as.factor(as.character(religiosity))  + as.factor(education) + as.factor(income) +
                               as.factor(ethnicity),  
                             data=data.analysis)


summary(arabAcceptUnord)

stargazer(arabAcceptUnord)

#options(scipen = 999)  # Set scipen to a high value to avoid scientific notation
#OR_unord <- exp(coef(arabAcceptUnord))
#OR_unord
#options(scipen = 0) 


plot(data.analysis$education)


OR_unord <- exp(coef(arabAcceptUnord))
OR_unord

sprintf("%.20f", exp(coef(arabAcceptUnord)))

sprintf("%.20f", OR_unord)


ciUnord <- confint(arabAcceptUnord)
ciUnord
stargazer(OR_unord)

#make predictions
predictions <- predict(arabAcceptUnord, newdata = subset_data, na.rm = TRUE)
length(predictions)
table(predictions)
data.analysis$arab_accept_character

length(data.analysis$arab_accept_character)


#create a table of actual vs. predicted classes
pred_table <- table(data.analysis$arab_accept_character, predictions)
pred_table


#add margins
pred_tab_marg <- addmargins(prediction_table)
print(pred_tab_marg)


#stargazer didnt work
stargazer(pred_tab_marg)

#trying xtable
#library(xtable)

#create latex table
latex_table <- xtable(pred_tab_marg)


print(latex_table, include.rownames = TRUE)


#had to subset the data with just these factors to get it to
#print the predictions
subset_data <- data.analysis[, c("arab_acc_fact_unord", "age", "foreign_born", 
                                 "religiosity", "sex", "education", 
                                 "income", "left_right", "ethnicity")]

subset_data
###----

table(data.analysis$arab_accept_character)
table(data.analysis$arab_coop)


table(data.analysis$uo_accept_secular)


data.analysis$arab_reject_binary


arabAcceptUnordBin <- multinom(arab_reject_binary~ age + foreign_born+ sex + left_right +              
                              as.factor(as.character(religiosity))  + as.factor(education) + as.factor(income) +
                              as.factor(ethnicity),  
                            data=data.analysis)
summary(arabAcceptUnordBin)
ORUnordBin <- exp(coef(arabAcceptUnordBin))
ORUnordBin


stargazer(arabAcceptUnordBin)
stargazer(ORUnordBin)

#create latex table
latex_table <- xtable(as.data.frame(ORUnordBin))
?xtable

print(latex_table, include.rownames = TRUE)


#predictions


subset_dataBIN <- data.analysis[, c("arab_reject_binary", "age", "foreign_born", 
                                 "religiosity", "sex", "education", 
                                 "income", "left_right", "ethnicity")]


#make predictions
predictionsBin <- predict(arabAcceptUnordBin, newdata = subset_dataBIN, na.rm = TRUE)
length(predictionsBin)
table(predictionsBin)
data.analysis$arab_reject_binary

length(data.analysis$arab_reject_binary)


#create a table of actual vs. predicted classes
pred_tableBIN <- table(data.analysis$arab_reject_binary, predictions)
pred_tableBIN


#add margins
pred_tabBIN_marg <- addmargins(pred_tableBIN)
print(pred_tabBIN_marg)


#trying xtable
#library(xtable)

#create latex table
latex_tableBIN <- xtable(pred_tabBIN_marg)


print(latex_tableBIN, include.rownames = TRUE)


data.analysis$arab_interactions

arabAcceptUnordBin2 <- glm(arab_reject_binary~ age + foreign_born+ sex + left_right +              
                                 as.factor(as.character(religiosity))  + as.factor(education) + as.factor(income) +
                                 as.factor(ethnicity),  
                               data=data.analysis,
                           family = "binomial")
summary(arabAcceptUnordBin2)

