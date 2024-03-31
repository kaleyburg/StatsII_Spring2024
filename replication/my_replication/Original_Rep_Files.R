rm(list = ls())

#library(MASS)
library(stargazer)
library(Zelig)
library(Amelia)

#########BEGIN REPLICATION CODE##########
load("data.analysis.Rdata")
head(data.analysis)
data.secular <- data.analysis[data.analysis$secular==1,]
data.uo <- data.analysis[data.analysis$uo==1,]

######Figure 1######
uo.respondents_accept_secular <- table(data.uo$secular_accept.character)[c(6,3,4,2,1,7,5)]/sum(table(data.uo$secular_accept.character))
uo.respondents_accept_arabs <- table(data.uo$arab_accept_character)[c(6,3,4,2,1,7,5)]/sum(table(data.uo$arab_accept_character))

secular.respondents_accept_uo <- table(data.secular$uo_accept_secular)[c(6,3,4,2,1,7,5)]/sum(table(data.secular$uo_accept_secular))
secular.respondents_accept_arabs <- table(data.secular$arab_accept_character)[c(6,3,4,2,1,7,5)]/sum(table(data.secular$arab_accept_character))

secular.arab.matrix <- rbind(secular.respondents_accept_uo,secular.respondents_accept_arabs)

uo.arab.matrix <- rbind(uo.respondents_accept_secular,uo.respondents_accept_arabs)

#pdf(file='distance_secular_arabs.pdf') 
par(mar=c(6.5,5.5,4.1,2.1))
a<-barplot(secular.arab.matrix, beside=T, ylim=c(0,0.6), las=2, axes = FALSE, axisnames = FALSE,
           ylab="Share of respondents", cex.lab=2.2)
legend("topleft", c("Secular toward UO","Secular toward Arabs"), bty="n", 
       fill=c("black", "gray"), cex=2.2)
labs=c("relative", "friend", "neighbor", "coworker", "citizen", "visitor", "none")
text(a[1,], par("usr")[3]- 0.02, labels = labs, srt = 45, adj = 1, xpd = TRUE, cex=2)
axis(2,cex.axis = 1.6)
#dev.off()

#pdf(file='distance_uo_arabs.pdf') 
par(mar=c(6.5,5.5,4.1,2.1))
b <- barplot(uo.arab.matrix, beside=T, ylim=c(0,0.6), axes = FALSE, axisnames = FALSE,
             ylab="Share of respondents", cex.axis = 1.6, cex.lab=1.6)
legend("topleft", c("UO toward Secular","UO toward Arabs"), bty="n", fill=c("black", "gray"),
       cex=2)
labs=c("relative","friend",  "neighbor",  "coworker", "citizen", "visitor", "none")
text(b[1,], par("usr")[3]- 0.02, labels = labs, srt = 45, adj = 1, xpd = TRUE, cex=2)
axis(2,cex.axis = 1.6)
#dev.off()

######Table 1######
arab.accept.lm.religiosity <- lm(arab_accept ~ age + foreign_born+ sex + left_right +              
                                   as.factor(religiosity)  + as.factor(education) + as.factor(income) +
                                   as.factor(ethnicity),  
                                 data=data.analysis)
summary(arab.accept.lm.religiosity) 

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

#pdf(file='predict_left_right.pdf') 
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
#dev.off()

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

#pdf("predict_education.pdf")
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
#dev.off()

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

#pdf("predict_religiosity.pdf")
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
#dev.off()

#social distance - ordered (robustness check)

arab.accept.polr <- polr(as.factor(arab_accept) ~ age + foreign_born +
                           as.factor(as.character(religiosity)) + sex + 
                           as.factor(education) + as.factor(income) +
                           left_right + 
                           as.factor(ethnicity),
                         data=data.analysis)
summary(arab.accept.polr) #substantively similar results to lm

#######Information presented in"Preferences for Exclusion and Cooperation"#######

arab.reject <- data.analysis[data.analysis$arab_reject_binary==1,]
arab.accept <- data.analysis[data.analysis$arab_reject_binary==0,]

print(mean(as.numeric(as.vector(arab.reject$arab_coop)), na.rm=T))
print(mean(as.numeric(as.vector(arab.accept$arab_coop)), na.rm=T))
print(t.test(as.numeric(as.vector(arab.reject$arab_coop)), as.numeric(as.vector(arab.accept$arab_coop))))

#######Information presented in "Data and Research Design"#######

data.analysis.with_distance <- data.analysis[(! is.na(data.analysis$arab_coop)) &
                                               (! is.na(data.analysis$arab_reject_binary)),]
table(data.analysis.with_distance$arab_coop) / sum(table(data.analysis.with_distance$arab_coop))
table(data.analysis.with_distance$secular_coop) / sum(table(data.analysis.with_distance$secular_coop))
table(data.analysis.with_distance$uo_coop) / sum(table(data.analysis.with_distance$uo_coop))

#cooperation by order
arab_first <- data.analysis[data.analysis$arab_order==1,]
arab_second <- data.analysis[data.analysis$arab_order==2,]
arab_third <- data.analysis[data.analysis$arab_order==3,]

print(mean(arab_first$arab_coop==1, na.rm=T))
print(mean(arab_second$arab_coop==1, na.rm=T))
print(mean(arab_third$arab_coop==1, na.rm=T))

print(t.test(arab_first$arab_coop==1, arab_second$arab_coop==1))
print(t.test(arab_first$arab_coop==1, arab_third$arab_coop==1))
print(t.test(arab_second$arab_coop==1, arab_third$arab_coop==1))

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
######Table A2######
arab.treatment <- lm(a_treatment ~ arab_accept + age + foreign_born +
                       sex + left_right +              
                       as.factor(religiosity)  + as.factor(education) + as.factor(income) +
                       as.factor(ethnicity) + as.factor(arab_interactions),  
                     data=data.analysis)
summary(arab.treatment) 

stargazer(arab.treatment,
          no.space=T, covariate.labels=c("Social Distance (binary)", "Age", "Foreign Born", "Sex",  "Left-right", 
                                         "Secular", "Traditional", "Ultra Orthodox", 
                                         "High-school", "Primary-school", "Undergrad",
                                         "High income", "Low income", "Very high income", "Very low income",
                                         "Mixed", "Other", "Sephardic", 
                                         "Month", "Never", "Week", "Year",
                                         "Constant"))
######Table A3######
use.data = data.analysis[,c("arab_coop","arab_reject_binary","age","foreign_born","sex",
                            "left_right","religiosity","education","income","ethnicity")]

##WE SHOULD ALSO AVOID THIS SORT OF RECODING IN LINE, IF POSSIBLE
use.data$arab_coop = as.numeric(as.character(use.data$arab_coop))
use.data$arab_reject_binary = as.numeric(as.character(use.data$arab_reject_binary))
use.data$age = as.numeric(as.character(use.data$age))
use.data$foreign_born = as.numeric(as.character(use.data$foreign_born))
use.data$sex = as.numeric(as.character(use.data$sex))
use.data$left_right = as.numeric(as.character(use.data$left_right))

amelia_fit <- amelia(use.data, m=10, 
                     idvars = 'arab_reject_binary', ##set as ID var because don't want to impute
                     noms = c('religiosity','education','income','ethnicity'))

summary(amelia_fit)

outzelig.arab.coop.binary.imputed <- zelig(arab_coop ~ arab_reject_binary,
                                               model = 'logit',
                                               data=amelia_fit)
summary(outzelig.arab.coop.binary.imputed)

outzelig.arab.coop.binary.covariates.imputed <- zelig(arab_coop ~ arab_reject_binary + age + foreign_born + sex + 
                            left_right + religiosity  + education + income + ethnicity,
                          model = 'logit',
                          data=amelia_fit)
summary(outzelig.arab.coop.binary.covariates.imputed)

use.data1 = data.analysis[,c("arab_coop","arab_reject_binary","trust.b","age","foreign_born","sex",
                            "left_right","religiosity","education","income","ethnicity")]
use.data1$arab_coop = as.numeric(as.character(use.data1$arab_coop))
use.data1$arab_reject_binary = as.numeric(as.character(use.data1$arab_reject_binary))
use.data1$trust.b = as.numeric(as.character(use.data1$trust.b))
use.data1$age = as.numeric(as.character(use.data1$age))
use.data1$foreign_born = as.numeric(as.character(use.data1$foreign_born))
use.data1$sex = as.numeric(as.character(use.data1$sex))
use.data1$left_right = as.numeric(as.character(use.data1$left_right))

amelia_fit1 <- amelia(use.data1, m=10, 
                     idvars = 'arab_reject_binary', ##set as ID var because don't want to impute
                     noms = c('religiosity','education','income','ethnicity'))

summary(amelia_fit1)

outzelig.arab.coop.trust.binary.imputed <- zelig(arab_coop ~ arab_reject_binary + trust.b,
                      model = 'logit',
                      data=amelia_fit1)
summary(outzelig.arab.coop.trust.binary.imputed)

outzelig.arab.coop.trust.binary.covariates.imputed <- zelig(arab_coop ~ arab_reject_binary + trust.b + age + 
                        foreign_born + sex + left_right + religiosity + education + income + ethnicity,
                      model = 'logit',
                      data=amelia_fit1)
summary(outzelig.arab.coop.trust.binary.covariates.imputed)

######Table A4######
arab.coop.glm.distance.cont <- glm(arab_coop ~ as.factor(arab_accept_lin), 
                                   data=data.analysis, family=binomial)
summary(arab.coop.glm.distance.cont) 

arab.coop.glm.distance.covariate.cont <- glm(arab_coop ~ as.factor(arab_accept_lin) + age + 
                                               foreign_born + sex + left_right + 
                                               as.factor(religiosity) + as.factor(education) + 
                                               as.factor(income) + as.factor(ethnicity),
                                             data=data.analysis, family=binomial)
summary(arab.coop.glm.distance.covariate.cont) 

arab.coop.trust.cont <- glm(arab_coop ~ as.factor(arab_accept_lin) + as.factor(trust.3), 
                            data=data.analysis, family=binomial)
summary(arab.coop.trust.cont) 

arab.coop.trust.covariates.cont <- glm(arab_coop ~ as.factor(arab_accept_lin) + as.factor(trust.3) +
                                         age + foreign_born + sex + left_right +
                                         as.factor(religiosity) + as.factor(education) + 
                                         as.factor(income) + as.factor(ethnicity), 
                                       data=data.analysis, family=binomial)
summary(arab.coop.trust.covariates.cont) 

stargazer(arab.coop.glm.distance.cont,arab.coop.glm.distance.covariate.cont,
          arab.coop.trust.cont,arab.coop.trust.covariates.cont,
          no.space=T,  covariate.labels=c("Social distance: Neighbor", "Social distance: Co-worker", "Socail distance: Citizen", 
                                          "Social distance: Visitor", "Social distance: None", "Age", "Foreign Born", "Sex", "Ideology", 
                                          "Secular", "Traditional", "Ultra-Orthodox", "High-school", "Primary school", "Undergrad",  
                                          "High", "Low", "Very high", "Very low", "Mixed", "Other","Sephardic", 
                                          "Trust: Little", "Trust: None", 
                                          "Constant"))

#####Table A5#####
use.data2 = data.analysis[,c("arab_coop","arab_accept_lin","age","foreign_born","sex",
                            "left_right","religiosity","education","income","ethnicity")]
use.data2$age = as.numeric(as.character(use.data2$age))
use.data2$foreign_born = as.numeric(as.character(use.data2$foreign_born))
use.data2$sex = as.numeric(as.character(use.data2$sex))
use.data2$left_right = as.numeric(as.character(use.data2$left_right))

amelia_fit2 <- amelia(use.data2, m=10, 
                     idvars = 'arab_coop', ##set as ID var because don't want to impute
                     noms = c('arab_accept_lin','religiosity','education','income','ethnicity'))

summary(amelia_fit2)

outzelig.arab.coop.cont.imputed <- zelig(arab_coop ~ arab_accept_lin,
                          model = 'logit',
                          data=amelia_fit2)
summary(outzelig.arab.coop.cont.imputed) 

outzelig.arab.coop.cont.covariates.imputed <- zelig(arab_coop ~ arab_accept_lin + age + 
                            foreign_born + sex + left_right + 
                            religiosity + education + income + ethnicity,
                          model = 'logit',
                          data=amelia_fit2)
summary(outzelig.arab.coop.cont.covariates.imputed) 

use.data3 = data.analysis[,c("arab_coop","arab_accept_lin","trust.3","age","foreign_born","sex",
                            "left_right","religiosity","education","income","ethnicity")]
use.data3$arab_coop = as.numeric(as.character(use.data3$arab_coop))
use.data3$age = as.numeric(as.character(use.data3$age))
use.data3$foreign_born = as.numeric(as.character(use.data3$foreign_born))
use.data3$sex = as.numeric(as.character(use.data3$sex))
use.data3$left_right = as.numeric(as.character(use.data3$left_right))

amelia_fit3 <- amelia(use.data3, m=10, 
                     idvars = 'arab_coop', ##set as ID var because don't want to impute
                     noms = c('arab_accept_lin','trust.3','religiosity','education','income','ethnicity'))

summary(amelia_fit3)

outzelig.arab.coop.trust.cont.imputed <- zelig(arab_coop ~ arab_accept_lin + trust.3,
                                                          model = 'logit',
                                                          data=amelia_fit3)
summary(outzelig.arab.coop.trust.cont.imputed)

outzelig.arab.coop.trust.cont.covariates.imputed <- zelig(arab_coop ~ arab_accept_lin + trust.3 + age + 
                        foreign_born + sex + left_right + 
                        religiosity + education + income + ethnicity,
                      model = 'logit',
                      data=amelia_fit3)
summary(outzelig.arab.coop.trust.cont.covariates.imputed)

####Table A6###### 
arab.coop.glm <- glm(arab_coop ~ arab_reject_binary, 
                     data=data.analysis, family=binomial)
summary(arab.coop.glm) 

arab.coop.glm.interaction <- glm(arab_coop ~ arab_reject_binary*arab_interactions_numeric, 
                                 data=data.analysis, family=binomial)
summary(arab.coop.glm.interaction) 

arab.coop.glm.interactions <- glm(arab_coop ~ arab_reject_binary*arab_interactions_numeric +
                                    age + foreign_born + sex + left_right + 
                                    as.factor(religiosity) + as.factor(education) + as.factor(income) + as.factor(ethnicity), 
                                  data=data.analysis, family=binomial)
summary(arab.coop.glm.interactions) 

stargazer(arab.coop.glm,arab.coop.glm.interaction,arab.coop.glm.interactions, no.space=T,
          covariate.labels=c("Social Distance (binary)", "Interactions", 
                             "Age", "Foreign Born", "Sex", "Left-right", 
                             "Secular", "Traditional", "Ultra Orthodox", "High-school", "Primary school", "Undergrad",  
                             "High", "Low", "Very high", "Very low",
                             "Mixed", "Other","Sephardic", "Distance:Interactions", "Constant"))

#######Table A7###### 
arab.coop.glm.distance.share <- glm(arab_coop ~ arab_reject_binary + nonjewish_pcnt, 
                              data=data.analysis, family=binomial)
summary(arab.coop.glm.distance.share) 

arab.coop.glm.distance.covariates.share <- glm(arab_coop ~ arab_reject_binary + age + 
                                           foreign_born + sex + left_right + 
                                           as.factor(religiosity) + as.factor(education) + as.factor(income) + as.factor(ethnicity) + nonjewish_pcnt,
                                         data=data.analysis, family=binomial)
summary(arab.coop.glm.distance.covariates.share) 

arab.coop.glm.distance.seg <- glm(arab_coop ~ arab_reject_binary + dis_arabs, 
                                    data=data.analysis, family=binomial)
summary(arab.coop.glm.distance.seg) 

arab.coop.glm.distance.covariates.seg <- glm(arab_coop ~ arab_reject_binary + age + foreign_born + sex + left_right + 
                                                 as.factor(religiosity) + as.factor(education) + as.factor(income) + as.factor(ethnicity) + dis_arabs,
                                               data=data.analysis, family=binomial)
summary(arab.coop.glm.distance.covariates.seg) 

arab.coop.glm.distance.covariates.share.seg <- glm(arab_coop ~ arab_reject_binary + age + foreign_born + sex + left_right + 
                                               as.factor(religiosity) + as.factor(education) + as.factor(income) + as.factor(ethnicity) + 
                                                 dis_arabs*nonjewish_pcnt,
                                             data=data.analysis, family=binomial)
summary(arab.coop.glm.distance.covariates.share.seg) 

stargazer(arab.coop.glm.distance.share, arab.coop.glm.distance.covariates.share,
          arab.coop.glm.distance.seg, arab.coop.glm.distance.covariates.seg,
          arab.coop.glm.distance.covariates.share.seg,
          no.space=T,  covariate.labels=c("Social Distance (binary)", "Age", "Foreign Born", "Sex", "Left-right", 
                                          "Secular", "Traditional", "Ultra Orthodox", "High-school", "Primary school", "Undergrad",  
                                          "High", "Low", "Very high", "Very low", "Mixed", "Other","Sephardic", "Non-Jewish percentage",
                                          "Non-Jewish segregation*Non-Jewish percentage", "Non-Jewish Segregation", 
                                          "Constant"))

######Table A8######
secular.coop <- glm(secular_coop ~ arab_reject_binary, 
                                         data=data.secular, family=binomial)
summary(secular.coop) 

uo.coop <- glm(uo_coop ~ arab_reject_binary, 
                                            data=data.uo, family=binomial)
summary(uo.coop) 

secular.coop.covariates <- glm(secular_coop ~ arab_reject_binary + age + 
                                           foreign_born + sex + left_right + 
                                           as.factor(education) + as.factor(income) + as.factor(ethnicity), 
                                         data=data.secular, family=binomial)
summary(secular.coop.covariates) 

uo.coop.covariates <- glm(uo_coop ~ arab_reject_binary + age +foreign_born + sex + left_right + 
                                              as.factor(education) + as.factor(income) + as.factor(ethnicity), 
                                            data=data.uo, family=binomial)
summary(uo.coop.covariates)

stargazer(secular.coop, secular.coop.covariates, uo.coop, uo.coop.covariates, no.space=T,
          covariate.labels=c("Social Distance from Arabs (binary)", "Age", "Foreign Born", "Sex", "Left-right", 
                             "High-school", "Primary school", "Undergrad",  
                             "High", "Low", "Very high", "Very low",
                             "Mixed", "Other","Sephardic", "Constant"))

#######Table A9########
arab.coop.glm.distance.covariates.share.interacted <- glm(arab_coop ~ arab_reject_binary*nonjewish_pcnt + age + 
                                                            foreign_born + sex + left_right + 
                                                            as.factor(religiosity) + as.factor(education) + as.factor(income) + as.factor(ethnicity),
                                                          data=data.analysis, family=binomial)
summary(arab.coop.glm.distance.covariates.share.interacted) 

arab.coop.glm.distance.covariates.seg.interacted <- glm(arab_coop ~ arab_reject_binary*dis_arabs + age + 
                                                          foreign_born + sex + left_right + 
                                                          as.factor(religiosity) + as.factor(education) + as.factor(income) + as.factor(ethnicity),
                                                        data=data.analysis, family=binomial)
summary(arab.coop.glm.distance.covariates.seg.interacted) 

stargazer(arab.coop.glm.distance.covariates.share.interacted,arab.coop.glm.distance.covariates.seg.interacted,
          no.space=T,  covariate.labels=c("Social Distance (binary)",
                                          "Non-Jewish percentage", "Non-Jewish Segregation", 
                                          "Age", "Foreign Born", "Sex", "Ideology", 
                                          "Secular", "Traditional", "Ultra Orthodox", "High-school", "Primary school", "Undergrad",  
                                          "High", "Low", "Very high", "Very low", "Mixed", "Other","Sephardic", 
                                          "Social Distance*Non-Jewish percentage","Social Distance*Non-Jewish segregation",  
                                          "Constant"))