# ==================================
# Lesson - logistic regression
# 
# Melinda Higgins, PhD
# updated 10/27/2021
# ==================================

# ==================================
# we're be working with the 
# helpmkh dataset
# ==================================

library(tidyverse)
load("help_set1_wide.RData")
helpdata <- helpdat.set1

# ============================================.
# For this lesson we'll use the helpmkh dataset
#
# Let's focus on homeless as the main outcome variable
# which is dichotomous coded 0 and 1. We'll use
# logistic regression to look at predicting whether someone
# was homeless or not using these variables
# female, pss_fr, pcs, and indtot
# ============================================.

h1 <- helpdata %>%
  dplyr::select(homeless, female, pcs, pss_fr, indtot)

# ============================================.
# let's look at the correlations between these variables
# ============================================;

# look at the correlation matrix
library(psych)
psych::corr.test(h1, method="pearson")

# add homeless as a factor
h1$homeless.f <- factor(h1$homeless,
                        levels = c(0,1),
                        labels = c("no","yes"))

# drop homeless as integer from h1
h1 <- h1[, -1]

# another fun package for making
# scatterplot matrix figures
# useful for visualizing correlations
library(GGally)
ggpairs(h1)

# and by group
# see the correlations by group in plot
ggpairs(h1, aes(color = homeless.f))

# ============================================.
# Given the correlation between pss_fr
# and homeless, let's run a t-test to see the comparison
# ============================================;

# Bartlett Test of Homogeneity of Variances
bartlett.test(pss_fr~homeless.f, data=h1)

# t-tests, unequal variance and then equal variance
t.test(pss_fr ~ homeless.f, h1)
t.test(pss_fr ~ homeless.f, h1,
       var.equal=TRUE)

# ============================================.
# Let's run a logistic regression of pss_fr to predict
# the probability of being homeless
# we'll also SAVE the predicted probabilities
# and the predicted group membership
#
# let's look at different thresholds pprob
# ctable gives us the classification table
#
# use the plots=roc to get the ROC curve
# ============================================;

glm1 <- glm(homeless.f ~ pss_fr, 
            data=h1,
            family=binomial)

glm1
summary(glm1)

# raw coefficients
coef(glm1)

# compute odds ratios = exp(coefficients)
or1 <- exp(coef(glm1))
or1

# and get 95% confidence intervals for odds ratios
or1.ci <- exp(confint(glm1))
or1.ci

# put together
data.frame(or1, or1.ci)

# add better column names
or1.df <- data.frame(or1, or1.ci)
names(or1.df) <- c("odds ratio", "95% CI LB", "95% CI UB")
or1.df

# get predicted probability of
# being homeless
glm1.predict <- predict(glm1, newdata=h1,
                        type="response")

# plot probability of being homeless
# by the continuous pss_fr predictor
plot(h1$pss_fr, glm1.predict)
abline(0.5, 0, col = "red")

# can also make plot using ggplot2
h1.predict1 <- cbind(h1, glm1.predict)
library(ggplot2)
ggplot(h1.predict1, aes(pss_fr, glm1.predict)) +
  geom_point() +
  geom_smooth(color = "blue") +
  geom_hline(yintercept = 0.5, color = "red") 

# we can also look at predicted probabilities
# by the original homeless NO, YES groups
ggplot(h1.predict1, aes(homeless.f, glm1.predict)) +
  geom_boxplot() +
  geom_hline(yintercept = 0.5, color = "red")

# look at how well or poorly the
# model does predicting homelessness
# with different cutoff probabilities

#confusion matrix using cutpoint of 0.5
table(h1$homeless.f, glm1.predict > 0.5)

#confusion matrix using cutpoint of 0.4
table(h1$homeless.f, glm1.predict > 0.4)

# OPTIONAL - try the caret package
library(caret)

# save table output
# list predicted first, then original values
tab1 <- table(glm1.predict > 0.5, 
              h1$homeless.f == "yes")
caret::confusionMatrix(tab1)

# compare to using online calculator
# for 2-x-2 table of confusion matrix
# https://statpages.info/ctab2x2.html

# Also compute the AUC and
# plot an ROC
library(ROCR)
p <- predict(glm1, newdata=h1, 
             type="response")
pr <- prediction(p, as.numeric(h1$homeless.f))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(0, 1, col = "red")

# compute AUC, area under the curve
# also called the C-statistic
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# ideally you want AUC as close to 1 as possible
# AUC > 0.9 is great
# AUC > 0.8 is good
# AUC > 0.7 is ok
# AUC < 0.7 are not very good
# AUC around 0.5 is no better than flipping a fair coin
# which is a useless model

# also - add title to plot with AUC in title
plot(prf,
     main = paste("ROC Curve, AUC = ", round(auc, 3)))
abline(0, 1, col="red")

# try again for indtot as a predictor
# compare to pss_fr as a predictor

# ============================================.
# Given the stronger correlation between indtot
# and homeless, let's run a t-test to see the comparison
# ============================================;

# Bartlett Test of Homogeneity of Variances
bartlett.test(indtot~homeless.f, data=h1)

# t-tests, unequal variance and then equal variance
t.test(indtot ~ homeless.f, h1)
t.test(indtot ~ homeless.f, h1,
       var.equal=TRUE)

# ============================================.
# Let's run a logistic regression of indtot to predict
# the probability of being homeless
# we'll also SAVE the predicted probabilities
# and the predicted group membership
#
# let's look at different thresholds pprob
# ctable gives us the classification table
#
# use the plots=roc to get the ROC curve
# ============================================;

glm2 <- glm(homeless.f ~ indtot, data=h1,
          family=binomial)

glm2
summary(glm2)
coef(glm2)
exp(coef(glm2))

# UPDATE - get 95% confidence intervals for odds ratios
exp(confint(glm2))

glm2.predict <- predict(glm2, newdata=h1,
                        type="response")

plot(h1$indtot, glm2.predict)

#confusion matrix
table(h1$homeless.f, glm2.predict > 0.5)

# UPDATE - look at %'s of total, set
# prop.r and prop.c to FALSE

library(gmodels)
CrossTable(h1$homeless.f, glm2.predict > 0.5,
           prop.r = FALSE,
           prop.c = FALSE,
           prop.t = TRUE)

# see https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

# make an ROC curve

library(ROCR)
p <- predict(glm2, newdata=h1, 
             type="response")
pr <- prediction(p, as.numeric(h1$homeless.f))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
# UPDATE - add abline diagonal reference line in RED
abline(0, 1, col="red")

auc2 <- performance(pr, measure = "auc")
auc2 <- auc2@y.values[[1]]
auc2

# UPDATE - add title to plot with AUC in title
plot(prf,
     main = paste("ROC Curve, AUC = ", round(auc2, 3)))
abline(0, 1, col="red")

# compare 2 models: glm1 (pss_fr) and glm2 (indtot)
# lower AIC and BIC is better
# and higher AUC is better
AIC(glm1, glm2)
BIC(glm1, glm2)
c(auc, auc2)

# ======================================
# make another model with 4 predictors entered together

glm3 <- glm(homeless.f ~ female + pss_fr + pcs + indtot, 
            data=h1, family=binomial)

glm3
summary(glm3)
coef(glm3)
exp(coef(glm3))
exp(confint(glm3))

# put together
data.frame(exp(coef(glm3)),
           exp(confint(glm3)))

glm3.predict <- predict(glm3, newdata=h1,
                        type="response")

gmodels::CrossTable(h1$homeless.f, glm3.predict > 0.5,
                    prop.r = FALSE,
                    prop.c = FALSE,
                    prop.t = TRUE)

# see https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

# make an ROC curve for model m3
# library(ROCR)
p <- predict(glm3, newdata=h1, 
             type="response")
pr <- prediction(p, as.numeric(h1$homeless.f))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(0, 1, col="red")

auc3 <- performance(pr, measure = "auc")
auc3 <- auc3@y.values[[1]]
auc3

# add title to plot with AUC in title
plot(prf,
     main = paste("ROC Curve, AUC = ", round(auc3, 3)))
abline(0, 1, col="red")

# compare 3 models: 
# glm1 with pss_fr 
# glm2 with indtot
# glm3 with female + pss_fr + pcs + indtot
# lower AIC and BIC is better
# and higher AUC is better
AIC(glm1, glm2, glm3)
BIC(glm1, glm2, glm3)
c(auc, auc2, auc3)

# for nested models we can compare
# glm3 and glm2
# or glm3 and glm1
anova(glm2, glm3, test = "Chisq")
anova(glm1, glm3, test = "Chisq")

# note - test options are
# “Rao”, “LRT”, “Chisq”, “F”, “Cp”
# see help("anova.glm")

# ======================================
# added 10/28/2020
# see sjplot package
# https://strengejacke.github.io/sjPlot/articles/plot_model_estimates.html

library(sjPlot)
sjPlot::plot_model(glm3, vline.color = "red")

# notice that scale is a log scale on X-axis
# see https://andrewpwheeler.com/2013/10/26/odds-ratios-need-to-be-graphed-on-log-scales/

# other articles to explore - packages for presenting model summaries:
# https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html
# https://www.r-bloggers.com/2018/05/elegant-regression-results-tables-and-plots-in-r-the-finalfit-package/
# http://www.danieldsjoberg.com/gtsummary/articles/gallery.html
