#################
# Read the data #
#################

pitching <- read.csv('pitching.csv')[, -1]

# Create a vector with meaningless for analysis columns.
excluded.columns <- c("nameLast", 
                      "nameFirst",
                      "playerID", 
                      "teamID", 
                      "POS", 
                      "CurrentYearStatus",
                      "yearID"
                      )

# Remove these columns from the data
pitching <- pitching[ , !(names(pitching) %in% excluded.columns)]

# Split pitching data into train set (salaries in 2011-2015), and test set (salaries in 2016)
pitching.2016 <- pitching[which(pitching$salaryYear == 2016), ]
pitching.2011_15 <- pitching[which(pitching$salaryYear != 2016), ]

# Disable scientific notation
options(scipen=999)

#############################
# Exploratory data analysis #
#############################

# distribution of salaries by years
library(ggplot2)
dist <- ggplot(data=data.frame(year = factor(pitching$salaryYear), 
                       salary = pitching$salary),
       aes(year, salary)) + 
  geom_boxplot(outlier.colour = "red")

# salary trend by years
library(dplyr)
df1 <- pitching %>% group_by(salaryYear) %>% summarise(mean(salary))
trend <- ggplot(data=df1, aes(salaryYear, `mean(salary)`)) + 
  geom_line()

install.packages("gridExtra")
library(gridExtra)
grid.arrange(dist, trend, ncol=2) # plot the graphs

# independent 2-group t-test to see if true difference in means of test data 
# salaries and train data salaries is equal to 0. The p-value of the test is >0.05,
# which favors the null hypothesis that there is no difference in mean salaries
# between the train and test data.
t.test(pitching.2011_15$salary, pitching.2016$salary)

# We deal with high dimensional data. We will use PCA for dimensionality reduction and 
# to analyze what variables related to players' in-field performance influence their salaries 
# most

# exclude the response variable and salaryYear column
exclude <- c('salaryYear', 'salary')

pca = prcomp(pitching[ , !(names(pitching) %in% exclude)], 
             scale = TRUE, 
             center = TRUE
             )
pcaVar = pca$sdev^2
pve = pcaVar/sum(pcaVar)
plot(pve, xlab = "PC", ylab = "PVE", type = "b", ylim =c(0,1)) # there is an "elbow" on the graph
                                                               # at the 5th PC suggesting that first 
                                                               # five PCs would well explane variation 
                                                               # of the data
# The following code allows to visualize the biplot
install.packages('FactoMineR')
library(FactoMineR)
res.pca = PCA(pitching[ , !(names(pitching) %in% exclude)], 
              scale.unit=TRUE, 
              ncp=5, 
              graph=T
              )

# We used Factoshiny package to create interactive visualization of distribution, 
# or clustering, of observations as well as
# to vizualize what factors influence clustering of the data
install.packages('Factoshiny')
library(Factoshiny)
PCAshiny(res.pca)

#####################
# Predictive models #
#####################

# First model - linear regression with stepwise model selection

fit.regr = lm(salary ~ . -salaryYear, pitching.2011_15)
summary(fit.regr) # the model is overfit
# stepwise model selection
install.packages('wle')
library(wle)
null.model = lm(salary ~ 1, data = pitching.2011_15)
full.model = lm(salary ~ . -salaryYear, data = pitching.2011_15)
step(null.model, 
     scope = list(lower = null.model, 
                  upper = full.model), 
     direction = "both") $ call
# test selected model      
fit.regr.select = lm(formula = salary ~ SO + BB + E + SV + G + W + GS + HBP + WP + 
                       GIDP + IBB, data = pitching.2011_15)
summary(fit.regr.select)
# regression model diagnostics
plot(fit.regr.select$residuals) # check for correlation overtime
hist(fit.regr.select$residuals) # check for normality of errors
plot(fit.regr.select, which = 2) # check for normality of errors
library(car)
residualPlots(fit.regr.select) # check for heteroskedasticity
vif(fit.regr.select) # check for Multicollinearity, remove GS
fit.regr.select.1 = lm(formula = salary ~ SO + BB + SV + G + W + HBP + WP + 
                         GIDP + IBB, data = pitching.2011_15)
summary(fit.regr.select.1)
# the final regression model satisfies the standard model assumptions
vif(fit.regr.select.1)
plot(fit.regr.select.1$residuals)
hist(fit.regr.select.1$residuals)
plot(fit.regr.select.1, which = 2)
residualPlots(fit.regr.select.1)

# Visualize relationships between variables in the final model
df <- pitching.2011_15[, c('salary', 'SO', 'BB', 'E', 'SV', 'G', "W",
                           'HBP', 'WP', 'GIDP', 'IBB')]
plot(df)

# Check prediction accuracy of the linear regression model
pred.regr <- predict(fit.regr.select.1, pitching.2016)
sqrt(mean((pred.regr - pitching.2016$salary)^2)) # RMSE ~3,700,000

# plot predicted vs actual salaries
plot(pred.regr, pitching.2016$salary, 
     main = "Linear regression. Predicted vs actual salaries",
     xlab = "Predicted salaries",
     ylab = "Test data salaries",
     font.main = 1)
abline(0, 1, col = "blue")

# Now we will use principal component regression (PCR), another method suitable for high dimensional data.

install.packages('pls')
library(pls)
set.seed(121)
pcr.fit = pcr(salary ~ . - salaryYear, data = pitching.2011_15, scale=TRUE, validation ="CV")
# Plot root mean square error of prediction (RMSEP) vs number of components used in the model. 
validationplot(pcr.fit, val.type = "RMSEP") # It looks like the model including first 22 PC's  
                                            # will have the lowest RMSEP

# predict the salaries with the PCR model and check model accuracy
pcr.pred = predict (pcr.fit, pitching.2016, ncomp = 22)
sqrt(mean((pcr.pred - pitching.2016$salary)^2)) # model accuracy on the test data is ~$3,613,000

# Disadvantages of PCR: hard to interpret. 
# Advantages: slightly better accuracy of prediction, better fit.

# Now we use random forest, the method that decorrelates the trees by considering only limited, 
# random sample, of predictors at every split. It allows to decrease influence of a single predictor, 
# or a group of predictors, highly correlated with the response variable.

library(randomForest)
set.seed(121)
rf.pitching = randomForest(salary ~ . - salaryYear, 
                            data=pitching.2011_15, 
                            importance =TRUE
                            )
rf.pitching
# predict the salaries with random forest
yhat.rf = predict(rf.pitching, newdata = pitching.2016)
sqrt(mean((yhat.rf - pitching.2016$salary)^2)) # model accuracy on the test data is ~$3,990,000

# Plot factors according to their importance
importance(rf.pitching)
varImpPlot(rf.pitching)

# Disadvantages of random forest: lower accuracy and relatively low fit (63% of variance explained), 
# but this model is easier to interpret.

# Boosting is the last method we tried. It does not involve dimensionality reduction, but we wanted
# to give it a try to see if this model will outperform random forest. Boosting is a tree-like method.
# It growth sequentially a number of trees. Each tree is grown using information from previously
# grown trees, which improves model accuracy and fit.

install.packages('gbm')
library(gbm)
set.seed(121)
boost.pitching = gbm(salary ~ . - salaryYear, 
                     data=pitching.2011_15, 
                     distribution= "gaussian", 
                     n.trees = 10000, 
                     interaction.depth = 4
                     )
summary(boost.pitching)
# Plot four factors with the highest relative influence vs salary. Ignore IPouts as the data is similar
# to BFP. Age is close in influence and tells us more about the data
par(mfrow =c(2,2))
plot(boost.pitching,i="SO")
plot(boost.pitching,i="W")
plot(boost.pitching,i="BFP")
plot(boost.pitching,i="age")
yhat.boost = predict(boost.pitching, newdata = pitching.2016,
                    n.trees =10000)
sqrt(mean((yhat.boost - pitching.2016$salary)^2)) # prediction accuracy is ~$3,568,000,
                                                  # best among all model we've tried.

# Disadvantages of boosting: still low accuracy although better than acuracy of the three other 
# models, but this model is easier to interpret.
