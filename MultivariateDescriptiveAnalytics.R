# Set working directory
setwd("---------")

# Load dataset
load("Cars2015.rda")

# Libraries
library(ggplot2)
library(Hmisc)
library(corrplot)
library(corrgram)
library(ppcor)
library(nortest)
library(tseries)
library(moments)
library(MASS)
library(car)
library(MVN)
library(GGally)

# ------------------------------------------------------------------------------------------------
# a) Choose a quantitative variable and explore its distribution in terms of descriptive measures of center, 
# dispersion, skewness and kurtosis. Is a normal model a plausible one for its distribution? 
# If the answer is no, can you think of a transformation of the variable that improves normality. 
# Are there any outliers?

# The two most important measures of center are the mean and the median. When it comes to dispersion measures, which describe
# how the values of a variable are distributed, the most significant ones are the variance, the standard deviation, and the interquartile range.
# The graph which shows most of these atributtes is the boxplot, in which we have added the mean as a red dot.


# We choose CityMPG: City miles per gallon

str(Cars2015)
summary(Cars2015$CityMPG)

# Dispersion: dispersion (also called variability, scatter, or spread) is the extent to which a distribution is 
# stretched or squeezed. Common examples of measures of statistical dispersion are the variance, standard deviation, 
# and interquartile range.

boxplot(Cars2015$CityMPG)
hist(Cars2015$CityMPG)
boxplot <- ggplot(data = Cars2015, aes(x = "", y=CityMPG)) + 
  geom_boxplot() + geom_jitter() + 
  stat_summary(fun.y="mean", geom="point", shape=20, size=5, color="red", fill="red") +
  ggtitle("CityMPG Boxplot")
  

var <-  var(Cars2015$CityMPG)
std <-  sqrt(var)
iqr <-  IQR(Cars2015$CityMPG)
mean <- mean(Cars2015$CityMPG)
median <- median(Cars2015$CityMPG)

# Anscombe: kurtosis test. In a normal distrib. kurtosis = 3
anscombe.test(Cars2015$CityMPG)  
# In this case, p-value>0.005 --> kurtosis equal to 3 (The one which a normal distribution has)

# Agostino: skewness test. 
agostino.test(Cars2015$CityMPG)
# p-value < 0.005 --> data has skewness

# There is skewness, so it can not be a normal distribution. To ensure it, we apply the 
# Jarque-bera test.

jarque.bera.test(Cars2015$CityMPG)

#The p-value < 0.005 : There is not a normal distribution

# qqPlot is designed to give us the information regarding tests on normality.
# The line depicted in qqPlot has to do with the mean and standard deviation of the data

qqPlot(Cars2015$CityMPG, dist="norm")

# Power Transformations, Box-Cox transformation to improve normality, linear relationship between two variables,
# and/or constant variance.

summary(powerTransform(Cars2015$CityMPG))

# The estimated parameter for the transformation is λ=-0.4668
# Log transformation also possible λ=0

# We apply the transformation

cityMPGt=bcPower(Cars2015$CityMPG, lambda=-0.4668)
cityMPGlog=bcPower(Cars2015$CityMPG, lambda=0)

#Graphically the origial and the 2 transformations applied

par(mfrow=c(1,3))
qqPlot(Cars2015$CityMPG, dist="norm")
qqPlot(cityMPGt, dist="norm")
qqPlot(cityMPGlog, dist="norm")
par(mfrow=c(1,1))

#Cheking improvement of normality
jarque.bera.test(cityMPGt)
jarque.bera.test(cityMPGlog)

# Now we can say that the distribution is normal

# Agostino: skewness test. 
agostino.test(cityMPGt)
# p-value > 0.005 --> data has no skewness

# ------------------------------------------------------------------------------------------------
# b) Choose two quantitative variables and describe its joint bivariate distribution. 
# Does it seem to be Normal? Are there any outliers?

# We choose weight

histogram(Cars2015$Weight)

# Bivariate Normality for the joint variable (rdoorcl$V1,rdoorop$V1)
# Estimating bivariate parameter (\lambda_1,\lambda_2)
powerTransform(cbind(Cars2015$CityMPG,Cars2015$Weight))
summary(powerTransform(cbind(Cars2015$CityMPG,Cars2015$Weight)~1))

# Transformations to Multinormality 
# Although We can accept the logarithmic transformation for both variables, 
# we are going to transform them with (\lambda_1,\lambda_2) values =c(-0.0757, -0.2821).
# Defining the transformed variable with those lambdas
bivariateT=bcPower(cbind(Cars2015$CityMPG,Cars2015$Weight), c(-0.0757, -0.2821))
bivariateTlog=bcPower(cbind(Cars2015$CityMPG,Cars2015$Weight), c(0, 0))

#Bivariate Normality Before the transformation
mvn(cbind(Cars2015$CityMPG,Cars2015$Weight), mvnTest="mardia", multivariatePlot="qq")
#Bivariate Normality After the transformation, lambda option
mvn(bivariateT, mvnTest="mardia", multivariatePlot="qq")
#Bivariate Normality After the transformation, log option
mvn(bivariateTlog, mvnTest="mardia", multivariatePlot="qq")

# ------------------------------------------------------------------------------------------------
# c) Choose a subset of 4 or 5 quantitative variables and explore linear relationships through:

# R matrix of pairwise correlations
# Matrix of partial correlations
# Coefficient of determination (function r2multv() we define in R)
# The determinant of R (correlation matrix) as an overall measure of linear relationships.
# An eigenanalysis of matrix R, looking for really small eigenvalues.

# R matrix of pairwise correlations

# We choose CityMPG, Weight, Length, Width and Height.

my_data <- Cars2015[, c(7,10,11,13,15)]

# R matrix of pairwise correlations

library(corrplot)
corrplot(cor(my_data))
corrplot.mixed(cor(my_data))

# Strong positive correlation between weight and width: 0.91
# Strong negative correlation between CityMPG and Weight: -0.83

#Partial correlations, package ppcor. Detailed output

library(ppcor)
pcor(my_data)
matrix.partial=pcor(my_data)$estimate

#Visualizing partial correlations
corrgram(matrix.partial,
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

corrplot.mixed(matrix.partial,order="AOE" )

# Coefficient of determination (function r2multv() we define in R)

r2multv<-function(x){
  r2s=1-1/(diag(solve(cov(x)))*diag(cov(x)))
  r2s
}

r2multv(my_data)
#Weight is the variable most likely to be predicted by the other 4.


# The determinant of R (correlation matrix) as an overall measure of linear relationships.

det(my_data)
1-det(cor(my_data))^{1/6}

# Altogether, linear dependences explain 57% of the variability of this 5 variables.

# An eigenanalysis of matrix R, looking for really small eigenvalues.

eigen(cor(my_data))

# There is a very small Eigenvalue: 0.057

