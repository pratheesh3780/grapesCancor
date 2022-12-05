
###CANONICAL CORRELATION

require(ggplot2)
require(GGally)
require(CCA)
require(CCP)

attach(mmreg)


# intercorreclations within variable sets
psych <- mmreg[, 1:3]
acad <- mmreg[, 4:8]

GGally::ggpairs(psych)
GGally::ggpairs(acad)


# correlations using the matcor function from the CCA package
matcor(psych, acad)

# canonical correlation
cc1 <- cc(psych, acad)

# display the canonical correlations
cc1$cor

# raw canonical coefficients
cc1[3:4]
#The raw canonical coefficients are interpreted in a manner 
#analogous to interpreting regression coefficients i.e., 
#for the variable read, a one unit increase in reading leads 
#to a .0446 decrease in the first canonical variate of set 2 
#when all of the other variables are held constant. 
#Here is another example: being female leads to a .6321 decrease
#in the dimension 1 for the academic set with the other 
#predictors held constant.

# compute canonical loadings
cc2 <- comput(psych, acad, cc1)

# display canonical loadings
#Canonical loadings, also called structure coefficients, 
#measure the simple linear correlation between an 
#original observed variable in the u- or v-variable set and 
#that set's canonical variate.
cc2[3:6]

# tests of canonical dimensions
rho <- cc1$cor

## Define number of observations, number of variables in
#first set, and number of variables in the second set.
n <- dim(psych)[1]
p <- length(psych)
q <- length(acad)

# Calculate p-values using the F-approximations of 
# different test statistics:
library(CCP)
p.asym(rho, n, p, q, tstat = "Wilks")

p.asym(rho, n, p, q, tstat = "Hotelling")

p.asym(rho, n, p, q, tstat = "Pillai")

p.asym(rho, n, p, q, tstat = "Roy")


# standardized psych canonical coefficients diagonal matrix of 
# psych sd's

s1 <- diag(sqrt(diag(cov(psych))))
s1 %*% cc1$xcoef

# standardized acad canonical coefficients diagonal matrix of 
#acad sd's

s2 <- diag(sqrt(diag(cov(acad))))
s2 %*% cc1$ycoef




####ordered bar plot
num <- c(1, 8, 4, 3, 6, 7, 5, 2, 11, 3)
cat <- c(letters[1:10])
data <- data.frame(num, cat)
data2  <- data[order(data[,1],decreasing=TRUE),]
barplot(data2[,1],names.arg=data2[,2])


####ordered bar plot of raw coefficients
####ordered bar plot of standard coefficients



