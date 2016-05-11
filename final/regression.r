library(e1071)
library(boot)


rmse <- function(sim, obs) {
    sqrt(mean((sim - obs)^2))
}

linear.boot.camp <- function(D, i) {
    boot.sample <- D[i,]
    x <- boot.sample[!exclude]
    y <- boot.sample['MEDV']
    model <- svm(x, y, cost=125)
    res <- predict(model, x)
    err <- rmse(res, y)
    return(err)
}

poly.boot.camp <- function(D, i) {
    boot.sample <- D[i,]
    x <- boot.sample[!exclude]
    y <- boot.sample['MEDV']
    model <- svm(x, y, kernel='polynomial', degree=5, cost=1.6, coef0=7)
    res <- predict(model, x)
    err <- rmse(res, y)
    return(err)
}

rmse.list = c()
housing.data <- read.csv('housing.txt', header=TRUE)
exclude <- names(housing.data) %in% c('MEDV', 'CHAS', 'AGE', 'DIS', 'RAD', 'TAX', 'PTRATIO')

linear <- boot(housing.data, linear.boot.camp, R = 200)
linear.ci <- boot.ci(linear, type = c("basic"))
print(linear.ci)

poly <- boot(housing.data, poly.boot.camp, R = 200)
poly.ci <- boot.ci(poly, type = c("basic"))
print(poly.ci)
save.image()
