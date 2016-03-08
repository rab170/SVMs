#!/usr/bin/env Rscript

# Robert A Brown
# University of Rhode Island
# Homework # 03

source('solve-QP.r')

perceptron <- function(X, Y, eta) {
	n <- nrow(X)
	m <- ncol(X)
	
	b <- 0 
	w <- vector(mode="numeric", length=m)
	r <- max(apply(X, 1, function(x) sqrt(sum(x^2))))

	while(TRUE) {
		all_classified = TRUE
		for(i in 1:n) {
			x <- X[i,]
			if (sign(sum(w*x) - b) != Y[i]) {
				w <- w + eta*Y[i]*x
				b <- b - eta*Y[i]*r^2
				all_classified = FALSE
			}
		}
		if (all_classified)
			break
	}
	f <- function(x) { sign(sum(w*x) - b) }
	list("f"=f, "w"=w, "b"=b)
}

simple <- function(X, Y) {
	n <- nrow(X)
	m <- ncol(X)

	L.plus <- X[Y == 1,]
	L.minus <- X[Y == -1,]
	c.plus <- 1/nrow(L.plus) * apply(L.plus, 2, sum)
	c.minus <- 1/nrow(L.minus) * apply(L.minus, 2, sum)

	d <- c.plus - c.minus
	c <- 1/2*(c.plus + c.minus)
	
	f <- function(x) sign(sum((x - c)*d))
	list("f"=f, "c"=c, "d"=d)
}

maximum_margin <- function(X, Y, q) {
		
	r <- max(apply(X, 1, function(x) sqrt(sum(x^2))))
	X <- t(as.matrix(X)*Y)

	n <- nrow(X)
	m <- ncol(X)

	w_optimal <- vector(mode='numeric', length=n) 
	b_optimal <- vector(mode='numeric', length=n) 
	
	for (b in seq(-q, q)) {
		c <- 1 - Y*b
		I <- diag(n)
		zero <- x <- vector(mode="numeric", length=n)
		QP <- my.solve.QP(I, zero, X, c)
		w <- QP$solution
		if (QP$status == 0 && (all(w_optimal == 0) || norm(as.matrix(w)) < norm(as.matrix(w_optimal)))) {
			w_optimal <- w
			b_optimal <- b
		}
	}

	if (all(w_optimal == 0)) stop('no solution found')
	if (norm(as.matrix(w_optimal)) > q/r) stop('constraint violated') 

	f <- function(x) { sign(sum(w_optimal*x) - b_optimal) }
	list("f"=f, "w"=w_optimal, "b"=b_optimal)
}

plot_decision_boundry <- function(x, w, b, color) {
	m <- -1*w[1]/w[2]
	y <- sapply(x, function(x) m*x+b/w[2])
	lines(x, y, col=color)
}


df <- read.csv('data/training.csv')
n <- nrow(df)
m <- ncol(df)

X <- df[,1:(m-1)]
Y <- df[,m]
eta = .01

perceptron.model <- perceptron(X, Y, eta)
simple.model <- simple(X, Y)
max.margin <- maximum_margin(X, Y, 1000)
print(max.margin)

print('perceptron model training checks')
for (i in 1:n) {
	Y.expected = Y[i]
	Y.model = perceptron.model$f(X[i,])
	print(Y.model)
	print(Y.model == Y.expected)
}

print('simple model training checks')
for (i in 1:n) {
	Y.expected = Y[i]
	Y.model = simple.model$f(X[i,])
	print(Y.model)
	print(Y.model == Y.expected)
}

print('maximum margin classifier')
for (i in 1:n) {
	Y.expected = Y[i]
	Y.model = max.margin$f(X[i,])
	print(Y.model)
	print(Y.model == Y.expected)
}

png(filename="6_2.png")
plot(X, xlim=c(0,5), ylim=c(0,5), pch=ifelse(Y==-1, "-", "+"), cex=2)
plot_decision_boundry(X[,1], perceptron.model$w, perceptron.model$b, "blue")
plot_decision_boundry(X[,1], simple.model$d, sum(simple.model$d*simple.model$c), "red")
plot_decision_boundry(X[,1], max.margin$w, max.margin$b, "green")
legend('topright', c("perceptron", "simple", "maximum margin"), col=c("blue", "red", "green"), lty=1, bty='n', cex=.75)
title(main="problem 6.2")

x <- t(c(2, 2))
simple.pred <- simple.model$f(x)
perceptron.pred <- perceptron.model$f(x)
margin.pred <- max.margin$f(x)
print('perceptron prediction:')
print(perceptron.pred)
print('simple prediction:')
print(simple.pred)
print('maximum margin prediction:')
print(margin.pred)
points(x, col='black')
