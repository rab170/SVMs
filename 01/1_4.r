#Robert A Brown
#CS 581 ~ Homework 01 
#University of Rhode Island

learner <- function(training.df) {
	if (!is.data.frame(training.df))
	   stop("not a data frame")

	n <- ncol(training.df)
	target.attribute <- training.df[[n]] 
	target.levels <- table(target.attribute) 
	ix <- which.max(target.levels) 
	majority.label <- names(target.levels[ix]) 

	function(x) majority.label
}


df <- read.csv('data/mammals.csv')
write("\nexpected value from model 'trained' on mammals.csv is 'false'", stdout())
write("actual:", stdout())
m <-learner(df)
m(df)

df  <- read.csv('data/biomed.csv')
write("\nexpected value from model 'trained' on biomed.csv is 'MI'", stdout())
write("actual:", stdout())
m <-learner(df)
m(df)
