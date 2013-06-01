library(ggplot2)
library(plyr)
library(reshape2)

base.plot <- function(data, x, y) {
  ggplot(data=data, aes_string(x=x)) +
    geom_text(aes_string(y=y, label="countrycode"), size=3) +
    geom_smooth(aes_string(y=y))
}

add.rank <- function(data, variable) {
  data <- eval(substitute(
    ddply(data, c("year"), transform, rank=rank(-variable, ties.method=c("first"), na.last='keep')),
    list(variable=as.name(variable))
  ))
  
  data <- eval(substitute(
    ddply(data, c("country"), transform, change=c(NA, diff(variable))),
    list(variable=as.name(variable))
  ))
  
  data <- ddply(data, c("country"), transform, change.rank=c(NA, diff(rank)))
  data
}

find.change <- function(data, variable) {
  var <- eval(substitute(
    ddply(data, c("country"), summarize, variance=var(variable, na.rm=TRUE)),
    list(variable=as.name(variable))
    ))
  hi.change <- with(var, country[variance>=quantile(variance, 0.75)])
  var.rank <- ddply(data, c("country"), summarize, var.rank=var(rank, na.rm=TRUE))
  hi.change.rank <- with(var.rank, country[var.rank>=quantile(var.rank, 0.75)])
  
  list(hi.change=hi.change, hi.change.rank=hi.change.rank) 
}