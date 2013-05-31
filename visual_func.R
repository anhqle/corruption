library(ggplot2)
library(plyr)
library(reshape2)

base.plot <- function(data, x, y) {
  ggplot(data=data, aes_string(x=x)) +
    geom_text(aes_string(y=y, label="countrycode"), size=3) +
    geom_smooth(aes_string(y=y))
}