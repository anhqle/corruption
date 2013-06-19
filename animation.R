rm(list=ls())
try(detach("package:Hmisc", unload=TRUE), silent=TRUE)
library(ggplot2)
library(reshape2)
library(plyr)
library(maps)
library(animation)
source("visual_func.R")
load("gov_clean.RData")

wgi <- add.rank(wgi, "cc_est")
wgi.change <- find.change(subset(wgi, ADB==1 & asia==1), "cc_est")

country <- map_data("world")
wgi$country[wgi$country=="United States"] <- "USA"
wgi$country[wgi$country=="Russian Federation"] <- "USSR"
wgi$country[wgi$country=="United Kingdom"] <- "UK"
wgi$country[wgi$country=="St. Vincent and the Grenadines"] <- "Saint Vincent"
wgi$country[wgi$country=="St. Lucia"] <- "Saint Lucia"
wgi$country[wgi$country=="Congo, Dem. Rep."] <- "Zaire"

saveSWF({
  for (i in c(1996, 1998, 2000, 2002:2011)) {
    temp <- subset(na.omit(wgi[, c('year', 'country', 'cc_est')]), year==i)
    choro <- merge(country, temp, by.x="region", by.y="country")
    choro <- choro[order(choro$order), ]
    p <- ggplot(data=choro, aes(long, lat, fill=cc_est, group=group))
    print(
      p + geom_polygon() + ylim(c(-60,85)) +
        scale_fill_gradient("Control of\n corruption\n score", limits=c(-2,2.5)) + 
        labs(title=substitute(paste("Severity of corruption in the world, year ", i), list(i=i)))
      )
#     qplot(long, lat, data=choro, fill=cc_est, group=group, geom="polygon") +
#       scale_fill_gradient()
  }
}, swf.name="corr_ani.swf", ani.width = 1000, ani.height = 600,
        swftools="C:/Program Files (x86)/SWFTools",
         interval=1, outdir=getwd())

oopt = ani.options(interval = 0.1, nmax = 100)
## brownian motion: note the 'loop' option and how to set graphics
#    parameters with 'ani.first'
saveLatex({
  brownian.motion(pch = 21, cex = 5, col = "red", bg = "yellow",
                  main = "Demonstration of Brownian Motion")
}, ani.basename = "BM", ani.opts = "controls,loop,width=0.8\\textwidth",
          ani.first = par(mar = c(3, 3, 1, 0.5), mgp = c(2, 0.5, 0),
                          tcl = -0.3, cex.axis = 0.8, cex.lab = 0.8, cex.main = 1),
          latex.filename = "brownian.motion.tex", outdir=getwd())
ani.options(oopt)