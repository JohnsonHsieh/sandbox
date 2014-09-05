#'---
#'title: "Data analysis of adulterated oil"
#'author: "Johnson Hsieh (http://readata.org)"
#'date: "Friday, September 05, 2014"
#'output: html_document
#'---
#'
#' ## Basic data exploration
#+ warning=FALSE, comment=""
dat <- read.csv("manufacturer-list.csv")
dat <- within(dat, name <- factor(name, levels=name[order(-count)]))
dat <- dat[order(-dat$count, dat$region),]
head(dat)

library(ggplot2)
ggplot(dat, aes(x=1:length(count), y=count)) + geom_line(size=1) +
  labs(x="客戶排序", y="數量(桶)") + theme(text=element_text(size=18))

ggplot(dat[1:10,], aes(x=name, y=count)) + geom_bar(stat="identity") +
  labs(x="客戶排序", y="數量(桶)", title="Top 10") + theme(text=element_text(size=18))



#' ## Estimation of overall manufacturers
#+ warning=FALSE, comment=""
library(devtools)
# install_github("iNEXT", "JohnsonHsieh")
library(iNEXT)
out <- iNEXT(as.numeric(dat$count), size = seq(2000, 200000, length.out=200))
lab1 <- paste("廠商數(衛福部) =",out$BasicIndex[1,1])
lab2 <- paste("廠商數(估計) =",out$BasicIndex[1,2])
g <- ggiNEXT(out) + guides(colour=FALSE, fill=FALSE) +
  geom_hline(yintercept=out$BasicIndex[1,2], linetype=3, size=1.5, col="#00BFC4")
g + geom_text(x=80000, y=out$BasicIndex[1,1]-5, label=lab1, size=6) + 
  geom_text(x=160000, y=out$BasicIndex[1,2]+5, label=lab2, size=6, col="#00BFC4") +
  labs(x="銷售數量(桶)", y="廠商數量")

#' ## GIS visualization 
#+ warning=FALSE, comment=""
library(ggmap)
map <- get_googlemap(center = c(lon = 121, lat = 23.5), zoom=7, size=c(1024,1024),
                     color = "color", maptype = "terrain")

ggmap(map, extent = "device", legend="topleft") + 
  xlim(c(118.5,123)) + ylim(c(21,26.5)) + 
  geom_point(data = dat, aes(x = Longitude, y = Latitude, size=count), 
             alpha=0.6, color="#F8766D") + 
  scale_size_continuous(range=c(4,15)) + guides(size=guide_legend("進貨數量")) +
  theme(text=element_text(size=15))
