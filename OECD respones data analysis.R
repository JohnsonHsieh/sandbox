url <- "http://www.oecdbetterlifeindex.org/bli/rest/indexes/stats/country"
country <- read.table(url, header=T)
tmp <- data.frame("country"=country$country, "responses"=country$numResponses)
tmp <- tmp[-1,]
i=1

library(jsonlite)
json <- paste("http://www.oecdbetterlifeindex.org/bli/rest/indexes/responses;country=",tmp[1,1],";limit=",tmp[1,2],sep="")
a <- fromJSON(json)[,c(1,2,3,4,6)]

for(i in 2:nrow(tmp)){
  json <- paste("http://www.oecdbetterlifeindex.org/bli/rest/indexes/responses;country=",tmp[i,1],";limit=",tmp[i,2],sep="")
  b <- fromJSON(json)[,c(1,2,3,4,6)]
  a <- rbind(a, b)
}

score <- do.call("rbind", lapply(strsplit(a[,1],""), as.numeric))
colnames(score) <- c("Housing", "Income", "Jobs", "Community", "Education",
                     "Environment", "Civi Engagement", "Health", "Life Satisfaction", 
                     "Safety", "Work-Life Balance")
out <- data.frame(score, a[,-1])
# out[,"timestamp"] <- as.Date(out[,"timestamp"], format="%Y-%m-%dT%H:%M:%S")
head(out)
out[,"timestamp"] <- as.Date(out[,"timestamp"])

name_list = rbind(
  c("AUS","Australia"),
  c("AUT","Austria"),
  c("BEL","Belgium"),
  c("BRA","Brazil"),
  c("CHL","Chile"),
  c("DNK","Denmark"),
  c("EST","Estonia"),
  c("FIN","Finland"),
  c("FRA","France"),
  c("GRC","Greece"),
  c("IRL","Ireland"),
  c("ISL","Iceland"),
  c("ISR","Israel"),
  c("ITA","Italy"),
  c("JPN","Japan"),
  c("CAN","Canada"),
  c("KOR","Korea"),
  c("LUX","Luxembourg"),
  c("MEX","Mexico"),
  c("NLD","Netherlands"),
  c("NZL","New Zealand"),
  c("NOR","Norway"),
  c("POL","Poland"),
  c("PRT","Portugal"),
  c("RUS","Russian Federation"),
  c("SWE","Sweden"),
  c("SVK","Slovak Republic"),
  c("SVN","Slovenia"),
  c("ESP","Spain"),
  c("CHE","Switzerland"),
  c("CZE","ch Republic"),
  c("TUR","Turkey"),
  c("HUN","Hungary"),
  c("GBR","United Kingdom"),
  c("USA","United States"),
  c("TWN","Taiwan"))
# dim(out)
out$country <- factor(out$country, levels=name_list[,1])
out <- na.omit(out)
# dim(out)
for(i in 1:nrow(name_list)){
  out$country <- gsub(name_list[i,1], name_list[i,2], out$country)
}

table(out$country)


library(reshape)
res <- list()
for(i in 1:11){
  res[[i]] <- cast(out, country ~., fun.aggregate=mean, value=colnames(out)[i])[,2]
}

res <- data.frame(cast(out, country ~., fun.aggregate=mean, value=colnames(out)[i])[,1], do.call("cbind", res))
colnames(res) <- c("country", colnames(out)[1:11])
rownames(res) <- res[,1]
res <- res[,-1]
library(FactoMineR)
result <- PCA(res) # graphs generated automatically

plot(res$Housing, res$Income, pch="")
text(res$Housing, res$Income, res$country)
