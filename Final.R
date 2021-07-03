#1 
#Voter prediction could be difficult since data can be biased. The data used to predict the election will most likely come from surveys, which, 
#depending on how the survey is adminstered could contain a lot of people on one side than the other. If the survey was conducted online at a college
#it will most likely contain democrats. Voters may also answer that they will vote for one candidate, but may actually vote for the other candidate for
#whatever reason. People that are on side may not be willing to answer to surveys either.

#incorrect sampling that leads to uneven distribution of sample size which fails to properly predict the actual population. 

#2 
#Silver seemed to have used machine learning techniques such as hierarchical modelling while also generating a time series. The time series
#graph takes into account measurable variables as well as nonmeasurable variables which are included as extra random terms. Then the data is
#simulated forward in time showing possible percentages for who wins in each state. The hierarchical modelling is used for state polls to calculate 
#data for states that might not have gotten polled which may help with nonresponse bias. All of the math is calculated using the Bayes' Theorem.

#3
#In 2016, all the polls seem to be answered mainly by supporters of Clinton while leaving out the supporters of Trump. They all seem to contain
#errors that led to seeming like Clinton would win by a large margin. People might not be willing to say that they were voting for Trump and
#and said something else. Women voters might have been especially affected by this phenomenon. Trump had also gotten support from other Republican 
#voters who supported other candidates or were undecided during the polls. 


library(dplyr)
library(readr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(maps)
library(tidyr)
library(hexbin)
## set the working directory as the file location
setwd("M:/Jason/Virus/School/PSTAT 131/Project/data")
## put the data folder and this handout file together.
## read data and convert candidate from string to factor
election.raw <- read_delim("data/election/election.csv", delim = ",") %>% mutate(candidate=as.factor(candidate))

census_meta <- read_delim("data/census/metadata.csv", delim = ";", col_names = FALSE) 
census <- read_delim("data/census/census.csv", delim = ",") 

kable(election.raw %>% filter(county == "Los Angeles County"))  %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width=FALSE)

#4)
election.raw <- election.raw[election.raw$fips != 2000, ]
dim(election.raw)

#5)
election_federal <- election.raw[election.raw$fips == "US",]
election_state <- election.raw[election.raw$fips !="US" & is.na(election.raw$county),]
election <- election.raw[!is.na(election.raw$county),]

#6
ggplot(data=election.raw, aes(x=reorder(candidate, votes), y=votes, color=candidate, fill=candidate))+geom_bar(stat="identity") + coord_flip()+theme_minimal()+theme(legend.position="none")

#7)
groupCounty <- group_by(election,fips)
totalCounty <-election%>%group_by(fips)%>%summarise(total = sum(votes))
combinedCounty <- left_join(groupCounty, totalCounty, by = "fips")
countyPct <- mutate(combinedCounty, pct=votes/total)
county_winner <- top_n(countyPct, n=1)
county_winner

groupState <- group_by(election_state,fips)
totalState <-election_state%>%group_by(fips)%>%summarise(total = sum(votes))
combinedState <- left_join(groupState, totalState, by = "fips")
statePct <- mutate(combinedState, pct=votes/total)
state_winner <- top_n(statePct, n=1)
state_winner

## instructions 
states <- map_data("state")

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # color legend is unnecessary and takes too long

#8
counties = map_data("county")
ggplot(data = counties) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)

#9
fips = state.abb[match(states$region, tolower(state.name))]
states <- data.frame(states,fips)
statesMap <- left_join(states, state_winner, by = "fips")

#10
countyMap <- maps::county.fips
county.fips <- countyMap %>% separate(polyname, c("region","subregion"),sep=",")
county <- left_join(county.fips,counties,by = c("subregion","region"))
class(county$fips)="character"
county <- left_join(county,county_winner, by = "fips")

ggplot(data = county) + 
  geom_polygon(aes(x = long, y = lat, fill = candidate, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # color legend is unnecessary and takes too long

#11) 
ggplot(data=census,aes(x=White,y=Employed))+geom_hex(bins=70) + scale_fill_continuous(type="viridis") +theme_bw()

#12)
census.del <- census %>% drop_na()
census.del$Men = census.del$Men*100/census.del$TotalPop
census.del$Employed=census.del$Employed*100/census.del$TotalPop
census.del$Citizen=census.del$Citizen*100/census.del$TotalPop

census.del$Minority=census.del$Hispanic+census.del$Black+census.del$Native+census.del$Asian+census.del$Pacific
census.del$Hispanic <- NULL
census.del$Black <- NULL
census.del$Asian <- NULL
census.del$Pacific <- NULL
census.del$Native <- NULL
census.del$Walk <- NULL
census.del$PublicWork <- NULL
census.del$Construction <- NULL
census.del$Women <- NULL

census.subct <- census.del%>%group_by(State, County)%>%add_tally(name="CountyTotal")
census.subct$countyWeight <- census.subct$TotalPop/census.subct$CountyTotal
census.ct <- census.subct
countyWeightSum <-census.ct  %>%summarize_at(vars(countyWeight),sum)

names(countyWeightSum)[ncol(countyWeightSum)] <- "countyWeightSum"
census.ct <- left_join(census.ct,countyWeightSum, by = c("State","County"))

census.ct<-mutate(census.ct, countyWeight = countyWeight/countyWeightSum)
census.ct <- select(census.ct, -countyWeightSum, -CountyTotal)
census.ct[4:29] <- census.ct [4:29]*census.ct$countyWeight
census.ct <- census.ct%>%summarise_at(vars(Men:Minority),funs(sum))
census.ct<-ungroup(census.ct)

head(census.ct)

#13.
numCounty <- census.ct[3:27]
ct.pc <- prcomp(numCounty, scale=TRUE, center=TRUE)
ct.rotation <- ct.pc$rotation[,c(1,2)]

pc1<-sort(abs(ct.rotation[,1]),decreasing=TRUE)
head(pc1)
ct.rotation[c("IncomePerCap","ChildPoverty","Poverty"),]

numSubCount <- census.subct[3:28]
subct.pc <- prcomp(numSubCount, scale=TRUE, center=TRUE)
subct.rotation <- subct.pc$rotation[,c(1,2)]
pc2<-sort(abs(subct.rotation[,1]),decreasing=TRUE)
head(pc2)
subct.rotation[c("IncomePerCap","Professional","Poverty"),]


#14.

#for county
ct.var <- (ct.pc$sdev)^2
ct.pve = ct.var/sum(ct.var)
ct.cumulative <- cumsum(ct.pve)
which(ct.cumulative>=0.90)[1]

#for subcounty
subct.var <- (subct.pct$sdev)^2
subct.pve = subct.var/sum(subct.var)
subct.cumulative <- cumsum(subct.pve)
which(subct.cumulative>=0.90)[1]

#plot for county
plot(ct.pve, type="l", lwd=3)
plot(ct.cumulative, type="l", lwd=3)

#plot for subcounty
plot(subct.pve, type="l",lwd=3)
plot(subct.cumulative, type="l", lwd=3)

which(abs(rotation.pc[,1]) == 0.351555)


library(dendextend)
#15.
# ct.clustdist = dist(census.ct, method ="complete")
# plot(ct.clustdist)
# ct.dend = as.dendrogram(ct.hclustdist)
# ct.dend = color_branches(ct.dend, k=10)
# ct.dend = color_labels(ct.dend, k=10)
# ct.dend = set(ct.dend, "labels_cex",0.5)
# plot(ct.dend, horiz=T, main ="Heirarchical Clustering")
# 
# censusct.pc5=ct.pc$x[,1:5]
# ct.clustdist5= dist(censusct.pc5, method ="euclidean")
# ct.hclustdist5 = hclust(ct.clustdist5)
# ct.dend = as.dendrogram(ct.hclustdist)
# ct.dend = color_branches(ct.dend, k=10)
# ct.dend = color_labels(ct.dend, k=10)
# ct.dend = set(ct.dend, "labels_cex",0.5)
# plot(ct.dend, horiz=T, main ="Heirarchical Clustering PC5")
###########################################
numeric.census.ct = scale(census.ct[ ,-c(1,2)], center=TRUE, scale=TRUE)

distanceCensus = dist(numeric.census.ct, method = "euclidean") 
census.hcComp = hclust(distanceCensus, method = "complete")
census.hc10 = cutree(census.hcComp, k=10)

distcensus.pc5 = dist(ct.pc$x[,1:5], method = "euclidean")
census.pc5 = hclust(distcensus.pc5, "complete")
census.pc5.hc10 = cutree(census.pc5, k=10)

table(census.hc10)
table(census.pc5.hc10)

census.hccComp()
plot(census.hcComp)
plot(census.pc5)

#sanMateo 
SanMateo <- which(census.ct$County == "San Mateo")
census.hc10[227]
census.pc5.hc10[227]

census.hc10.cl3<-which(census.hc10=="7")
census.pc5.hc10.cl1 <-which(census.pc5.hc10 == "4")

census.ct[census.hc10.cl3,]
census.ct[census.pc5.hc10.cl1,]

scaled.three=scale(census.hc10.cl3, center=TRUE)
scaled.one=scale(census.pc5.hc10.cl1, center = TRUE)

scaled.three.dist = dist(scaled.three)
scaled.one.dist = dist(scaled.one)

#Mean
mean(scaled.three.dist)
mean(scaled.one.dist)

sd(scaled.three.dist)
sd(scaled.one.dist)

#16
tmpwinner <- county_winner %>% ungroup %>%
  mutate(state = state.name[match(state, state.abb)]) %>%               ## state abbreviations
  mutate_at(vars(state, county), tolower) %>%                           ## to all lowercase
  mutate(county = gsub(" county| columbia| city| parish", "", county))  ## remove suffixes
tmpcensus <- census.ct %>% mutate_at(vars(State, County), tolower)

election.cl <- tmpwinner %>%
  left_join(tmpcensus, by = c("state"="State", "county"="County")) %>% 
  na.omit

## save meta information
election.meta <- election.cl %>% select(c(county, fips, state, votes, pct, total))

## save predictors and class labels
election.cl = election.cl %>% select(-c(county, fips, state, votes, pct, total))

set.seed(10) 
n <- nrow(election.cl)
in.trn <- sample.int(n, 0.8*n) 
trn.cl <- election.cl[ in.trn,]
tst.cl <- election.cl[-in.trn,]

set.seed(20) 
nfold <- 10
folds <- sample(cut(1:nrow(trn.cl), breaks=nfold, labels=FALSE))

calc_error_rate = function(predicted.value, true.value){
  return(mean(true.value!=predicted.value))
}
records = matrix(NA, nrow=3, ncol=2)
colnames(records) = c("train.error","test.error")
rownames(records) = c("tree","logistic","lasso")

#-------------------------------------------
YTrain=trn.cl$candidate
YTest=tst.cl$candidate
#-------------------------------------------

#------------------------------------------
#logistic regression for training data

glm.fit = glm(candidate ~.,data= trn.cl,family=binomial)

response = predict(glm.fit, trn.cl, type="response")
predict.train <- trn.cl %>% mutate(res=(ifelse(response>.50, "Hillary Clinton", "Donald Trump")))
training_error<-calc_error_rate(predict.train$res,trn.cl$candidate)

response = predict(glm.fit, tst.cl, type="response")
predict.train <- tst.cl %>% mutate(res=(ifelse(response>.50, "Hillary Clinton", "Donald Trump")))
test_error<-calc_error_rate(predict.train$res,tst.cl$candidate)

records[2,1]<-training_error
records[2,2]<-test_error

------------------------------
library(glmnet)
x = model.matrix(candidate~., trn.cl)[,-1]
y <- droplevels(as.factor(trn.cl$candidate))
ridge.mod=cv.glmnet(x,y,alpha=1,family="binomial",lambda = c(1, 5, 10, 50) * 1e-4)
best<-ridge.mod$lambda.min
best
lasso_mod = glmnet(x, y, alpha = 1, family="binomial", lambda  = best)
lasso.coef <- coef(lasso_mod)
which(lasso.coef != 0)

coef<- predict(lasso_mod,type="coefficients",s=best)[1:20,]
x.tst <- model.matrix(candidate ~ . , tst.cl)[,-1]
train.pred = predict(lasso_mod, newx= x,type="response")
test.pred = predict(lasso_mod, newx= x.tst, type = "response")

lasso.train <- trn.cl %>% mutate(predict.train=(ifelse(train.pred>.50, "Hillary Clinton", "Donald Trump")))
lasso.test <- tst.cl %>% mutate(predict.test=(ifelse(test.pred>.50, "Hillary Clinton", "Donald Trump")))

training_error <- calc_error_rate(lasso.train$predict.train, trn.cl$candidate)
test_error <- calc_error_rate(lasso.test$predict.test, tst.cl$candidate)

records[3,1] <-training_error
records[3,2]<-test_error
records

#19
library(ROCR)
pred.tree <- predict(tree.pruned, tst.cl, type = "vector")
perf.tree <- performance(prediction(pred.tree[,13], as.numeric(tst.cl$candidate)), "tpr", "fpr")

pred.log <- predict(glm.fit, tst.cl, type = "response")
perf.log <- performance(prediction(pred.log, as.numeric(tst.cl$candidate)), "tpr", "fpr")

pred.lasso <- predict(lasso_mod, newx =x.tst, s = best, type="response")
perf.lasso <- performance(prediction(pred.lasso, as.numeric(tst.cl$candidate)), "tpr", "fpr")

tree.lm = plot(perf.tree, col=2, lwd=3, text=T, main="ROC Curve")
log.lm = plot(perf.log, add=TRUE, col=3, lwd=3)
lasso.lm = plot(perf.lasso, add=TRUE, col=4, lwd=3)

legend("right", legend=c("decision tree", "logistic","lasso logistic"), col = c(2,3,4),lty=1:1)
abline(0,1)

auc = performance(prediction(pred.tree[,13], as.numeric(tst.cl$candidate)), "auc")@y.values
auc

auc1 = performance(prediction(pred.log, as.numeric(tst.cl$candidate)), "auc")@y.values
auc1

auc2 = performance(prediction(pred.lasso, as.numeric(tst.cl$candidate)), "auc")@y.values
auc2
