library(plyr)
library(rpart)
library(ggplot2)
library(rattle)
library(RColorBrewer)


#These data tables come from Kaggle.com 
fielding = read.table(file.choose(), header=T, sep =',' )
all_star = read.table(file.choose(), header=T, sep =',' )
batting = read.table(file.choose(), header=T, sep =',' )

#Player table adjusted, only including player_id and birth_year
player = read.table(file.choose(), header=T, sep =',' )

#Shortstop MATRIX
batting = batting[batting$ab > 129, ]
fielding = fielding[fielding$g > 19, ]

all_star = all_star[,c(1,2,5)]
all_star = unique(all_star)
as_app = as.data.frame(table(all_star$player_id)) 

ss = subset(fielding, pos == "SS")
ss = merge(ss, batting, by = c("player_id","year"))

ss = subset(ss, !duplicated(ss$player_id, nmax = 2))
ss = merge(ss, player, by=c("player_id"))

v = subset(fielding, pos == "SS")
v = v[,c(1,6)]
v = unique(v)
as_app = merge(as_app, v, by.x =c("Var1"), by.y = c("player_id") )

ss = merge(ss, as_app, by.x = c("player_id", "pos"), by.y = c("Var1", "pos"), all = T)
ss$Freq[is.na(ss$Freq)] <- 0
ss$cs.y[is.na(ss$cs.y)] <- round(ss$sb.y * .33)
ss$g_idp[is.na(ss$g_idp)] <- round(ss$ab * .02)
ss$sf[is.na(ss$sf)] <- round(ss$ab * .00667)
ss$ibb[is.na(ss$ibb)] <- 1
ss["age"] = ss$year - ss$birth_year
ss["f_pct"] = (ss$po + ss$a) / (ss$po + ss$a+ ss$e)
ss["RF"] = (ss$po + ss$a) / ss$g.x
ss["avg"] = ss$h / ss$ab
ss["obp"] = (ss$h + ss$bb + ss$hbp) / (ss$ab + ss$bb + ss$hbp + ss$sf)
ss["TB"] = (ss$h - ss$double -ss$triple - ss$hr)+ (2*ss$double) + (3*ss$triple) + (4*ss$hr) 
ss["TA"] = (ss$TB + ss$bb + ss$hbp + ss$sb.y - ss$cs.y) / (ss$ab - ss$h + ss$cs.y + ss$g_idp)
ss["SLG"] = ss$TB / ss$ab
ss["OPS"] = (ss$obp*1.73) + ss$SLG
ss["PA"] = ss$ab + ss$bb + ss$hbp + ss$sh + ss$sf
ss["as"] = ifelse(ss$Freq >0, 1, 0)
ss["as.2"] = ifelse(ss$Freq >3, 1, 0)
ss["as.c"] = ss$as + ss$as.2
ss = ss[ss$year > 1932,]

#From Sabermetrics
woba = read.table(file.choose(), header=T, sep =',' )

ss = merge(ss, woba, by = c("year"))
ss["w_OBAply"] = ((ss$wBB* ss$bb) + (ss$wHBP*ss$hbp) + (ss$w1B*(ss$h - ss$double - ss$triple - ss$hr)) + (ss$w2B*ss$double) + (ss$w3B*ss$triple) + (ss$wHR*ss$hr)) / (ss$ab + ss$bb - ss$ibb + ss$sf + ss$hbp)
ss["wRAA"] = ((ss$w_OBAply - ss$wOBA) / (ss$wOBAScale)) *ss$PA
ss = ss[,-c(56:66)]

#Age/Batting stats Matrix -- Z
#reload batting data
batting = read.table(file.choose(), header=T, sep =',' )
z = merge(batting, player, by = c("player_id"))
z["age"] = z$year - z$birth_year
z = aggregate( cbind( g, ab, r, h,double,triple,hr, rbi, sb, cs,bb,so,ibb,hbp,sh,sf,g_idp ) ~ age , data = z , sum)
z = z[c(3:25),]
z["age_avg"] = z$h / z$ab
z["age_obp"] = (z$h + z$bb + z$hbp) / (z$ab + z$bb + z$hbp + z$sf)
z["age_TB"] = (z$h - z$double -z$triple - z$hr)+ (2*z$double) + (3*z$triple) + (4*z$hr) 
z["age_TA"] = (z$age_TB + z$bb + z$hbp + z$sb - z$cs) / (z$ab - z$h + z$cs + z$g_idp)
z["age_SLG"] = z$age_TB / z$ab
z["age_OPS"] = (z$age_obp * 1.73) + z$age_SLG
z = z[, c(1, 19:24)]

#AGE Fielding AVG -- X
#reload fielding
fielding = read.table(file.choose(), header=T, sep =',' )
x = subset(fielding, pos == "SS")
x = merge(x, player, by=c("player_id"))
x["age"] = x$year - x$birth_year
x = aggregate(cbind(g,po,a,e,dp)~age, data = x, sum)
x["age_f_pct"] = (x$po + x$a) / (x$po + x$a+ x$e)
x["age_RF"] = (x$po + x$a) / x$g
x = x[,c(1,7,8)]

#TOTAL MATRIX
total = merge(ss,z, by= c("age"))
total = merge(total,x, by= c("age"))
total = total[total$year < 2009, ]
total["d.avg"] = (total$avg - total$age_avg)
total["d.ops"] = (total$OPS - total$age_OPS)
total["d.ta"] = (total$TA - total$age_TA)
total["d.wraa"] = (total$wRAA - total$age_WRAA)
total["d.woba"] = (total$w_OBAply - total$age_w_OBAply)
total["d.f_pct"] = (total$f_pct - total$age_f_pct)
total["d.rf"] = (total$RF - total$age_RF)

#More Tangibles for analysis
#plyrtan Player data table composed of player_id, weight, height, bats, throws of player table
plyrtan = read.table(file.choose(), header=T, sep =',' )
total = merge(total, plyrtan, by = c("player_id"))

#Talent Projection - projecting the future talent in that specific yr (how hard will it become to make All-Star status yrs going forward)
#Experiment, in the end did not need...but interesting
#reload data
fielding = read.table(file.choose(), header=T, sep =',' )
all_star = read.table(file.choose(), header=T, sep =',' )
batting = read.table(file.choose(), header=T, sep =',' )
player = read.table(file.choose(), header=T, sep =',' )
all_star = all_star[,c(1,2)]
b = batting

b$cs[is.na(b$cs)] <- round(ss$sb * .33)
b$g_idp[is.na(b$g_idp)] <- round(ss$ab * .02)
b$sf[is.na(b$sf)] <- round(ss$ab * .00667)
b$ibb[is.na(b$ibb)] <- 1
b = aggregate( cbind( g, ab, r, h,double,triple,hr, rbi, sb, cs,bb,so,ibb,hbp,sh,sf,g_idp ) ~ player_id+year , data = b , sum)

f = subset(fielding, pos == "SS")
f = aggregate( cbind( g, po,a,e,dp ) ~ player_id+year , data = f , sum)

proj = merge(f, all_star, by=c("player_id", "year") )
proj = merge(proj, b, by=c("player_id", "year") )
proj = merge(proj, player, by=c("player_id") )
proj = proj[proj$g.x > 19, ]
proj["age"] = proj$year - proj$birth_year

#includes INF who made all star game and secondary pos is ss
#Experiment
proj["age"] = proj$year - proj$birth_year
proj["TB"] = (proj$h - proj$double -proj$triple - proj$hr)+ (2*proj$double) + (3*proj$triple) + (4*proj$hr)
proj["obp"] = (proj$h + proj$bb + proj$hbp) / (proj$ab + proj$bb + proj$hbp + proj$sf)
proj["SLG"] = proj$TB / proj$ab
proj["OPS"] = (proj$obp * 1.73) + proj$SLG
proj["RF"] = (proj$po + proj$a) / proj$g.x
proj["f_pct"] = (proj$po + proj$a) / (proj$po + proj$a+ proj$e)
proj["x"] = proj$OPS / proj$age
proj["y"] = proj$TB / proj$age
proj["b"] = proj$f_pct / proj$age
proj["c"] = proj$RF / proj$age

data = aggregate(cbind(g.x,po,a,e,dp, ab, r, h,double,triple,hr, rbi, sb, cs,bb,so,ibb,hbp,sh,sf,g_idp ) ~ year, data = proj, sum)
d = aggregate(cbind(age)~year, data = proj, mean)
data = merge(data, d, by = c("year"))
data["TB"] = (data$h - data$double -data$triple - data$hr)+ (2*data$double) + (3*data$triple) + (4*data$hr)
data["obp"] = (data$h + data$bb + data$hbp) / (data$ab + data$bb + data$hbp + data$sf)
data["SLG"] = data$TB / data$ab
data["OPS"] = (data$obp * 1.73) + data$SLG
data["RF"] = (data$po + data$a) / data$g.x
data["f_pct"] = (data$po + data$a) / (data$po + data$a+ data$e)
data["x.OPS"] = data$OPS / data$age
data["x.TB"] = data$TB / data$age
data["x.f_pct"] = data$f_pct / data$age
data["x.rf"] = data$RF / data$age
data = data[,c(1,30:33,23)]
data["n.OPS"] = (data$x.OPS - min(data$x.OPS)) / (max(data$x.OPS) - min(data$x.OPS))
data["n.TB"] = (data$x.TB - min(data$x.TB)) / (max(data$x.TB) - min(data$x.TB))
data["n.f_pct"] = (data$x.f_pct - min(data$x.f_pct)) / (max(data$x.f_pct) - min(data$x.f_pct))
data["n.rf"] = (data$x.rf - min(data$x.rf)) / (max(data$x.rf) - min(data$x.rf))
data["tot1"] = data$n.rf + data$n.f_pct + data$n.TB + data$n.OPS
data["tot2"] = data$n.rf + data$n.TB + data$n.OPS
data["tot3"] = data$n.rf + data$n.OPS
data["tot4"] = data$n.rf + data$n.TB
data = data[,-c(7:10)]

#FINAL MATRIX
total = merge(total, data, by = c("year"))


#FEATURE SELECTION MEthods 
u = total[,c(51, 3,11:14, 24:39, 42:50, 56:77, 79:83)]
#1. Gini impurity
o = NULL
for(i in 2:ncol(u)){
  tree_i = rpart(as~  d.ta + TA +d.ops+double+ u[,i], data = total ,method = "class",control = rpart.control(cp=0.00001))
  pred_i <- predict(tree_i, u, type = "class")
  conf_i <- table(u$as, pred_i)
  acc_i <- sum(diag(conf_i)) / sum(conf_i)
  o[i] = acc_i}
o

#2. Information Gain
u= NULL
for(i in 2:ncol(u)){
  tree_i <- rpart(as ~ u[,i], u, method = "class", parms = list(split =   "information"))
  pred_i <- predict(tree_i, u, type = "class")
  conf_i <- table(u$as, pred_i)
  acc_i <- sum(diag(conf_i)) / sum(conf_i)
  print(acc_i)}

tree = rpart(as~ TB + d.rf +w_OBAply+age.x+weight+OPS+r+d.avg+dp, data = total ,method = "class",control = rpart.control(cp=0.00001))
pred = predict(tree, total, type = "class")
conf = table(total$as, pred)
conf

#SPLIT DATA - TRAIN & TEST
n = nrow(total)
shuffled = total[sample(n), ]
train = shuffled[1:round(0.6*n),]
test <- shuffled[(round(0.6 * n) + 1):n,]

tree = rpart(as~ TB + d.rf, data = train ,method = "class")
pred = predict(tree, test, type = "class")
conf = table(test$as, pred)
conf
acc

#TESTS - Percentages
TP = conf[1,1]
FN = conf[1,2]
FP = conf[2,1]
TN = conf[2,2]
acc = (TP + TN) / (TP + FN + FP + TN)
acc
prec = TP / (TP + FP)
prec
rec = TP / (TP + FN)
rec

#CROSS VALIDATION - 4
accs <- rep(0,4)
for (i in 1:4) {
  indices = (((i-1) * round((1/4)*nrow(shuffled))) + 1):((i*round((1/4) * nrow(shuffled))))
  train = shuffled[-indices,]
  test = shuffled[indices,]
  tree = rpart(as~c.avg, train, method = "class")
  pred = predict(tree, test, type = "class")
  conf = table(test$as, pred)
  accs[i] = sum(diag(conf))/sum(conf)
}
mean(accs)

#ROC curve
library(ROCR)
all_probs <- predict(tree, test, type = "prob")
all_probs

tree <- rpart(as ~ ., train, method = "class")
probs <- predict(tree, test, type = "prob")[,2]
pred <- prediction(probs, test$as)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

#Testing?
tree = rpart(as~ TB + d.rf +w_OBAply+age.x+weight+OPS+r+d.avg+dp, data = total ,method = "class",control = rpart.control(cp=0.00001))
total["pred"] = predict(tree, total, type = "class")
total["right"] = ifelse( total$as == total$pred, "Y", "N" )

#Quick sample graph
ggplot(total, aes(factor(age.x), d.rf, shape = factor(as), colour = right)) + geom_point() 

label = labs(title = "Career Outcomes of Shortstops Prediction (Three Classes)", y = "Batting Average", x = "Fielding Percentage")
ggplot(total, aes(f_pct, avg, shape = factor(as.c), colour = factor(right))) +geom_point() +label+theme(legend.title=element_blank())

label = labs(title = "Actual Career Outcomes of Shortstops (Three Classes)", y = "Batting Average", x = "Fielding Percentage")
ggplot(total, aes(f_pct, avg, shape = factor(as.c), colour = factor(as.c))) +geom_point() +label+theme(legend.title=element_blank())

fancyRpartPlot(tree)










