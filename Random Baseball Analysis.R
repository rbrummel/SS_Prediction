#Random Baseball Analysis
#Bits and pieces of code might be from different sessions

library(ggplot2)

pitching = read.table(file.choose(), header=T, sep =',' )
all_star = read.table(file.choose(), header=T, sep =',' )
batting = read.table(file.choose(), header=T, sep =',' )
fielding = read.table(file.choose(), header=T, sep =',' )

#Player data table, holding only player_id and birthdate
player = read.table(file.choose(), header=T, sep =',' )
pitch_team_merg= merge(all_star, pitching, by.x = c("player_id", "year", "team_id"), by.y = c("player_id", "year", "team_id"))

#BATTING AVG OF INFIELDERS
batting = batting[,c(1,2,7,9,12,14,16,19,21)]

total = merge(batting, player, by=c("player_id"))
total = total[total$year > 1932,]

fielding = fielding[,c(1,2,6)]
SS = subset(fielding, pos == "SS")
SS = merge(total, SS, by=c("player_id", "year"))
SS = unique(SS)
SS["age"] = SS$year - SS$birth_year
x = aggregate( cbind ( ab, h ) ~ age, data = SS, sum)
x["SS_avg"] = x$h / x$ab
x = x [-c(1,2,25:29),]  #at least 2500 at bats #at least 2500 at bats
ggplot(x, aes(age, SS_avg)) +geom_smooth()
all = x[,c(1,4)]

#Compare how All-Stars stand with the avg player
#JETER
jeter = subset(SS, player_id == "jeterde01")
jeter["jeter_avg"] = jeter$h/jeter$ab
jeter_graph = ggplot(jeter, aes(age, jeter_avg)) +geom_smooth()
y = merge(x, jeter, by=c("age"))
y = y[,c(1,4,16)]

#--RIPKEN
ripken = subset(SS, player_id == "ripkeca01")
ripken["ripken_avg"] = ripken$h/ripken$ab
ripken = ripken[,c(12,13)]
y = merge(y, ripken, by=c("age"))

#--OZZIE THE WIZARD
oz = subset(SS, player_id == "smithoz01")
oz["oz_avg"] = oz$h/oz$ab
oz = oz[,c(12,13)]
y = merge(y, oz, by=c("age"))
y

#Ozzie Smith in depth
smith = subset(batting, player_id == "smithoz01")
smith["smith_obp"] = (smith$h + smith$bb + smith$hbp) / (smith$ab + smith$bb + smith$hbp + smith$sf)
smith["smith_TB"] = (smith$h - smith$double -smith$triple - smith$hr)+ (2*smith$double) + (3*smith$triple) +(4*smith$hr)
smith["smith_TA"] = (smith$age_TB + smith$bb + smith$hbp + smith$sb - smith$cs) / (smith$ab - smith$h + smith$cs +smith$g_idp)
smith = merge(smith, player, by = c("player_id"))
smith["age"] = smith$year - smith$birth_year
smith["avg"] = smith$h / smith$ab
smith["smith_avg"] = smith$h / smith$ab
smith = smith[,c(27,29,23)]
u = merge(z, smith, by=c("age"))
smith = subset(total, player_id == "smithoz01")
smith["smith_avg"] = smith$h/smith$ab
smith = smith[,c(7,8)]


label = labs(title = "Ozzie Smith vs Average Player, Batting Averages by Age", y = "Batting Average", x = "Player's Age")
ggplot(u, aes(age)) + geom_smooth(aes(y = age_avg, colour = "Avg. Player")) +geom_smooth(aes(y = smith_avg, colour = "Smith")) + label +theme(legend.title=element_blank())
  
fielding = read.table(file.choose(), header=T, sep =',' )
x = subset(fielding, pos == "SS")
x = merge(x, player, by=c("player_id"))
x["age"] = x$year - x$birth_year
x = aggregate(cbind(g,po,a,e,dp)~age, data = x, sum)
x["age_f_pct"] = (x$po + x$a) / (x$po + x$a+ x$e)
x["age_RF"] = (x$po + x$a) / x$g
View(x)
x = x[,c(1,7,8)]

smith = subset(fielding, player_id == "smithoz01")
smith = merge(smith, player, by=c("player_id"))
smith["age"] = smith$year - smith$birth_year
smith["smith_f_pct"] = (smith$po + smith$a) / (smith$po + smith$a+ smith$e)
smith["smith_RF"] = (smith$po + smith$a) / smith$gsmith = smith[,c(20:22)]
label = labs(title = "Ozzie Smith vs Average Shortstop, Fielding Percentage by Age", y = "Fielding Percentage", x = "Player's Age")
ggplot(x, aes(age)) + geom_smooth(aes(y = age_f_pct, colour = "Avg. Shortstop")) +geom_smooth(aes(y = smith_f_pct, colour = "Smith")) + label +theme(legend.title=element_blank())
ggplot(y, aes(age)) + geom_line(aes(y = avg, colour = "avg")) + geom_line(aes(y = jeter_avg, colour = "jeter_avg")) + geom_line(aes(y = ripken_avg, colour = "ripken_avg")) + geom_line(aes(y = smith_avg, colour = "smith_avg"))
label = labs(title = "Avg Player, Jeter, Ripken, Smith Batting Avg Thru Age", y = "Batting Avg", x = "Player Age")
ggplot(y, aes(age)) + geom_smooth(aes(y = avg, colour = "avg")) + geom_smooth(aes(y = jeter_avg, colour = "jeter_avg")) + geom_smooth(aes(y = ripken_avg, colour = "ripken_avg")) + geom_smooth(aes(y = smith_avg, colour = "smith_avg")) + label
ggplot(y, aes(age)) + geom_smooth(aes(y = avg, colour = "avg"), size = 1.2) + geom_smooth(aes(y = jeter_avg, colour = "jeter_avg"), size = 1.2) + geom_smooth(aes(y = ripken_avg, colour = "ripken_avg"), size = 1.2) + geom_smooth(aes(y = smith_avg, colour = "smith_avg"), size = 1.2) + label


#1B
first = subset(fielding, pos == "1B")
first = merge(total, first, by=c("player_id", "year"))
first = unique(first)
first["age"] = first$year - first$birth_year
x = aggregate( cbind ( ab, h ) ~ age, data = first, sum)
x = x [-c(1,2,26:32),]
x["first_avg"] = x$h / x$ab
x = x[,c(1,4)]
all = merge(all, x, by=c("age"))

#2B
second = subset(fielding, pos == "2B")
second = merge(total, second, by=c("player_id", "year"))
second = unique(second)
second["age"] = second$year - second$birth_year
x = aggregate( cbind ( ab, h ) ~ age, data = second, sum)
x = x [-c(1,24:28),]
x["second_avg"] = x$h / x$ab
x = x[,c(1,4)]
all = merge(all, x, by=c("age"))

#3B
third = subset(fielding, pos == "3B")
third = merge(total, third, by=c("player_id", "year"))
third = unique(third)
third["age"] = third$year - third$birth_year
x = aggregate( cbind ( ab, h ) ~ age, data = third, sum)
x = x [-c(1,2,3,25:31),]
x["third_avg"] = x$h / x$ab
x = x[,c(1,4)]
all = merge(all, x, by=c("age"))


#C
catcher = subset(fielding, pos == "C")
catcher = merge(total, catcher, by=c("player_id", "year"))
catcher = unique(catcher)
catcher["age"] = catcher$year - catcher$birth_year
x = aggregate( cbind ( ab, h ) ~ age, data = catcher, sum)
x = x [-c(1,2,26:29),]
x["catcher_avg"] = x$h / x$ab
x = x[,c(1,4)]
all = merge(all, x, by=c("age"))


#OF
fielding[fielding == "LF" | fielding == "RF" | fielding == "CF"] <- "OF"
of = subset(fielding, pos == "OF")
of = merge(total, of, by=c("player_id", "year"))
of = unique(of)
of["age"] = of$year - of$birth_year
x = aggregate( cbind ( ab, h ) ~ age, data = of, sum)
x = x [-c(1,2,26:29),]
x["of_avg"] = x$h / x$ab
x = x[,c(1,4)]
all = merge(all, x, by=c("age"))



#Batting Avg Graphic
label = labs(title = "Batting Average By Positons and Age", y = "Batting Avg", x = "Player Age", colour = "Infield Positions")
ggplot(all, aes(age)) +geom_line(aes(y = of_avg, colour = "Outfield"), size = 1.2)+ geom_line(aes(y = catcher_avg, colour = "Catcher"), size = 1.2)+ geom_line(aes(y = SS_avg, colour = "Shortstop"), size = 1.2) + geom_line(aes(y = first_avg, colour = "First Base"), size = 1.2) + geom_line(aes(y = second_avg, colour = "Second Base"), size = 1.2) + geom_line(aes(y = third_avg, colour = "Third Base"), size = 1.2) + label

#Early Career
early_two = all[c(1:7),]
ggplot(early_two, aes(age)) +geom_line(aes(y = of_avg, colour = "Outfield"), size = 1.2)+ geom_line(aes(y = catcher_avg, colour = "Catcher"), size = 1.2)+ geom_line(aes(y = SS_avg, colour = "Shortstop"), size = 1.2) + geom_line(aes(y = first_avg, colour = "First Base"), size = 1.2) + geom_line(aes(y = second_avg, colour = "Second Base"), size = 1.2) + geom_line(aes(y = third_avg, colour = "Third Base"), size = 1.2) + label

#HR/AB OF INFIELDERS
#1B
x = aggregate( cbind ( ab, hr ) ~ age, data = first, sum)
x = x [-c(1,2,26:32),]
x["first_hr_avg"] = x$hr / x$ab
x = x[,c(1,4)]
z = x

#2B
x = aggregate( cbind ( ab, hr ) ~ age, data = second, sum)
x = x [-c(1,24:28),]
x["second_hr_avg"] = x$hr / x$ab
x = x[,c(1,4)]
z = merge(z,x, by = c("age"))

#SS
x = aggregate( cbind ( ab, hr ) ~ age, data = SS, sum)
x = x [-c(1,2,25:29),]
x["ss_hr_avg"] = x$hr / x$ab
x = x[,c(1,4)]
z = merge(z,x, by = c("age"))

#3B
x = aggregate( cbind ( ab, hr ) ~ age, data = third, sum)
x = x [-c(1,2,3,25:31),]
x["third_hr_avg"] = x$hr / x$ab
x = x[,c(1,4)]
z = merge(z,x, by = c("age"))

#C
x = aggregate( cbind ( ab, hr ) ~ age, data = catcher, sum)
x = x [-c(1,2,26:29),]
x["catcher_hr_avg"] = x$hr / x$ab
x = x[,c(1,4)]
z = merge(z,x, by = c("age"))

#OF
x = aggregate( cbind ( ab, hr ) ~ age, data = of, sum)
x = x [-c(1,2,26:29),]
x["of_hr_avg"] = x$hr / x$ab
x = x[,c(1,4)]
z = merge(z,x, by = c("age"))

#HR Graphic
label = labs(title = "Infielders HR per AB Ratio by Age", y = "HR per AB Avg", x = "Player Age", colour = "Infield Positions") 
ggplot(z, aes(age)) + geom_line(aes(y = of_hr_avg, colour = "Outfield"), size = 1.2) + geom_line(aes(y = catcher_hr_avg, colour = "Catcher"), size = 1.2) + geom_line(aes(y = ss_hr_avg, colour = "Shortstop"), size = 1.2) + geom_line(aes(y = first_hr_avg, colour = "First Base"), size = 1.2) + geom_line(aes(y = second_hr_avg, colour = "Second Base"), size = 1.2) + geom_line(aes(y = third_hr_avg, colour = "Third Base"), size = 1.2) + label

#OBP
#1B
x = aggregate( cbind ( ab, h, bb, hbp, sf ) ~ age, data = first, sum)
x = x [-c(1,2,26:32),]
x["first_obp"] = (x$h + x$bb + x$hbp) / (x$ab + x$bb + x$hbp + x$sf)
x = x[,c(1,7)]
o = x

#2B
x = aggregate( cbind ( ab, h, bb, hbp, sf) ~ age, data = second, sum)
x = x [-c(1,24:28),]
x["second_obp"] = (x$h + x$bb + x$hbp) / (x$ab + x$bb + x$hbp + x$sf)
x = x[,c(1,7)]
o = merge(o,x, by = c("age"))

#SS
x = aggregate( cbind (ab, h, bb, hbp, sf ) ~ age, data = SS, sum)
x = x [-c(1,2,25:29),]
x["ss_obp"] = (x$h + x$bb + x$hbp) / (x$ab + x$bb + x$hbp + x$sf)
x = x[,c(1,7)]
o = merge(o,x, by = c("age"))

#3B
x = aggregate( cbind ( ab, h, bb, hbp, sf ) ~ age, data = third, sum)
x = x [-c(1,2,3,25:31),]
x["third_obp"] = (x$h + x$bb + x$hbp) / (x$ab + x$bb + x$hbp + x$sf)
x = x[,c(1,7)]
o = merge(o,x, by = c("age"))

#C
x = aggregate( cbind ( ab, h, bb, hbp, sf ) ~ age, data = catcher, sum)
x = x [-c(1,2,26:29),]
x["catcher_obp"] = (x$h + x$bb + x$hbp) / (x$ab + x$bb + x$hbp + x$sf)
x = x[,c(1,7)]
o = merge(o,x, by = c("age"))

#OF
x = aggregate( cbind ( ab, h, bb, hbp, sf ) ~ age, data = of, sum)
x = x [-c(1,2,26:29),]
x["of_obp"] = (x$h + x$bb + x$hbp) / (x$ab + x$bb + x$hbp + x$sf)
x = x[,c(1,7)]
o = merge(o,x, by = c("age"))

#GRAPHICS
label = labs(title = "On Base Percentage by Age & Position", y = "OBP", x = "Player Age", colour = "Positions") 
ggplot(o, aes(age)) +geom_line(aes(y = of_obp, colour = "Outfield"), size = 1.2) + geom_line(aes(y = catcher_obp, colour = "Catcher"), size = 1.2) + geom_line(aes(y = ss_obp, colour = "Shortstop"), size = 1.2) + geom_line(aes(y = first_obp, colour = "First Base"), size = 1.2) + geom_line(aes(y = second_obp, colour = "Second Base"), size = 1.2) + geom_line(aes(y = third_obp, colour = "Third Base"), size = 1.2) + label
early_two = o[c(1:7),]
ggplot(early_two, aes(age)) +geom_line(aes(y = of_obp, colour = "Outfield"), size = 1.2)+ geom_line(aes(y = catcher_obp, colour = "Catcher"), size = 1.2)+ geom_line(aes(y = ss_obp, colour = "Shortstop"), size = 1.2) + geom_line(aes(y = first_obp, colour = "First Base"), size = 1.2) + geom_line(aes(y = second_obp, colour = "Second Base"), size = 1.2) + geom_line(aes(y = third_obp, colour = "Third Base"), size = 1.2) + label