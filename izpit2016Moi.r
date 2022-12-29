f  = function(x,n){
  i = (1:n)
  sum(x^i/i)
  
}
n = 5
x = seq(2.3,10.1,by=0.2)
xl = length(x)
nl = rep(c(2,3,5),length.out = xl)

inx = (1:xl)
f2 = function(x,n){
  x = f(x,n)
}
f3 = function(x,n){
  for (i in 1:length(x)) {
    x[i] = f2(x[i],n)
  }
  x
}
s1 = sum(f3(x,n))
s2  = sum(f3(nl,n))
s  = s1 + s2
s
#-----------------------------
#2
#a)
data = data.frame(state.x77)
i = order(data$Illiteracy)
name = rownames(data)
badIlliteracyTown = name[i[1:5]]
badIlliteracyTown
#b)
i = order(data$Area,decreasing = T)
sumOfPop = sum(data[i[1:10],1])
sumOfPop

#v)
v = sum(data$Life.Exp>70)
v

#g) 
g = name[which.max(data$Population/data$Area)]
g

#--------------
#3)

#Ho : s negramotnost ot 1% e murder = s negramotnost ot >=1% murder
#Ho : s negramotnost ot 1% e murder< s negramotnost ot >=1% murder

d  = data[data$Illiteracy<1,]
d1M = d$Murder
d2  = data[data$Illiteracy>=1,]
d2M = d2$Murder
d1M
d2M
shapiro.test(d1M)#normalno e
shapiro.test(d2M)#normalno e

t.test(d1M,d2M,alternative = "less")
# mojem da othuvrim H0 => e вярно che  в щатите с неграмотност под 1% стават по-малко убийства?


#--------------
#4
data =data.frame(x3 = anscombe$x3, y3 = anscombe$y3)
plot(data)
outlierRowIdx = which(data$y3>9)
newData = data[-outlierRowIdx,]
plot(newData)
boxplot(newData)

cor(data$x3,data$y3)
cor(newData$x3,newData$y3)


model = lm(data$x3 ~ data$y3);
sm   = summary(model);
sm
model = lm(newData$x3 ~ newData$y3);
sm   = summary(model);
sm

#------------------


#5)
#h0 : ravni sa
#h1 : pri jenite e po efektivno

w = 100
m = 200
wp = 62
mp = 100
prop.test(c(wp,w),c(mp,m),alternative = "greater",conf.level = 0.99)
#0.03> 0.01 -> ne othvurlqme H0 => ima ednakuv efekt


#3.2
data = data.frame(state.x77)

d2  = data[data$Illiteracy>=1,]
d2M = d2$Murder
shapiro.test(d2M)
t.test(d2M,conf.level = 0.9)


#4.2
data = data.frame(x2=anscombe$x2, y2=anscombe$y2)
l2 = lm(data$y2 ~ log(data$x2))
s = summary(l2)
plot(l2)
#points(data$x2, data$y2)
#plot(c(data$x3, data$y3))
abline(l2)
#smqteme korelaciqta na x2 i y2
#ako cor blizo do 1 => imat lineina vruzka
#ako cor blizo do 0 => nqmat lineina vruzka
cor(data$y2, data$x2)


