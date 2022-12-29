library("MASS")


#1
#a)
sum(Aids2$age<20)

#b)
i = order(Aids2$diag)
Aids2[i[1:5],2]
#samo muzhe


#v)
men = Aids2[Aids2$sex=='M',]
sum(men$T.categ=='blood')/length(men$T.categ)


#g)
barplot(prop.table(table(Aids2$status,Aids2$state),2),legend=T)

#2)
women =Aids2[Aids2$sex=='F',]
lw  = length(women[,women$status])
lm  = length(men[,men$status])
pw  = table(women$status=="D")[2]
pm = table(men$status=="D")[2]
prop.test(c(pw,lw),c(pm,lm),alternative = "less")
#pvalue>0.05 => Ne othurlqme hipotezata => Ne e vqrno che smurtnostta pri jeni e < ot tazi na muzhe



#3

x = Aids2[Aids2$status == 'D',];
y = (x$death - x$diag)/365 + x$age


shapiro.test(y)
#ne sa normalni => wilcox.test

wilcox.test(y,mu=38)
#pvalue>0.05 => Priemame H0 che srednoto e 38




#4)


x = rchisq(100,10)

hist(x,probability = T)
curve(dchisq(x,df=10),add = T)