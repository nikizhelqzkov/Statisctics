

#ex1

denyZeroHyp = function(n){
  t = table(sample(1:6,size = n,replace = T))
  #Smqtame dali othvurlq H0
  result = chisq.test(t)$p.value<0.05
  result

}
#Gledame kakuv procent ot ot vsichki othvurlqt H0

f1 = function(n){
  s = sum(replicate(10000,denyZeroHyp(n)))/10000*100
  result = paste(s, "%")
  result
}
f1(100)
f1(200)
f1(400)

#ex2
checkNormality = function(n) {
    x = runif(n, 5, 12)
    hypResults = shapiro.test(x)$p.value > 0.05
    hypResults
}

f2 = function(nBig,n){
  results = replicate(n=nBig,checkNormality(n))
  prop.table(table(results))
}
f2(10000,15)
f2(10000,25)
f2(10000,50)
f2(10000,90)


#ex3

f3 = function(n){
  beta1 = 5
  # 4 реда - по 1 за всяка подточка
  beta1_est =  matrix(data = NA, nrow = 4, ncol = 10000)
  beta1_sd_err = matrix(data = NA, nrow = 4, ncol = 10000)
  beta1_sd_err = matrix(data = NA, nrow = 4, ncol = 10000)
  leftConfInt = matrix(data = NA, nrow = 4, ncol = 10000)
  rightConfInt = matrix(data = NA, nrow = 4, ncol = 10000)
  
  for (k in 1:10000) {
    x = sample(1:10, size = n, replace = TRUE)
    
    # а)
    e = rnorm(n, mean = 0, sd = 2)
    y = beta1 * x + e
    
    df = data.frame(x, y)
    model = lm(y ~ x, data = df)
    model_summary = summary(model)
    p = paste("a)")
    print(p)
    print(confint(model))
    print("----------")
    print(model_summary)
    beta1_est[1, k] = model_summary$coefficients[2, 1] # бета1 шапка
    beta1_sd_err[1, k] = model_summary$coefficients[2, 2]
    leftConfInt[1,k] = confint(model)[2,1]
    rightConfInt[1,k] = confint(model)[2,2]
    
    # б)
    e = runif(n, -3.5, 3.5)
    y = beta1 * x + e
    
    df = data.frame(x, y)
    model = lm(y ~ x, data = df)
    model_summary = summary(model)
    p = paste("b)")
    beta1_est[2, k] = model_summary$coefficients[2, 1] # бета1 шапка
    beta1_sd_err[2, k] = model_summary$coefficients[2, 2]
    leftConfInt[2,k] = confint(model)[2,1]
    rightConfInt[2,k] = confint(model)[2,2]
    
    # в)
    v = rexp(n, 0.7)
    w = rexp(n, 0.7)
    e = v - w
    y = beta1 * x + e
    
    df = data.frame(x, y)
    model = lm(y ~ x, data = df)
    model_summary = summary(model)
    beta1_est[3, k] = model_summary$coefficients[2, 1] # бета1 шапка
    beta1_sd_err[3, k] = model_summary$coefficients[2, 2]
    leftConfInt[3,k] = confint(model)[2,1]
    rightConfInt[3,k] = confint(model)[2,2]
    
    # г)
    u = rexp(n, 0.5)
    e = u - 2
    y = beta1 * x + e
    
    df = data.frame(x, y)
    model = lm(y ~ x, data = df)
    model_summary = summary(model)
 
    beta1_est[4, k] = model_summary$coefficients[2, 1] # бета1 шапка
    beta1_sd_err[4, k] = model_summary$coefficients[2, 2]
    leftConfInt[4,k] = confint(model)[2,1]
    rightConfInt[4,k] = confint(model)[2,2]
  }
  
  tbl = matrix(data = NA, nrow = 4, ncol = 3)
  rownames(tbl) = c("а)", "б)","в)","г)")
  colnames(tbl) = c("Колко често доверителният интервал за бета1 съдържа истинската стойност?",
                    "средната дължина на доверителния интервал на базата на 10000 повторения",
                    "средното на beta1_est на базата на 10000 повторения")
  for (k in 1:4) {
    tbl[k, 1] = paste(sum((beta1 > leftConfInt[k,]) & (beta1< rightConfInt[k,])) / 10000 * 100, "%") 
    tbl[k, 2] = mean(rightConfInt[k,] - leftConfInt[k,]) # (-beta1_sd_err , +beta1_sd_err)
    tbl[k, 3] = mean(beta1_est[k,]) 
  }
  n = paste("N =",n)
  print(n)
  graphics(beta1_est)
  print(tbl)
  
}
graphics = function(beta1_est){
  hist(beta1_est)
  qqnorm(beta1_est)
}
ns = c(30,50, 100, 500)
for (n in ns) {
  f3(n)
}

