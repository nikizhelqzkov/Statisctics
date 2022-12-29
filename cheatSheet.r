#------------------------------------- R -------------------------------------#

# � - Alt + 0181
# https://cran.r-project.org/doc/contrib/Short-refcard.pdf
# https://towardsdatascience.com/all-probability-distributions-explained-in-six-minutes-fe57b1d49600

# !!! https://willdrevo.com/r-language-cheatsheet !!!
# https://www.stat.umn.edu/geyer/old/5101/rlook.html

#---------------------------------- ������ ----------------------------------#

# install.packages('UsingR') - ����������� �� ������
# library('UsingR') - ��������� �� �����
# load(data) - ��������� �� ����� �� �����
# ?cancer - ���� ���������� ����� ������������ ��������� �� �����

#------------------------------ ������� ������� ------------------------------#

# x = 3 == x <- 3 == x -> 3
# x ->> 3 (����������� � �������� ������)
# 'hello' == print('hello')
# data types : numeric, integer, complex, character, logical
# class(x) == typeof(x)
# sqrt(x), abs(x), ceiling(x), floor(x)
# nchar(x) == length(x)
# grepl('hello', x) - ����� ������ � ���
# paste(x, y) == concatenate(x, y)
# my_function <- function(x) { print(x) } ; my_function()

#                                   ���������
# +, -, *, /, %/% (����������� �������), ^, %% (������� �� �����)
# ==, !=, >, <, >=, <=
# &, &&, |, ||, !
# : (��������� �� ���������������� �� �����), %in% (������� �� ������� � ���������), 
# %*% (��������� �� �������)

#                               ������� � �������
# [4] - ����� 4-��� �������
# [-4] - ����� ������ ����� 4-��� �������
# [2:4] - ����� �� 2-��� �� 4-��� �������
# [-(2:4)] - ����� ������ ����� �� 2-��� �� 4-��� �������
# [c(1, 5)] - ����� �������� 1 � 5
# [x %in% c(1, 2, 5)] - ����� ������� � ���������

# cbind(x) - ������ ������ ��� �������
# rbind(x) - ������ ��� ��� �������
# c(a, ..., z) - ������� ������ �� ���������
# matrix(vector, nrow = n, ncol = m, dimnames = list(c('a', 'b', 'c'))) - ������� �������
# data.frame(x, y, ...) - ��������� �� ������� ��� ��������� (������� �� ������� �� ���� �� �����)
# sort(x), length(x)
# sum(x), cumsum(x), mean(x), cummean(x), median(x)
# quantile(x, probs = ...) - ����� ������� �� ��������, ������������� �� �������� �����������
# �� ������������ �� 0.25 - ����� �������, 0.75 - �������� ������� � �.�.
# sd(x) - ���������� ����������
# summary(x) - ����� ���������� �� �������, ��������, ����, ������� � ��.
# replicate(x, times) - times ���� ���� ����� ����� �� x
# sample(vector, size = ..., replace = T/F, prob = c(...)) - ����� ��������� �� ����������� 

#                               ������������� �� �����

# table(x) - ��������� ������� � ����� � ������� �������
# prop.table(x) - ��������� ���������� �� ����� ������ � ������� ���� ���������
# �� ������ ���������
# plot(x, y, ...) - ������ �����(�������) � ��������
# barplot(x) - ������ ���������� ������� ��������
# boxplot(x) - ������ ��������
# hist(x, probability = T) - ������ ����������
# lines(density(x)) - ������ �����, ����� ������ �� ����������� �� ������������, 
# ������� �� ���� ��� ��� hist ����� �������� probability = T
# qqnorm(x) - ������ ������� (�������� �� x �� ��������� �� ���������� ��������� �������� 
# �������� �����)
# qqlines(x, y) - ������ ����� �� ������� (���������� �� y �� ��������� �� ���������� �� x)

#                                 �������� �� �����

# t.test(x, y, var.equal = T, alternative = 'less') - ����� t-���� �� ������� ����� ����
# prop.test(x, n, p = NULL, alternative = c("two.sided", "less", "greater"), conf.level = 0.95,
#          correct = T) - ������ �� ������� ����� ���������
# shapiro.test(x) - ����� ���� �� �������� ������������� �� ������ �� ������-����, �������� �� 
# �� ����������� ����� ������� � ���������� �������� ���������
# wilcox.test(x, conf.int = T/F, conf.level = 0.96) - ����� ���� �� ���������
# ����������� �� t-test �� ��������� �� ��� ����, �������� �� ������ ������� �� �� ��������
# ������������

#------------------------------- ������������� -------------------------------#

# r{function} - ��������� ���������� (random variates), ���������� �� �������� �������� 
# d{function} - ���������� ���������� (probability density)
# p{function} - ����������� ���������� (cumulative probability)
# q{function} - �������� �� ���������� (quantile)

# � ������:
# ��������� ���������� ������� �� ... -> �������� r{function}
# �������� "����������" ... �� ��������� �������� -> �������� d{function}
# ����� � ������������ ... -> �������� p{function}
# ����� � ���������� �� X% �� ... -> �������� q{function}

# ��������� ������������� #

# ��������� ������������� - ������������ ������� ��������� ���� ��������� � ����� ��������

# ������� ������������� - ������ �������� �� �������� � ��� ������ (�������� �� ������)
# n - ���� ����������, size - ���� �����, prob - ������������ �� ����� ��� ����� ����, 
# x - ������ �� �����, p - ������ �� �����������
# rbinom(n, size, prob)
# dbinom(x, size, prob, log = T/F)  
# pbinom(q, size, prob, lower.tail = T/F, log.p = T/F)  
# qbinom(p, size, prob, lower.tail = T/F, log.p = T/F)

# ��������� ������������� - ��������� ����� ���� ������ ������� ���� �� �� ����� �� ����������
# ����� (��� ����� ��� ��� ���� �� ���, ����� � ������������ �� ��� ��� ����?)
# n - ���� ����������, lambda - ������ ������� �� �����, x - ���� ������, q - ���� ������
# p - ����������
# rpois(n, lambda) 
# dpois(x, lambda, log = T/F) 
# ppois(q, lambda, lower.tail = T/F, log.p = T/F) 
# qpois(p, lambda, lower.tail = T/F, log.p = T/F) 

# ����������� ������������� - ������ ���� ��������� ����� �� ������ �����
# n - ���� �������� �� ������ �����, prob - ������������ �� ����� ��� ����� ����, 
# x - ���� �������� �� ������ �����, q - ���� �������� �� ������ �����, 
# p - ������ �� �����������
# rgeom(n, prob) 
# dgeom(x, prob, log = T/F) 
# pgeom(q, prob, lower.tail = T/F, log.p = T/F) 
# qgeom(p, prob, lower.tail = T/F, log.p = T/F)

# ����������� ������� ������������� - 
# n - ���� ����������, size - ���� ������� �����, prob - ������������ �� ����� ��� ����� ����
# x - ������ �� ��������, q - ������ �� ��������, mu - ������������ �������������� ���� ������ 
# ��������
# rnbinom(n, size, prob, mu) 
# dnbinom(x, size, prob, mu, log = T/F)
# pnbinom(q, size, prob, mu, lower.tail = T/F, log.p = T/F)
# qnbinom(p, size, prob, mu, lower.tail = T/F, log.p = T/F)

# ���������������� ������������� - ������ ������������ �� n ������, k �� �� ... � m �� �� ...
# nn - ���� ����������, m - ���� �� {������/����������/�.�.} ������, 
# n - ���� �� {�������/����������/�.�} ������, k - ���� �� ��������, ����� �� ��������,
# x - ������ �� ����������, q - ������ �� ����������, p - ������������
# rhyper(nn, m, n, k)
# dhyper(x, m, n, k, log = T/F)
# phyper(q, m, n, k, lower.tail = T/F, log.p = T/F)
# qhyper(p, m, n, k, lower.tail = T/F, log.p = T/F)

# ������������ ������������� #

# ������������ ������������� - ������������ ����� �� ������� ����� �������� � ����� �������� 

# �������� (�������) �������������
# n - ���� ���������� (������� �� ���������), mean - ������ ����������� (default = 0), 
# sd - ���������� ���������� (default = 1), x - ������ �� �����, p - ������ �� �����������
# rnorm(n, mean = 0, sd = 1)
# dnorm(x, mean = 0, sd = 1, log = T/F)
# pnorm(q, mean = 0, sd = 1, lower.tail = T/F, log = T/F)
# qnorm(p, mean = 0, sd = 1, lower.tail = T/F, log = T/F)

# E������������� ������������� - �������� ������� �� �����, ����� ������ �� �� ������ ����� 
# ������ ������� �� �� �����
# n - ���� ����������, rate - ������� �� ��������� (default = 1)
# rexp(n, rate = 1)
# dexp(x, rate = 1, log = T/F)
# pexp(q, rate = 1, lower.tail = T/F, log.p = T/F)
# qexp(p, rate = 1, lower.tail = T/F, log.p = T/F)

# ���������� ������������� - ������ ����������, ��� ����� ����� ������� ��� ����� ������� ��
# �� ����� (�������� �� �����)
# n - ���� ����������, min - ����� ������� (default = 0.0), max - ����� ������� (default = 1.0)
# runif(n, min = 0, max = 1)
# dunif(x, min = 0, max = 1, log = T/F)
# punif(q, min = 0, max = 1, lower.tail = T/F, log.p = T/F)
# qunif(p, min = 0, max = 1, lower.tail = T/F, log.p = T/F)

# T-�������������
# t.test(x) - ���� �� �� ����� �������� �������� �� �������� ������������� � �������� ��
# ������ �������, ������� ��� �������� �� ������ �� ��������
# ������ ��������

#-----------------------------------------------------------------------------# 

# ��� �� ���������� �������� �������������:
# I ����� - ���� ����������, ���������� �� �������?, ������� ����� �� ����������
# II ����� - boxplot(), ������ � ���������� ����� ���������
# III ����� - qqnorm(), qqline() - ����� �� �������
# IV ����� - shapirotest(x), ���� x ���� �� �������� �������������

# shapiro -> ������ ���� ��������������� � ��������
# ����-���� ���� 30 ����������, ��������������� �� ����������� -> n > 30 -> t.test()
# ��� �� � -> wilcox.test
# ��� � ��������� ��� �������� -> prop.test
# �������� �� ���������� ������������� -> chisq.test

# pvalue - ������������ �� �����������, ���� ����� ��� ����������� ��� ���������, �� 
# �������� 0 � �����
# alfa = 0.05, pvalue = 0.06, alfa < pvalue -> �� ���������� �������� ��������
# alfa = 0.10, pvalue = 0.06, alfa > pvalue -> ���������� �������� ��������

# �� �� ������������� ������� � �� �� ������� �� ��������� ���� �� �������� ������������
# qqplot - �� ���� ����������
# boxplot - �� ����� ����������

# ����������� ����������� -> cor()
# ����������� ��������:
#   ��������� �����    - barplot(), prop.table(table())
#   ������������ ����� - hist(), lines(density())
# 

#                        ����
# ��������� �����
# ������� �����
# ���������
# ��������� �������� ��������
# ������������ �������� ��������
# ����������� ���������
# �������� �� �������� � ���� �������
# �������� �� �����������
# �� ������� �������
# ������� ��������
# �������� � ����� ����������
# ���������� ��������
# ������������ ������