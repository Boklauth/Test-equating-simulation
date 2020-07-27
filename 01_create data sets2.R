




######### notes #############
# adapted some parameters (a, b) from Fitzpatrick, Joseph, Skorupski, William P, 2016
# for beginning the simulation
# coding names for test equating are based on Kolen and Brennan (2014)
# Form X is the old form.
# Form Y is the new form.
# Form V is the anchor test existed in both forms.


# coding overview ####
# 1-generate theta groups ####
# 2-Form X parameters ####
# 3-Form Y parameters ####
# 4-Form V parameters ####
# 5-Preparing data sets for equating #### 




# set seed
seed_value<-348000
set.seed(seed_value)


# 1-generate theta groups #####
# must run set.seed together with random number generation

# sample size 
n<-10000

# thata1, a group of examinees who took Form X #####

theta1<-rnorm(n, 0, 1)
# checking mean and sd
mean(theta1)
sd(theta1)

# thata2, a group of examinees who took Form Y #####
# Form Y is a new form

theta2<-rnorm(n, .25, 1)
# checking mean and sd
mean(theta2)
sd(theta2)

# thata3, a group of examinees who took Form Y #####

theta3<-rnorm(n, 0.8, 1)
# checking mean and sd
mean(theta3)
sd(theta3)

# 2-Form X parameters #####

# choose test form length
formlength = 60
# formlength<-40


# don't sort item discrimination, but let the level of discrimination...
# ...distribute randomly on test items

# generate item difficulty for Form X
# from a standard normal distribution
# aim for item diff that > -2



# ax and bx need to have some correlation
# based on KB36 in SNSequate package, 2PL model gives 
# corr bwn. a & b = -0.58 and -0.4 for Form X and Y, respectively
# corr bwn. a $ intercept = 0.41 and 0.41

# item disc. for for x in KB36
# min = 0.3086857, max = 1.624367, mean = 0.921305, sd = 0.3246572

# intercept for form x
# min = -2.767308, max = 1.824331, mean = -0.1111229, sd = 1.148457

# item diff. for form x in KB36
# min= -2.102507, max = 6.294024, mean = 0.626234, sd= 1.642625


# we should be in control of creating item diff. 
# hence, create item diff. first. 
# use item diff. with min= -2.102507, max = 6.294024
# it has mean = 0.626234, sd= 1.642625
seed_value<-123456
set.seed(seed_value)
bx<-rep(0, formlength)
k<-0 # set index
m = 0.1539 # mean
std = 1 # standard deviation
while (k < formlength){
  temp_value<-rnorm(1, m, std)
  if (temp_value > -2 & temp_value < 3){
    k = k+1
    bx[k]<-temp_value
  } 
}
sort(bx)
hist(bx, prob=TRUE)
min(bx)
max(bx)
mean(bx)
sd(bx)

# creating item disc. for Form X 
# based on KB36 in SNSequate package, 2PL model gives 
# corr bwn. a & b = -0.58 and -0.4 for Form X and Y, respectively
# corr bwn. a $ intercept = 0.41 and 0.41

# item disc. for for x in KB36
# min = 0.3086857, max = 1.624367, mean = 0.921305, sd = 0.3246572

#seed_value<-12345 good one
seed_value<-348
set.seed(seed_value)
rho = -0.58
ax<-rep(0, formlength)
k=0
while (k < formlength) {
  temp_value <-rnorm(1, rho * bx, sqrt(1 - rho^2))
  if (temp_value > 0.307 & temp_value<1.624367){
    k <- k+1
    ax[k] <- temp_value
  }
}
#ax
hist(ax, prob=TRUE)
min(ax)
max(ax)
mean(ax)
sd(ax)

# theta1 who took Form X #####

xpop1<-bi.irt(formlength=formlength, n=n, a=ax, b=bx, theta=theta1)
xpop1.responses<-xpop1$item_responses

# frequency of total score
xpop1.total<-as.data.frame(table(xpop1.responses$total))
#View(xpop1.total)
par(mfrow=c(1,1))
hist(xpop1.responses$total, prob=FALSE)
library(moments)
skewness(xpop1.responses$total)
kurtosis(xpop1.responses$total)


# 3-Form Y parameters  ####
# item parameters for Form Y from KB36 in the SNSequate package

# mean diff. for form y in KB36
# min = -1.830765, max = 2.731795, mean = -0.0006748479, sd = 1.170816

# intercept (beta0) for Form Y
# min = -1.966786, max = 2.348743, mean = 0.1388044, sd = 1.022047

# generate item difficulty for Form Y
seed_value<-348000
set.seed(seed_value)
by<-rep(0, formlength)
k<-0 # set index
m = -0.20465 # mean
std = 1.2455 # standard deviation
while (k < formlength){
  temp_value<-rnorm(1, m, std)
  if (temp_value > -1.830 & temp_value < 2.778){
    k = k+1
    by[k]<-temp_value
  } 
}
sort(by)
hist(by, prob=TRUE)
min(by)
max(by)
mean(by)
sd(by)

# item disc. for Form Y

# based on KB36 in SNSequate package, 2PL model gives 
# corr bwn. a & b = -0.58 and -0.4 for Form X and Y, respectively
# corr bwn. a $ intercept = 0.41 and 0.41
# item disc. for Form Y in KB36
# min = 0.3083478, max = 1.778345, mean = 1.014896, sd = 0.301668

seed_value<-123456
set.seed(seed_value)
rho = -0.4
ay<-rep(0, formlength)
k=0
while (k < formlength) {
  temp_value <-rnorm(1, rho * by, sqrt(1 - rho^2))
  if (temp_value > 0.308 & temp_value < 1.778){
    k <- k+1
    ay[k] <- temp_value
  }
}
ay
hist(ay, prob=TRUE)
min(ay)
max(ay)
mean(ay)
sd(ay)

# theta2 who took Form Y #####

ypop2<-bi.irt(formlength=formlength, n=n, a=ay, b=by, theta=theta2)
ypop2.responses<-ypop2$item_responses
# frequency of total score
ypop2.total<-as.data.frame(table(ypop2.responses$total))
#View(ypop2.total)
hist(ypop2.responses$total, prob=FALSE)
skewness(ypop2.responses$total)
kurtosis(ypop2.responses$total)

# 4-Form V parameters #####
# form V is an external anchor test
# the type of anchor test is miditest.

# item difficulty for both forms

# item difficulty for midi external anchor test

b.anchor.bank<-as.data.frame(cbind(sort(bx), sort(by)))
b.anchor.bank$b.avg<-(b.anchor.bank[,1]+b.anchor.bank[,2])/2
# the goal is to be representative of both forms
# and difficulty is within the range of both forms

# method: select every 3th item of b.avg
# abtrim: trim both ends of item difficulty
# select every third item within the range 3 to length(bx) exclusive
bv<-b.anchor.bank$b.avg[seq(3, length(bx)-1, by = 3)]
bv
min(bv)
max(bv)
# not perfect but it meets the definition of miditest

# check the length of anchor test
length(bv)
# 19 items in the anchor test
# percentage
round(100*length(bv)/formlength)
# 32%

# create item discrimination
# follow Form X paramters
# based on KB36 in SNSequate package, 2PL model gives 
# corr bwn. a & b = -0.58 and -0.4 for Form X and Y, respectively
# corr bwn. a $ intercept = 0.41 and 0.41

# item disc. for for x in KB36
# min = 0.3086857, max = 1.624367, mean = 0.921305, sd = 0.3246572
# use mean corr as well 

seed_value<-348000
set.seed(seed_value)
rho = (-0.58+-0.4)/2
av<-rep(0, length(bv))
k=0
while (k < length(bv)) {
  temp_value <-rnorm(1, rho * bv, sqrt(1 - rho^2))
  if (temp_value > 0.308 & temp_value < 1.624){
    k <- k+1
    av[k] <- temp_value
  }
}
av
hist(av, prob=TRUE)
min(av)
max(av)
mean(av)
sd(av)

# theta1 who took Form V (anchor test) #####

vpop1<-bi.irt(formlength=length(bv), n=n, a=av, b=bv, theta=theta1)
vpop1.responses<-vpop1$item_responses
# frequency of total score
vpop1.total<-as.data.frame(table(vpop1.responses$total))

hist(vpop1.responses$total, prob=FALSE)
skewness(vpop1.responses$total)
kurtosis(vpop1.responses$total)


# theta2 who took Form V (anchor test) #####

vpop2<-bi.irt(formlength=length(bv), n=n, a=av, b=bv, theta=theta2)
vpop2.responses<-vpop2$item_responses
# frequency of total score
vpop2.total<-as.data.frame(table(vpop2.responses$total))
#View(vpop2.total)
hist(vpop2.responses$total, prob=FALSE)
skewness(vpop2.responses$total)
kurtosis(vpop2.responses$total)

# theta3 who took Form Y #####

ypop3<-bi.irt(formlength=formlength, n=n, a=ay, b=by, theta=theta3)
ypop3.responses<-ypop3$item_responses
# frequency of total score
ypop3.total<-as.data.frame(table(ypop3.responses$total))
#View(ypop3.total)
hist(ypop3.responses$total, prob=FALSE)

# 5-Preparing data sets for equating ##### 
# Form X theta1 data #####
# four line of base R code can get the desired dataset 
# data sets are merged without unique id
# but the end results are the same if sorting is not involved


#base.form<-xpop1.responses['total'] 
#anchor<-vpop1.responses$total
#base.form$anchor<-anchor
#View(base.form)


# another way
# get theta and total scores from the anchor test
anchor<-vpop1.responses %>% 
  select(theta, total) %>% 
  rename(anchor=total)

# get theta and total scores from Form X
formx<-xpop1.responses %>% 
  select(theta, total)

# merge both tests by theta
baseform<-full_join(formx, anchor, by='theta') %>% 
  select(total, anchor)
#View(baseform)


# Form Y theta2 data #####
anchor<-vpop2.responses %>% 
  select(theta, total) %>% 
  rename(anchor=total)

# get theta and total scores from Form X
formytheta2<-ypop2.responses %>% 
  select(theta, total)

# merge both tests by theta
newform.t2<-full_join(formytheta2, anchor, by='theta') %>% 
  select(total, anchor)
#View(newform.t2)

# Form Y theta3 data #####
anchor<-vpop3.responses %>% 
  select(theta, total) %>% 
  rename(anchor=total)

# get theta and total scores from Form X
formytheta3<-ypop3.responses %>% 
  select(theta, total)

# merge both tests by theta
newform.t3<-full_join(formytheta3, anchor, by='theta') %>% 
  select(total, anchor)
#View(newform.t3)
df<-list('x1'= baseform, 'y2'=newform.t2, 'y3'=newform.t3)

