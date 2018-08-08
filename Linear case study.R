# Import Customer file 
require(readxl)
credit <- read_excel("Linear Regression Case.xlsx")
View(credit)
str(credit)

#descriptive analysis
var_Summ=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3, IQR=iqr,UC1=UC1,LC1=LC1))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)
    prop<-prop.table(table(x))
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}

# create a vraibale Total Spent
credit$total_spent <- credit$cardspent + credit$card2spent

#Numeric Variable
num_var <- sapply(credit, is.numeric)
View(num_var)

#Descriptive Stats
diag_stats<-t(data.frame(apply(credit[num_var], 2, var_Summ)))
View(diag_stats)
write.csv(diag_stats,"diag.csv")

#Missing Values - impute mean values
credit$townsize[is.na(credit$townsize)]<-2.68727490996399
credit$lncreddebt[is.na(credit$lncreddebt)] <- -0.130453521688581
credit$lnothdebt[is.na(credit$lnothdebt)] <- 0.696915259859722
credit$commutetime[is.na(credit$commutetime)] <-25.3455382152861
credit$longten[is.na(credit$longten)] <- 708.871753051831
credit$lnlongten[is.na(credit$lnlongten)] <- 5.61129792063086
credit$cardten[is.na(credit$cardten)] <- 720.478391356543
credit$lncardten[is.na(credit$lncardten)] <- 6.42630873047865

# Large number of missing value
credit$lntollmon[is.na(credit$lntollmon)] <- log(credit$tollmon + 1)
credit$lntollten[is.na(credit$lntollten)] <- log(credit$tollten + 1)
credit$lnequipmon[is.na(credit$lnequipmon)] <- log(credit$equipmon + 1)
credit$lnequipten[is.na(credit$lnequipten)] <- log(credit$equipten + 1)
credit$lncardmon[is.na(credit$lncardmon)] <- log(credit$cardmon + 1)
credit$lncardten[is.na(credit$lncardten)] <- log(credit$cardten + 1)
credit$lnwiremon[is.na(credit$lnwiremon)] <- log(credit$wiremon + 1)
credit$lnwireten[is.na(credit$lnwireten)] <- log(credit$wireten + 1)

#outliers - Lower Limit
credit$age[credit$age < 20] <- 20
credit$ed[credit$ed < 9] <- 9
credit$income[credit$income < 13] <- 13
credit$lninc[credit$lninc < 2.56494935746154] <- 2.56494935746154
credit$debtinc[credit$debtinc < 1.9] <- 1.9
credit$creddebt[credit$creddebt < 0.101088] <- 0.101088
credit$lncreddebt[credit$lncreddebt < -2.29160361221836] <- -2.29160361221836
credit$othdebt[credit$othdebt < 0.2876923] <- 0.2876923
credit$lnothdebt[credit$lnothdebt < -1.24348344030873] <- -1.24348344030873
credit$commutetime[credit$commutetime < 16] <- 16
credit$carditems[credit$carditems < 5] <- 5
credit$cardspent[credit$cardspent < 91.3045] <- 91.3045
credit$card2items[credit$card2items < 1] <- 1
credit$card2spent[credit$card2spent < 14.8195] <- 14.8195
credit$tenure[credit$tenure < 4] <- 4
credit$longmon[credit$longmon < 2.9] <- 2.9
credit$lnlongmon[credit$lnlongmon < 1.06471073699243] <- 1.06471073699243
credit$longten[credit$longten < 12.62] <- 12.62
credit$lnlongten[credit$lnlongten < 2.53527150100047] <- 2.53527150100047
credit$lntollmon[credit$lntollmon < 2.58399755243223] <- 2.58399755243223
credit$lntollten[credit$lntollten < 4.20849009406172] <- 4.20849009406172
credit$lnequipmon[credit$lnequipmon < 3.13983261752775] <- 3.13983261752775
credit$lnequipten[credit$lnequipten < 4.25122986342176] <- 4.25122986342176
credit$lncardmon[credit$lncardmon < 1.98100146886658] <- 1.98100146886658
credit$lncardten[credit$lncardten < 4.0943445622221] <- 4.0943445622221
credit$lnwiremon[credit$lnwiremon < 2.99296416263195] <- 2.99296416263195
credit$lnwireten[credit$lnwireten < 4.11413436573936] <- 4.11413436573936
credit$hourstv[credit$hourstv < 12] <- 12
credit$total_spent[credit$total_spent < 133.106] <- 133.106

#outliers - Upper Limit
credit$age[credit$age > 76] <- 76
credit$ed[credit$ed > 20] <- 20
credit$employ[credit$employ > 31] <- 31
credit$income[credit$income > 147] <- 147
credit$lninc[credit$lninc > 4.99043258677874] <- 4.99043258677874
credit$debtinc[credit$debtinc > 22.2] <- 22.2
credit$creddebt[credit$creddebt > 6.3730104] <- 6.3730104
credit$lncreddebt[credit$lncreddebt > 1.85229733265072] <- 1.85229733265072
credit$othdebt[credit$othdebt > 11.8159808] <- 11.8159808
credit$lnothdebt[credit$lnothdebt > 2.46958637465373] <- 2.46958637465373
credit$spoused[credit$spoused > 18] <- 18
credit$reside[credit$reside > 5] <- 5
credit$pets[credit$pets > 10] <- 10
credit$pets_cats[credit$pets_cats > 2] <- 2
credit$pets_dogs[credit$pets_dogs > 2] <- 2
credit$pets_birds[credit$pets_birds > 1] <- 1
credit$pets_reptiles[credit$pets_reptiles > 0] <- 0
credit$pets_small[credit$pets_small > 1] <- 1
credit$pets_freshfish[credit$pets_freshfish > 8] <- 8
credit$address[credit$address > 40] <- 40
credit$carvalue[credit$carvalue > 72] <- 72
credit$commutetime[credit$commutetime > 35] <- 35
credit$carditems[credit$carditems > 16] <- 16
credit$cardspent[credit$cardspent > 782.3155] <- 782.3155
credit$card2items[credit$card2items > 9] <- 9
credit$card2spent[credit$card2spent > 419.447] <- 419.447
credit$longmon[credit$longmon > 36.7575] <- 36.7575
credit$lnlongmon[credit$lnlongmon > 3.60434189192823] <- 3.60434189192823
credit$longten[credit$longten > 2567.65] <- 2567.65
credit$lnlongten[credit$lnlongten > 7.85074476077837] <- 7.85074476077837
credit$tollmon[credit$tollmon > 43.5] <- 43.5
credit$lntollmon[credit$lntollmon > 3.9269116179219] <- 3.9269116179219
credit$tollten[credit$tollten > 2620.2125] <- 2620.2125
credit$lntollten[credit$lntollten > 8.10664162466756] <- 8.10664162466756
credit$equipmon[credit$equipmon > 49.0525] <- 49.0525
credit$lnequipmon[credit$lnequipmon > 4.06547339321985] <- 4.06547339321985
credit$equipten[credit$equipten > 2600.99] <- 2600.99
credit$lnequipten[credit$lnequipten > 8.11763084060603] <- 8.11763084060603
credit$cardmon[credit$cardmon > 42] <- 42
credit$lncardmon[credit$lncardmon > 3.83945231259331] <- 3.83945231259331
credit$cardten[credit$cardten > 2455.75] <- 2455.75
credit$lncardten[credit$lncardten > 7.92325745199798] <- 7.92325745199798
credit$wiremon[credit$wiremon > 51.305] <- 51.305
credit$lnwiremon[credit$lnwiremon > 4.26728166509711] <- 4.26728166509711
credit$wireten[credit$wireten > 2687.9225] <- 2687.9225
credit$lnwireten[credit$lnwireten > 8.31081668556107] <- 8.31081668556107
credit$hourstv[credit$hourstv > 28] <- 28
credit$total_spent[credit$total_spent > 1145.1465] <- 1145.1465

#taking log of dependent variable to make it uniformily distributed
hist(log(credit$total_spent))
credit$ln_totalspent <- log(credit$total_spent)
View(credit)

#Variable Selection
# anova test
summary(aov(ln_totalspent ~ region+townsize+gender+agecat+birthmonth+edcat+jobcat+union+employ+empcat+retire+inccat+default+jobsat+marital+spousedcat+homeown+hometype+address+addresscat+cars+carown+cartype+carcatvalue+carbought+carbuy+commute+commutecat+commutecar+commutemotorcycle+commutecarpool+commutebus+commuterail+commutepublic+commutebike+commutewalk+commutenonmotor+telecommute+reason+polview+polparty+polcontrib+vote+card+cardtype+cardbenefit+cardfee+cardtenure+cardtenurecat+card2+card2type+card2benefit+card2fee+card2tenure+card2tenurecat+active+bfast+churn+tollfree+equip+callcard+wireless+multline+voice+pager+internet+callid+callwait+forward+confer+ebill+owntv+ownvcr+owndvd+owncd+ownpda+ownpc+ownipod+owngame+ownfax+news+response_01+response_02+response_03
, data = credit))

#Stepwise Analysis
require(MASS)
step <- step(lm(ln_totalspent ~ age+ed+income+lninc+debtinc+creddebt+lncreddebt+othdebt+lnothdebt+spoused+reside+pets+pets_cats+pets_dogs+pets_birds+pets_reptiles+pets_small+pets_saltfish+pets_freshfish+carvalue+commutetime+tenure+longmon+lnlongmon+longten+tollmon+lntollmon+lnlongten+tollten+lntollten+equipmon+lnequipmon+equipten+lnequipten+cardmon+lncardmon+cardten+lncardten+wiremon+lnwiremon++wireten+lnwireten+hourstv+region+gender+agecat+edcat+employ+empcat+retire+inccat+card+card2+internet+ownvcr+owndvd+response_03, data = credit))

#convert categorical variable to factor
credit$region <- as.factor(credit$region)
credit$gender <- as.factor(credit$gender)
credit$edcat <- as.factor(credit$edcat)
credit$response_03 <- as.factor(credit$response_03)
credit$internet <- as.factor(credit$internet)
credit$card <- as.factor(credit$card)
credit$card2 <- as.factor(credit$card2)
credit$owndvd <- as.factor(credit$owndvd)

#Splitting data into Training, Validaton and Testing Dataset
set.seed(1234)

train_ind <- sample(1:nrow(credit), size = floor(0.70 * nrow(credit)))

training_credit <- credit[train_ind,]
testing <- credit[-train_ind,]
View(training_credit)

fit <- lm(ln_totalspent ~ age + lninc + debtinc + lncreddebt + othdebt + 
            pets_birds + cardmon + lncardmon + cardten + lncardten + 
            lnwiremon + region + gender + edcat + card + card2 + internet + 
            owndvd + response_03, data = training_credit)

summary(fit)

#cooks d data set

training_credit$new <- cooks.distance(fit)
training_credit1 <- subset(training_credit,new < 4/3500)
View(training_credit1)

fit1 <- lm(ln_totalspent ~ age + lninc + debtinc + lncreddebt + othdebt + 
             pets_birds + cardmon + lncardmon + cardten + lncardten + 
             lnwiremon + region + gender + edcat + card + card2 + internet + 
             owndvd + response_03, data = training_credit1)

summary(fit1)

training_credit1$new1 <- cooks.distance(fit1)
training_credit2 <- subset(training_credit1,new < 4/3352)
View(training_credit2)

fit2 <- lm(ln_totalspent ~ age + lninc + debtinc + lncreddebt + othdebt + 
             pets_birds + cardmon + lncardmon + cardten + lncardten + 
             lnwiremon + region + gender + edcat + card + card2 + internet + 
             owndvd + response_03, data = training_credit2)

summary(fit2)

# After removing the non significant variables from fit2
final_model <- lm(ln_totalspent ~ age + lninc + 
              cardmon + cardten + region +
               gender + card + card2 + 
             owndvd, data = training_credit2)

summary(final_model)


#######################SCORING USING PREDICT FUNCTION

# Training Dataset
t1<-cbind(training_credit , pred_sales = exp(predict(final_model)))
names(t1)
t1<- transform(t1, APE = abs(pred_sales - ln_totalspent)/ln_totalspent)
mean(t1$APE)
View(t1)

# Testing Dataset
t2<-cbind(testing, pred_sales=exp(predict(final_model,testing)))
t2<- transform(t2, APE = abs(pred_sales - ln_totalspent)/ln_totalspent)
mean(t2$APE)
View(t2)

##################################Decile Analysis Reports - t1(training)

# find the decile locations 
decLocations <- quantile(t1$pred_sales, probs = seq(0.1,0.9,by=0.1))
t1$decile <- findInterval(t1$pred_sales,c(-Inf,decLocations, Inf))
require(sqldf)
t1_DA <- sqldf("select decile, count(decile) as count, avg(pred_sales) as avg_pre_sales,   
               avg(ln_totalspent) as avg_Actual_sales
               from t1
               group by decile
               order by decile desc")
View(t1_DA)
write.csv(t1_DA,"t1_DA.csv")

##################################Decile Analysis Reports - t2(testing)

decLocations <- quantile(t2$pred_sales, probs = seq(0.1,0.9,by=0.1))
t2$decile <- findInterval(t2$pred_sales,c(-Inf,decLocations, Inf))

require(sqldf)
t2_DA <- sqldf("select decile, count(decile) as count, avg(pred_sales) as avg_pre_sales,   
               avg(ln_totalspent) as avg_Actual_sales
               from t2
               group by decile
               order by decile desc")

View(t2_DA)
write.csv(t2_DA,"t2_DA.csv")

#######################################################################