library(fpp3)
library(urca)
pacman::p_load(readxl)
temp<-read_excel("jobopening.xls")
temp
jobopening=temp%>%mutate(date=yearmonth(date))%>%as_tsibble(index = date)
jobopening
#b
jobopeninghold=jobopening%>%filter_index("2000 Dec"~"2021 May")
jobopeninghold
lambda=jobopeninghold%>%features(JOR,features = guerrero)%>%pull(lambda_guerrero)

jobopeninghold%>%autoplot(JOR)
jobopeninghold%>%autoplot(box_cox(JOR,lambda))
# not able to see much difference after transformation.so We are going with the data without
#transformation

#c

jobopeninghold%>%features(JOR,unitroot_nsdiffs)
jobopeninghold%>%features(difference(JOR),unitroot_nsdiffs)
#D=0 no seasonal difference needed

transformTS=as.ts(jobopeninghold%>%select(JOR))
plot(transformTS)
#we can see a grow period after period except the outlier around 2008-2010,going with trend

#d
summary(ur.df(transformTS,type="trend",lags = 30,selectlags = "AIC"))
#test statistic is compared to the tau3 critical values
#failed to reject null hypothesis,critical value<test statistic.can't rule out
#a unit root,remdy d=1
#d=1 and D=0

#E
jobopeninghold%>%gg_tsdisplay(difference(JOR),
                                plot_type = "partial",lag=87)
#ACF has more statistically significant coeificient than PACF has,meaning non seasonal AR 
# more important than non seasonal MA.so q needs to set as small as posible,q=0 .
#In ACF there are a lot of imporatnt lags.But in PACF the first 2 lags are much 
#important than other lags.The highest one occuring at 2 so p=2
#In acf there are a lot of seasonal lag but in PACF after 60 seasonal lags are
#slowly shutting down.concludes that seasonal MA is less important than seasonal AR
#therefore Q need to set as small as posiible,Q=0.In PACF there are significant coeificient
# throgh lag 60(5th season)so going with P=5
#our intial guess for SARIMA terms are pdq(2,1,0)+PDQ(5,0,0)

#p=2 d=1 q=0
#P=5 D=0 Q=0
report(jobopeninghold%>%model(ARIMA(JOR~0+pdq(2,1,0)+PDQ(5,0,0)))) 
report(jobopeninghold%>%model(ARIMA(JOR~0+pdq(2,1,0)+PDQ(1,0,0))))
report(jobopeninghold%>%model(ARIMA(JOR~0+pdq(2,1,0)+PDQ(2,0,0))))
report(jobopeninghold%>%model(ARIMA(JOR~0+pdq(1,1,0)+PDQ(2,0,1))))
report(jobopeninghold%>%model(ARIMA(JOR~0+pdq(2,1,0)+PDQ(2,0,1))))#best model

Modelfinal=jobopeninghold%>%model(ARIMA(JOR~0+pdq(2,1,0)+PDQ(2,0,1)))
Modelfinal%>%gg_tsresiduals(lag=30)
augment(Modelfinal)%>%features(.innov,ljung_box,lag=30,dof=5)
qchisq(.95,25)


#here resulting 95% critical value for a chi2(25) distribution is 37.65248
#36.8<37.65248(our test statistic is less than associated critical value).Also our p value is 
#not<0.05 .based on 5% test size we failed to rejcect the null.
# most of the auto correlations are well with in the confidence bands except 1
#all this indicates that our model is appropriate

#F
Modelfinal%>%forecast(h=4)%>%
    autoplot(filter_index(jobopening,"Dec 2019"~"Sep 2021"))

#g-second model

jobopeninghold%>%gg_season(JOR)
jobopeninghold%>%gg_subseries(JOR)
#there is no zeros or -ve values so error=M
#the plot of JOR is non linear so we use trend=M 
#seasonality varying all the time,season=M
modelETS=jobopeninghold%>%model(MMM=ETS(JOR~error("M")+trend("M")+season("M")))
report(modelETS)

modelETS%>%forecast(h=4)%>%
  autoplot(filter_index(jobopening,"Dec 2019"~"Sep 2021"))

#SARIMA model is better than exponential smoothing.because forecast values in sarima
#model is more closer to actual value than Exponential smoothing model
#h
Modelfinal%>%forecast(h=6)%>%
  autoplot(filter_index(jobopening,"Dec 2019"~"Nov 2021"))

#additional notes
#comparison b/w forecast values an actual values of our SARIMA and Exponential smoothing model
fit=jobopeninghold%>%model(SARIMA210x201=ARIMA(JOR~0+pdq(2,1,0)+PDQ(2,0,1)))
fit%>%select(SARIMA210x201)%>%forecast(h=4)
foreFINAL=fit%>%select(SARIMA210x201)%>%forecast(h=4)
compare=filter_index(jobopening,"2021 Jun"~"2021 sep")%>%
  mutate(FORE=foreFINAL$.mean)
compare
foreFINAL2=modelETS%>%select(MMM)%>%forecast(h=4)
compare2=filter_index(jobopening,"2021 Jun"~"2021 Sep")%>%
  mutate(FORE=foreFINAL2$.mean)
compare2

