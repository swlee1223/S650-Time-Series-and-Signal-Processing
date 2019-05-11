## reading data
setwd('C:\\Users\\swlee\\OneDrive\\Desktop\\2019Spr_MSDS\\Time Series\\youtube-5000-channels-videos-daily-count-every-3h')
data<-read.csv('data.csv', header =T)


## centering to zero to meet the assumption (E(X) has to equal to zero)
likecount<-data$likecount - mean(data$likecount)
dislikecount<-data$dislikecount -  mean(data$dislikecount)
cmtcount<-data$cmtcount -  mean(data$cmtcount)
subcount<-data$subcount -  mean(data$subcount)
viewcount<-data$viewcount - mean(data$viewcount)
t<-data$t

## plotting time series data
par(mfrow=c(2,3))
plot(subcount~t, type='l', main='Total Subscribe Counts per time')
plot(likecount~t, type='l', main='Total Like Counts per time')
plot(dislikecount~t, type='l', main='Total Dislike Counts per time')
plot(cmtcount~t, type='l', main='Total Comment Counts per time')
plot(viewcount~t, type='l', main='Total View Counts per time')

likecount<-diff(data$likecount)
dislikecount<-diff(data$dislikecount)
cmtcount<-diff(data$cmtcount)
subcount<-diff(data$subcount)
viewcount<-diff(data$viewcount)
t<-t[-1]

## plotting time series data (diff)
par(mfrow=c(2,3))
plot(subcount~t, type='l', main='Total Subscribe Counts per time')
plot(likecount~t, type='l', main='Total Like Counts per time')
plot(dislikecount~t, type='l', main='Total Dislike Counts per time')
plot(cmtcount~t, type='l', main='Total Comment Counts per time')
plot(viewcount~t, type='l', main='Total View Counts per time')


## plotting autocovariance
par(mfrow=c(2,3))
acf(subcount, main='Subscribe (y) Auto-correlations')
acf(likecount, main='Like Count (x1) Auto-correlations')
acf(dislikecount, main='Dislike Count (x2) Auto-correlations')
acf(cmtcount, main='Comment Count (x3) Auto-correlations')
acf(viewcount, main='Viewcount (x4) Auto-correlations')


autocov<-matrix(NA, nrow=25, ncol=5)
autocov[,1]<-as.vector(acf(subcount, plot = FALSE)$acf)
autocov[,2]<-as.vector(acf(likecount, plot = FALSE)$acf)
autocov[,3]<-as.vector(acf(dislikecount, plot = FALSE)$acf)
autocov[,4]<-as.vector(acf(cmtcount, plot = FALSE)$acf)
autocov[,5]<-as.vector(acf(viewcount, plot = FALSE)$acf)
write.csv(as.data.frame(autocov), file='autocovmat.csv' ,row.names=F)


par(mfrow=c(2,2))
ccf(likecount, subcount,  main = 'Cross-Covariance of \nSubscribe (y) and Like(x1)')
ccf(dislikecount, subcount, main='Cross-Covariance of \nSubscribe (y) and Disike (x2)')
ccf(cmtcount, subcount,  main = 'Cross-Covariance of \nSubscribe (y) and Comment (x3)')
ccf(viewcount, subcount,  main = 'Cross-Covariance of \nSubscribe (y) and Viewcount (x4)')

ccfcov<-matrix(NA, nrow=43, ncol=4)
ccfcov[,1]<-as.vector(ccf(likecount, subcount, plot=F)$acf)
ccfcov[,2]<-as.vector(ccf(dislikecount, subcount, plot=F)$acf)
ccfcov[,3]<-as.vector(ccf(cmtcount, subcount,   plot=F)$acf)
ccfcov[,4]<-as.vector(ccf(viewcount, subcount,   plot=F)$acf)

write.csv(as.data.frame(ccfcov), file='ccfmat.csv' ,row.names=F)


## Spectral Analysis for seasonal term
par(mfrow=c(2,3))
spec.sub<-spec.pgram(subcount,spans=10,taper=0,log="no")
spec.like<-spec.pgram(likecount,spans=10,taper=0,log="no")
spec.dislike<-spec.pgram(dislikecount,spans=10,taper=0,log="no")
spec.cmt<-spec.pgram(cmtcount, spans=10, taper=0, log='no')
spec.view<-spec.pgram(viewcount, spans=10, taper=0, log='no')


## There seems like seasonal (daily) pattern in every 8 time period, Which is obvious because there is pattern of sleeping and being awake.
subspec<-cbind(spec.sub$freq, spec.sub$spec)
likespec<-cbind(spec.like$freq, spec.like$spec)
dislikespec<-cbind(spec.dislike$freq, spec.dislike$spec)
cmtspec<-cbind(spec.cmt$freq, spec.cmt$spec)
viewspec<-cbind(spec.view$freq, spec.view$spec)


write.csv(subspec, 'subspec.csv')

library(aTSA)
adf.test(subcount, nlag=8)
adf.test(likecount, nlag = 8)
adf.test(dislikecount, nlag=8)
adf.test(cmtcount, nlag=8)
adf.test(viewcount, nlag=8)

## Take 8th difference for 8 lagged term
subdiff<-diff(subcount, 8)
par(mfrow=c(1,1))
acf(subdiff, lag.max=40)
pacf(subdiff, lag.max=40, main='Series sub_8 (y)')
spec.pgram(subdiff, spans=10,taper=0,log="no")

## seasonal AR(3)
## non-seasonal AR(2 or 1)
likediff<-diff(likecount, 8)
par(mfrow=c(1,1))
acf(likediff, lag.max=40)
pacf(likediff, lag.max=40, main='Series like_8 (x1)')

## seasonal AR(2)
## non-seasonal AR(0 or 1)

dislikediff<-diff(dislikecount, 8)
par(mfrow=c(1,1))
acf(dislikediff, lag.max=40)
pacf(dislikediff, lag.max=40)

## seasonal AR(2)
## non-seasonal AR(0 or 2)

dislikediff<-diff(dislikecount, 8)
par(mfrow=c(1,1))
acf(dislikediff, lag.max=40)
pacf(dislikediff, lag.max=40)

cmtdiff<-diff(cmtcount, 8)
par(mfrow=c(1,1))
acf(cmtdiff, lag.max=40)
pacf(cmtdiff, lag.max=40)

## seasonal AR(3)
## non-seasonal AR(0 or 1)

viewdiff<-diff(viewcount, 8)
par(mfrow=c(1,1))
acf(viewdiff, lag.max=40)
pacf(viewdiff, lag.max=40)

## seasonal AR(3)
## non-seasonal AR(2 or 1)

## making lag terms for model-fitting
B = c(0,1)
like_1 = filter(likecount,B,sides=1)
dislike_1 = filter(dislikecount, B, sides=1)
sub_1 = filter(subcount,B,sides=1)
cmt_1 = filter(cmtcount,B,sides=1)
view_1 = filter(viewcount,B,sides=1)

B = c(0,0,1)
like_2 = filter(likecount,B,sides=1)
dislike_2 = filter(dislikecount, B, sides=1)
sub_2 = filter(subcount,B,sides=1)
cmt_2 = filter(cmtcount,B,sides=1)
view_2 = filter(viewcount,B,sides=1)

N = length(subcount)
rm(B)

like_1 = like_1[3:N]
like_2 = like_2[3:N]
dislike_1 = dislike_1[3:N]
dislike_2 = dislike_2[3:N]
view_1 = view_1[3:N]
view_2 = view_2[3:N]
cmt_1 = cmt_1[3:N]
cmt_2 = cmt_2[3:N]

subcount<-subcount[3:N]
likecount<-likecount[3:N]
dislikecount<-dislikecount[3:N]
cmtcount<-cmtcount[3:N]
viewcount<-viewcount[3:N]




N = N-2
N1 = N - 50
N2 = seq(N1+1,N)

X1 = cbind(likecount, like_1, like_2, dislikecount, dislike_1, dislike_2, viewcount, view_1, view_2, cmtcount, cmt_1, cmt_2)

subf = subcount[N2]
X1f = X1[N2,]
subt = subcount[1:N1]
X1t = X1[1:N1,]

## fitting arimax model

# simplist
res1 = arima(subt, xreg=X1t, order=c(1,0,1), seasonal = list(order=c(2,1,0), period=8))
res1

# AR(2)
res2 = arima(subt, xreg=X1t, order=c(2,0,1), seasonal = list(order=c(2,1,0), period=8))
res2

# SMA(1)
res3 = arima(subt, xreg=X1t, order=c(1,0,1), seasonal = list(order=c(2,1,1), period=8))
res3

# SMA(2)
res4 =arima(subt, xreg=X1t, order=c(1,0,1), seasonal = list(order=c(2,1,2), period=8))
res4

# increase everything by 1 except moving SMA
res5 =arima(subt, xreg=X1t, order=c(2,1,2), seasonal = list(order=c(2,2,2), period=8))
res5






AICval<-c(AIC(res1), AIC(res2), AIC(res3), AIC(res4), AIC(res5))
BICval<-c(BIC(res1), BIC(res2), BIC(res3), BIC(res4), BIC(res5))
loglikli<-c(-2*res1$loglik + 9*log(N), -2*res2$loglik + 9*log(N), -2*res3$loglik + 9*log(N), -2*res4$loglik + 9*log(N), -2*res5$loglik + 9*log(N))

testresults<-cbind(AICval, BICval, loglikli)
rownames(testresults)<-c("res1", "res2", "res3", "res4", "res5")
testresults

write.csv(testresults, 'testresults.csv')
write.csv(res5$coef, file='res5coef.csv')
## comparing AIC, BIC, loglikelihood values, res5 seems to be the best model among the 4 models

hRes5 = residuals(res5)
plot(hRes5)
acf(hRes5, main="output")

par(mfrow=c(1,1))
Box.test(hRes5,lag=20)
spec.pgram(hRes5, spans= 10, taper=0, log='no')

spectrum(hRes5,log='no')
coef(res5)
res5

yh=predict(res5,newxreg=X1f)
yhp = yh$pred
ts.plot(subf, yhp, lty=1:2)



yh=predict(res4,newxreg=X1f)
yhp = yh$pred
ts.plot(subf, yhp, lty=1:2)

###########################################################################################################
#                                            ....Etc....                                                  #
###########################################################################################################
# plot(data)
# plot(subcount~likecount)
# plot(subcount~dislikecount)
# plot(likecount~dislikecount)
# plot(viewcount~likecount)
# plot(cmtcount~likecount)
# 
# 
# # Plotting for entire lags for each variable
# par(mfrow=c(2,3))
# acf(subcount, main='Subscribe (y) Auto-correlations', lag.max=length(subcount))
# acf(likecount, main='Like Count (x) Auto-correlations', lag.max=length(likecount))
# acf(dislikecount, main='Dislike Count (x) Auto-correlations', lag.max=length(dislikecount))
# acf(viewcount, main='Viewcount (x) Auto-correlations', lag.max=length(viewcount))
# acf(cmtcount, main='Comment Count (x) Auto-correlations', lag.max=length(cmtcount))
# 
# # Fitting sine and cosine on spectral term ==> didn't work well
# sub.spectralfit=lm(subcount~cos(2*pi*t/8)+ sin(2*pi*t))
# summary(sub.spectralfit)
# 
# par(mfrow=c(1,2))
# acf(subcount, main='Subscribe (y) Auto-correlations')
# acf(sub.spectralfit$residuals, main='Subscribe (y) Auto-correlations (After)')
# acf(subcount, main='Subscribe (y) Auto-correlations', lag.max=length(subcount))
# acf(sub.spectralfit$residuals, main='Subscribe (y) Auto-correlations (After)', lag.max=length(subcount))
# 
# like.spectralfit=lm(likecount~cos(2*pi*t/8)+sin(2*pi*t))
# summary(like.spectralfit)
# 
# acf(likecount, main='Like Count (x1) Auto-correlations')
# acf(like.spectralfit$residuals, main='Like (x1) Auto-correlations (After)')
# acf(likecount, main='Like Count (x) Auto-correlations', lag.max=length(likecount))
# acf(like.spectralfit$residuals, main='Like (y) Auto-correlations (After)', lag.max=length(likecount))
# 
# dislike.spectralfit=lm(dislikecount~cos(2*pi*t/8))
# summary(dislike.spectralfit)
# 
# acf(dislikecount, main='Disike Count (x) Auto-correlations')
# acf(dislike.spectralfit$residuals, main='Dislike (y) Auto-correlations (After)')
# acf(dislikecount, main='Dislike Count (x) Auto-correlations', lag.max=length(likecount))
# acf(dislike.spectralfit$residuals, main='Dislike (y) Auto-correlations (After)', lag.max=length(likecount))
# 
# viewcount.spectralfit=lm(viewcount~cos(2*pi*t/8))
# summary(viewcount.spectralfit)
# 
# acf(viewcount, main='View Count (x) Auto-correlations')
# acf(viewcount.spectralfit$residuals, main='View Count (y) Auto-correlations (After)')
# acf(viewcount, main='View Count(x) Auto-correlations', lag.max=length(likecount))
# acf(viewcount.spectralfit$residuals, main='View Count(y) Auto-correlations (After)', lag.max=length(likecount))
# 
# cmt.spectralfit=lm(cmtcount~cos(2*pi*t/8))
# summary(cmt.spectralfit)
# 
# acf(cmtcount, main='Comment Count (x) Auto-correlations')
# acf(cmt.spectralfit$residuals, main='Comment (y) Auto-correlations (After)')
# acf(cmtcount, main='Comment Count (x) Auto-correlations', lag.max=length(likecount))
# acf(cmt.spectralfit$residuals, main='Comment (y) Auto-correlations (After)', lag.max=length(likecount))
# 
# 
# acf(subcount, plot=F)[8]
# acf(sub.spectralfit$residuals, plot=F)[8]
# 
# acf(likecount, plot=F)[8]
# acf(like.spectralfit$residuals, plot=F)[8]
# 
# acf(dislikecount, plot=F)[8]
# acf(dislike.spectralfit$residuals, plot=F)[8]
# 
# acf(cmtcount, plot=F)[8]
# acf(cmt.spectralfit$residuals, plot=F)[8]
# 
# acf(viewcount, plot=F)[8]
# acf(viewcount.spectralfit$residuals, plot=F)[8]
# 
# subcount<-sub.spectralfit$residuals
# likecount<-like.spectralfit$residuals
# dislikecount<-dislike.spectralfit$residuals
# viewcount<-viewcount.spectralfit$residuals
# cmtcount<-cmt.spectralfit$residuals