# Setup
library(readxl)
library(knitr)
library(tidyverse)
library(kableExtra)
library(survival)
library(survminer)
library(VIM)
library(missForest)
library(writexl)

df = data.frame(read_excel("df.xlsx"))    #importing the dataset
df[df=="?"] = " "    #removing all "?'s" with blanks. This prepares the dataset for imputation.
df.new = data.frame(read_excel("df.new.xlsx")) 



# Dataset: Variable Summary (Appendix)
df.sum <- data.frame(read_excel("df.sum.xlsx"))
df.sum



# Table of the data
df <- data.frame(read_excel("df.xlsx"))
df



# Dataset Imputation
set.seed(7522) 
df.i = missForest(df, maxiter = 30, ntree = 1000)
df.i$ximp #quick check of imputed values
df.i$OOBerror #this is the normalized mean squared error. We will compared this with the next step

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x}

df.impute = round_df(df.i$ximp,2) #imputed values table
df.new = df.impute[,c(-5,-12)] #remove incomplete strata from original data
Age.s = ifelse(df.impute$Age < 55,0,ifelse(df.impute$Age < 71, 1, 2)) #new age strata based on imputed data
WMS.s = ifelse(df.impute$WMS < 12,0,ifelse(df.impute$WMS < 15, 1, 2)) #new WMS strata based on imputed data
df.new$Age.s = Age.s
df.new$WMS.s = WMS.s

s.df = Surv(df.new$Survival,df.new$Status)

write_xlsx(df.new,"df.new.xlsx")
write.csv(df.new,"df.new.csv")

s.df = Surv(df.new$Survival,df.new$Status)



# Kaplan-Meier Plots
km.plots = list()

km.all = survfit(s.df~1,type="kaplan-meier", data=df.new)
km.plots[[1]] = ggsurvplot(km.all, 
                           palette = "#2E9FDF", 
                           conf.int = TRUE, 
                           title="Post-Myocardial Infarction Survival", 
                           subtitle="Survival Among All Groups",
                           font.title=c(14,"bold.italic"),
                           font.subtitle = c(10,"italic"),
                           font.x = c(9, "bold.italic"),
                           font.y = c(9, "bold.italic"),
                           ylab="Surival Proportion", 
                           xlab="Time to Death (Months)",
                           surv.median.line = "hv",
                           legend.title = "Groups",
                           legend.labs = "All")


km.age = survfit(s.df~Age.s, type="kaplan-meier", data = df.new)
km.plots[[2]] = ggsurvplot(km.age, 
                           palette = c("darkcyan","darkgoldenrod3","darkorange3"), 
                           subtitle="Survival, Stratified by Age Group",
                           font.subtitle = c(10,"italic"),
                           font.x = c(9, "bold.italic"),
                           font.y = c(9, "bold.italic"),
                           ylab="Surival Proportion", 
                           xlab="Time to Death (Months)",
                           surv.median.line = "hv",
                           legend.title = "Groups",
                           legend.labs = c("< 55 Year","55 - 65 Years","> 65 Years"))

km.effusion = survfit(s.df~P.Effusion, type="kaplan-meier", data = df.new)
km.plots[[3]] = ggsurvplot(km.effusion, 
                           palette = c("darkcyan","darkgoldenrod3"), 
                           subtitle="Survival, Stratified by Presence of Pericardial Effusion",
                           font.subtitle = c(10,"italic"),
                           font.x = c(9, "bold.italic"),
                           font.y = c(9, "bold.italic"),
                           ylab="Surival Proportion", 
                           xlab="Time to Death (Months)",
                           surv.median.line = "hv",
                           legend.title = "Groups",
                           legend.labs = c("Present","Absent"))

km.wms = survfit(s.df~WMS.s, type="kaplan-meier", data = df.new)
km.plots[[4]] = ggsurvplot(km.wms, 
                           palette = c("darkcyan","darkgoldenrod3","darkorange3"), 
                           subtitle="Survival, Stratified by Wall Motion Index",
                           font.subtitle = c(10,"italic"),
                           font.x = c(9, "bold.italic"),
                           font.y = c(9, "bold.italic"),
                           ylab="Surival Proportion", 
                           xlab="Time to Death (Months)",
                           surv.median.line = "hv",
                           legend.title = "Groups",
                           legend.labs = c("< 12", "12-14", "> 14"))


arrange_ggsurvplots(km.plots, print=TRUE, ncol=2, nrow=2)

# Kaplan-Meier Suvival Summary
ks1 = data.frame(t(summary(km.all)$table))
ks2 = data.frame(summary(km.age)$table)
ks3 = data.frame(summary(km.effusion)$table)
ks4 = data.frame(summary(km.wms)$table)

ksall = rbind(ks1, ks2, ks3, ks4)
km.as = ksall[,c(1,4,5,7,8,9)]
colnames(km.as) = c("Records","Events","Mean","Median","Median 0.95 LCL","Median 0.95 UCL")
rownames(km.as) = c("All","Age:<55", "Age: 55-65", "Age: >65", "P.Eff: Absent","P.Eff: Present", "WMS: < 12", "WMS: 12-14", "WMS: >14")

kable(km.as, caption="Kaplan-Meier Results",align="c", digits=2) %>%
  kable_styling(position = "center", latex_options="hold_position")


# Kaplan-Meier Cumulative Hazard Estimators
haz.plots = list()

haz.plots[[1]] = ggsurvplot(km.all, 
                            fun = "cumhaz",
                            palette = "#2E9FDF", 
                            conf.int = TRUE, 
                            title="Post-Myocardial Infarction Hazard", 
                            subtitle="Hazard Among All Groups",
                            font.title=c(14,"bold.italic"),
                            font.subtitle = c(10,"italic"),
                            font.x = c(9, "bold.italic"),
                            font.y = c(9, "bold.italic"),
                            ylab="Surival Proportion", 
                            xlab="Time to Death (Months)",
                            legend.title = "Groups",
                            legend.labs = "All")

haz.plots[[2]] = ggsurvplot(km.age, 
                            fun = "cumhaz",
                            palette = c("darkcyan","darkgoldenrod3","darkorange3"), 
                            subtitle="Hazard, Stratified by Age Group",
                            font.subtitle = c(10,"italic"),
                            font.x = c(9, "bold.italic"),
                            font.y = c(9, "bold.italic"),
                            ylab="Surival Proportion", 
                            xlab="Time to Death (Months)",
                            legend.title = "Groups",
                            legend.labs = c("< 55 Year","55 - 65 Years","> 65 Years"))

haz.plots[[3]] = ggsurvplot(km.effusion, 
                            fun = "cumhaz",
                            palette = c("darkcyan","darkgoldenrod3"), 
                            subtitle="Hazard, Stratified by Pericardial Effusion Presence",
                            font.subtitle = c(10,"italic"),
                            font.x = c(9, "bold.italic"),
                            font.y = c(9, "bold.italic"),
                            ylab="Surival Proportion", 
                            xlab="Time to Death (Months)",
                            legend.title = "Groups",
                            legend.labs = c("Present","Absent"))

haz.plots[[4]] = ggsurvplot(km.wms, 
                            fun = "cumhaz",
                            palette = c("darkcyan","darkgoldenrod3","darkorange3"), 
                            subtitle="Hazard, Stratified by Wall Motion Index",
                            font.subtitle = c(10,"italic"),
                            font.x = c(9, "bold.italic"),
                            font.y = c(9, "bold.italic"),
                            ylab="Surival Proportion", 
                            xlab="Time to Death (Months)",
                            legend.title = "Groups",
                            legend.labs = c("< 12", "12-14", "> 14"))


arrange_ggsurvplots(haz.plots, print=TRUE, ncol=2, nrow=2)


### [GRAPHIC] Weibull Curve
### [GRAPHIC] Log-Logistic Curve


# Regression (chapter 4-6 material)

### [GRAPHIC] Cox PH Curve - Note we need to verify that Proportional Hazards assumption is not violated. 
### If it is, refer to page 136 for details on how to adjust analysis strategy
### [GRAPHIC] Other
###  Model parameter estimates and CIs
### other - grab from Ch 4/5 HW (covariate interatcion, Step AIC covariate selection, etc,)

## Model Dianostics
### LRT/AIC/R/BIC (for each regressio models - ch 4-6 materials)
### Residual Analysis (for the regression models - ch 4-6 material)
### QQ Plot 
#1) for the overall models (ch3 material)
#2) for the stratified models (ch3 material)
#4) for the regression models (ch 4-6 material)

#Weibull parametric model 

library(readxl)
library(knitr)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(survival)
library(survminer)

months=df.new$Survival
status=df.new$Status
months.u=months[status == 1]
months.u = sort(months.u)
nu = length(months.u)

#parameter estimation
weib.fit=survreg(Surv(months,status)~1,dist="weib")
alphahat=1/weib.fit$scale
scalehat=exp(weib.fit$coefficients)

#Point and CI estimates for 3 quantiles
medhat25.w = predict(weib.fit,type="uquantile",p=0.25,se.fit=T)
medhat25.1.w = medhat25.w$fit[1]
medhat25.1.se.w = medhat25.w$se.fit[1]
C.I.median25.w = c(exp(medhat25.1.w),exp(medhat25.1.w-1.96*medhat25.1.se.w),exp(medhat25.1.w+1.96*medhat25.1.se.w))
names(C.I.median25.w) = c("median_25_w","LCL","UCL")

medhat5.w = predict(weib.fit,type="uquantile",p=0.5,se.fit=T)
medhat5.1.w = medhat5.w$fit[1]
medhat5.1.se.w = medhat5.w$se.fit[1]
C.I.median5.w = c(exp(medhat5.1.w),exp(medhat5.1.w-1.96*medhat5.1.se.w),exp(medhat5.1.w+1.96*medhat5.1.se.w))
names(C.I.median5.w) = c("median_50_w","LCL","UCL")

medhat75.w = predict(weib.fit,type="uquantile",p=0.75,se.fit=T)
medhat75.1.w = medhat75.w$fit[1]
medhat75.1.se.w = medhat75.w$se.fit[1]
C.I.median75.w = c(exp(medhat75.1.w),exp(medhat75.1.w-1.96*medhat75.1.se.w),exp(medhat75.1.w+1.96*medhat75.1.se.w))
names(C.I.median75.w) = c("median_75_w","LCL","UCL")

C.I.median25.w
C.I.median5.w
C.I.median75.w

#point estimates S(t)

Shat.w = 1- pweibull(months.u,alphahat,scalehat)
C.I.Shat.w = data.frame(months.u,Shat.w)
round(C.I.Shat.w,5)

#k-m and s curves same plot

s.df.new = Surv(df.new$Survival,df.new$Status)
km.all = survfit(s.df~1,type="kaplan-meier", data = df)
ggsurvplot(km.all, 
           palette = "#2E9FDF", 
           conf.int = TRUE, 
           title="Post-Myocardial Infarction Survival", 
           subtitle="All Groups",
           font.title="bold",
           font.subtitle = "italic",
           ylab="Surival Proportion", 
           xlab="Time to Death (Months)",
           surv.median.line = "hv",
           legend.title = "Groups",
           legend.labs = "All")

plot(km.all,conf.int=F,xlab="time until death (in months)",ylab="proportion survived")
lines(months.u, Shat.w, lty=4)
legend(40, 0.8, legend=c("Kaplan-Meier", "Weibull"),lty=1:4, cex=0.8)
abline(h=0)

#####


library(survminer)
library(MASS)

y=df.new$Survival

#select the following pool of possible covariates

x1=df.new$Age
x2=df.new$Age.Strata
x3=df.new$P.Effusion
x4=df.new$F.Shortening
x5=df.new$EPSS
x6=df.new$LVDD
x7=df.new$WMI
y2=Surv(months,status)
data.frame.2=data.frame(y,x1,x2,x3,x4,x5,x6,x7)

cph.fit1=coxph(Surv(months,status)~x2,data=df.new)
summary(cph.fit1)

cph.fit2=stepAIC(cph.fit1,~.^2)
cph.fit2$anova

summary(cph.fit1)

pairs(data.frame.2) 
pairs(data.frame.3)

#diagnostics - check for cox assumptions
test.ph <- cox.zph(cph.fit1)
ggcoxzph(test.ph)
ggcoxdiagnostics(cph.fit1, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(cph.fit1, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
