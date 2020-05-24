library(readxl)
library(knitr)
library(tidyverse)
library(kableExtra)
library(survival)
library(ggsurvplot)

# Introduction ###################
## Background

## Objective

## Data
### Header definition
df.sum <- data.frame(read_excel("df.sum.xlsx"))
df.sum

### Table of the data
df <- data.frame(read_excel("df.xlsx"))
df

## Definitions
### Medical definitions
### Censored information
### Randomness


# Methods ###################
## Statistical Analysis
### Kaplan-Meier
### Weibull
### Cox PH

# Results ###################
### Descriptive Statistics [point estimates and quantiles go here]
### Summary statistics

## Survival Analysis (chapter 3 material) 
### [GRAPHIC] KM Curve
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

# Conclusion

# Discussion

#Weibull parametric model 

library(readxl)
library(knitr)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(survival)
library(survminer)

df <- data.frame(read_excel("df.xlsx"))

months=df$Survival
status=df$Status
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

s.df = Surv(df$Survival,df$Status)
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