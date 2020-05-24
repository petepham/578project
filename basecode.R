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
df <- data.frame(read_excel("df.xlsx"))
s.df = Surv(df$Survival,df$Status)
km.all = survfit(s.df~1,type="kaplan-meier", data = df)
ggsurvplot(km.all, 
           palette = "#2E9FDF", 
           conf.int = TRUE, 
           title="Post-Myocardial Infarction Survival", 
           subtitle="All Groups",
           font.title=c(12,"bold.italic"),
           font.subtitle = c(10,"italic"),
           font.x = c(9, "bold.italic"),
           font.y = c(9, "bold.italic"),
           ylab="Surival Proportion", 
           xlab="Time to Death (Months)",
           surv.median.line = "hv",
           legend.title = "Groups",
           legend.labs = "All")

km.age = survfit(s.df~Age.Strata, type="kaplan-meier", data = df)
ggsurvplot(km.age, 
           palette = c("darkcyan","darkgoldenrod3","darkorange3"), 
           title="Post-Myocardial Infarction Survival", 
           subtitle="Stratified by Age Group",
           font.title=c(12,"bold.italic"),
           font.subtitle = c(10,"italic"),
           font.x = c(9, "bold.italic"),
           font.y = c(9, "bold.italic"),
           ylab="Surival Proportion", 
           xlab="Time to Death (Months)",
           surv.median.line = "hv",
           legend.title = "Groups",
           legend.labs = c("< 45 Year","45 - 64 Years","\u2265 65 Years"))

km.effusion = survfit(s.df~P.Effusion, type="kaplan-meier", data = df)
ggsurvplot(km.effusion, 
           palette = c("darkcyan","darkgoldenrod3"), 
           title="Post-Myocardial Infarction Survival", 
           subtitle="Stratified by Presence of Pericardial Effusion",
           font.title=c(12,"bold.italic"),
           font.subtitle = c(10,"italic"),
           font.x = c(9, "bold.italic"),
           font.y = c(9, "bold.italic"),
           ylab="Surival Proportion", 
           xlab="Time to Death (Months)",
           surv.median.line = "hv",
           legend.title = "Groups",
           legend.labs = c("Present","Absent"))

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





