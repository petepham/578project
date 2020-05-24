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

### [GRAPHIC] Cox PH Curve (Weibull?)
### [GRAPHIC] Other
### other - grab from Ch 4/5 HW

## Model Dianostics
##  LRT
### AIC/R/BIC (for the regressio models - ch 4-6 materials)
### Residual Analysis (for the regression models - ch 4-6 models)
### QQ Plot 
  #1) for the overall models (ch3 material)
  #2) for the stratified models (ch3 material)
  #4) for the regression models (ch 4-6 material)

# Conclusion

# Discussion
