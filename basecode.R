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

#test comment


# Methods ###################
## Statistical Analysis
### Kaplan-Meier
### Weibull
### Cox PH

# Results ###################
## Descriptive Statistics
### Summary statistics

## Survival Analysis
### [GRAPHIC] KM Curve
### [GRAPHIC] Weibull Curve
### [GRAPHIC] Cox PH Curve

## Model Dianostics
### AIC/R/BIC
### Residual Analysis
### QQ Plot


# Conclusion

# Discussion
