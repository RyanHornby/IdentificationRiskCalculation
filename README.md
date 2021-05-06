# IdentificationRiskCalculation 
## Calculating the Identification Risks in Partially Synthetic Microdata

This package calculates the identification risk in partially synthetic microdata. The expected match risk, 
the true match rate, and the false match rate are reported. The calculation supports mixed data type, including categorical variables and continuous variables. The details of the calculation methods, including how identification risks are defined, are available at this published paper: http://www.tdp.cat/issues21/abs.a425a21.php

We use the ```synthpop``` to synthesize variables.

```{r}
install.packages("synthpop")
devtools::install_github("https://github.com/RyanHornby/IdentificationRiskCalculation")

require(IdentificationRiskCalculation)
require(synthpop)
```

Here we provide demonstration on how to use this package. ```CEdata``` is available in the package. It contains a few continuous and a few categorical variables.

```{r}
CEdata <- IdentificationRiskCalculation::CEdata
CEdata$Urban <- as.factor(CEdata$Urban)
CEdata$Marital <- as.factor(CEdata$Marital)
CEdata$Tenure <- as.factor(CEdata$Tenure)
```

Assume that ```Age, Urban, Marital``` are known variables. We use a 0.1 radius for continuous ```Age``` variable.

```{r}
knownvars <- c("Age", "Urban", "Marital")
r_age <- 0.1
```

For the following 5 scenarios, we use the same 0.1 radius for all continuous variables for demonstration purpose. The output contains the expected match risk ```exp.risk```, the true match rate ```true.rate```, and the false match rate ```false.rate```.

## Scenario 1: Income (continuous)
```{r}
synvars1 <- c("Income")
syndata1 <- syn(CEdata, m = 20, visit.sequence = synvars1)
r_income <- 0.1
riskList1 <- IdentificationRisk(origdata = CEdata, 
                                syndata = syndata1$syn, 
                                known = knownvars, 
                                syn = synvars1, 
                                r = c(r_age, r_income))

exp.risk1 <- riskList1$exp.risk_vector
true.rate1 <- riskList1$true.rate_vector
false.rate1 <- riskList1$false.rate_vector
```

## Scenario 2: Tenure (categorical), Income (continuous)
```{r}
synvars2 <- c("Tenure","Income")
syndata2 <- syn(CEdata, m = 20, visit.sequence = synvars2)
r_income <- 0.1
riskList2 <- IdentificationRisk(origdata = CEdata, 
                                syndata = syndata2$syn, 
                                known = knownvars, 
                                syn = synvars2, 
                                r = c(r_age, r_income))
                                
exp.risk2 <- riskList2$exp.risk_vector
true.rate2 <- riskList2$true.rate_vector
false.rate2 <- riskList2$false.rate_vector
```

## Scenario 3: Expenditure (continuous), Income (continuous)
```{r}
synvars3 <- c("Expenditure","Income")
syndata3 <- syn(CEdata, m = 20, visit.sequence = synvars3)
r_income <- 0.1
r_expenditure <- 0.1
riskList3 <- IdentificationRisk(origdata = CEdata, 
                                syndata = syndata3$syn, 
                                known = knownvars,
                                syn = synvars3, 
                                r = c(r_age, r_expenditure, 
                                      r_income))

exp.risk3 <- riskList3$exp.risk_vector
true.rate3 <- riskList3$true.rate_vector
false.rate3 <- riskList3$false.rate_vector
```

## Scenario 4: Tenure (categorical), Expenditure (continuous), Income (continuous)
```{r}
synvars4 <- c("Tenure", "Expenditure", "Income")
syndata4 <- syn(CEdata, m = 20, visit.sequence = synvars4)
r_income <- 0.1
r_expenditure <- 0.1
riskList4 <- IdentificationRisk(origdata = CEdata, 
                                syndata = syndata4$syn, 
                                known = knownvars, 
                                syn = synvars4,
                                r = c(r_age, r_expenditure, 
                                      r_income))

exp.risk4 <- riskList4$exp.risk_vector
true.rate4 <- riskList4$true.rate_vector
false.rate4 <- riskList4$false.rate_vector
```

## Scenario 5: Tenure (categorical)
```{r}
synvars5 <- c("Tenure", "Urban")
syndata5 <- syn(CEdata, m = 20, visit.sequence = synvars5)
riskList5 <- IdentificationRisk(origdata = CEdata, 
                                syndata = syndata5$syn, 
                                known = knownvars, 
                                syn = synvars5)

exp.risk5 <- riskList5$exp.risk_vector
true.rate5 <- riskList5$true.rate_vector
false.rate5 <- riskList5$false.rate_vector
```
