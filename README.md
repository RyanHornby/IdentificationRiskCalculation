# IdentificationRiskCalculation 
## Calculating the Identification Risk in Partially Synthetic Microdata

This package calculates the identification risk in partially synthetic microdata. The expected match risk, 
the true match rate, and the false match rate are reported. The calculation supports mixed data type, including categorical variables and continuous variables.

We use the ```synthpop``` to synthesize variables.

```{r}
install.packages("synthpop")
devtools::install_github("https://github.com/RyanHornby/IdentificationRisk")

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

For the following 5 scenarios, we use the same 0.1 radius for all continuous variables for demonstration purpose,

## Scenario 1: Income (continuous)
```{r}
synvars1 <- c("Income")
syndata1 <- syn(CEdata, m = 20, visit.sequence = synvars1)
r_income <- 0.1
riskList1 <- IdentificationRiskContinuous(CEdata, syndata1$syn, 
                                          knownvars, synvars1, 
                                          c(r_age, r_income))
IR1 <- riskList1$exp.risk_vector
```

## Scenario 2: Tenure (categorical), Income (continuous)
```{r}
synvars2 <- c("Tenure","Income")
syndata2 <- syn(CEdata, m = 20, visit.sequence = synvars2)
r_income <- 0.1
riskList2 <- IdentificationRiskContinuous(CEdata, syndata2$syn, 
                                          knownvars, synvars2, 
                                          c(r_age, r_income))
IR2 <- riskList2$exp.risk_vector
```

## Scenario 3: Expenditure (continuous), Income (continuous)
```{r}
synvars3 <- c("Expenditure","Income")
syndata3 <- syn(CEdata, m = 20, visit.sequence = synvars3)
r_income <- 0.1
r_expenditure <- 0.1
riskList3 <- IdentificationRiskContinuous(CEdata, syndata3$syn, 
                                          knownvars, synvars3, 
                                          c(r_age, r_expenditure,
                                            r_income))
IR3 <- riskList3$exp.risk_vector
```

## Scenario 4: Tenure (categorical), Expenditure (continuous), Income (continuous)
```{r}
synvars4 <- c("Tenure", "Expenditure", "Income")
syndata4 <- syn(CEdata, m = 20, visit.sequence = synvars4)
r_income <- 0.1
r_expenditure <- 0.1
riskList4 <- IdentificationRiskContinuous(CEdata, syndata4$syn, 
                                          knownvars, synvars4,
                                          c(r_age, r_expenditure,
                                            r_income))
IR4 <- riskList4$exp.risk_vector
```

## Scenario 5: Tenure (categorical)
```{r}
synvars5 <- c("Tenure", "Urban")
syndata5 <- syn(CEdata, m = 20, visit.sequence = synvars5)
riskList5 <- IdentificationRiskCategorical(CEdata, syndata5$syn, 
                                          knownvars, synvars5)
IR5 <- riskList5$exp.risk_vector
```
