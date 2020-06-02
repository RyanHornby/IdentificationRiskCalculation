# IdentificationRiskCalculation 
## Calculating the Identification Risk in Partially Synthetic Microdata

This package calculates the identification risk in partially synthetic microdata. The expected match risk, 
the true match rate, and the false match rate are reported. The calculation supports mixed data type, including categorical variables and continuous variables.

```{r}
require(IdentificationRiskCalculation)
require(synthpop)

CEdata <- IdentificationRiskCalculation::CEdata
CEdata$Urban <- as.factor(CEdata$Urban)
CEdata$Marital <- as.factor(CEdata$Marital)
CEdata$Tenure <- as.factor(CEdata$Tenure)
```
