### SEM Model Setup for Need for Dominance ###

### Load libraries for project
library(lavaan)
library(psych)
library(tidyverse)
library(dplyr)
library(tidyr)
library(Hmisc)
library(apaTables)
library(performance)

# Load file
setwd("~/Desktop/Projects/Github-Repos/Need-for-Dominance/scripts/r")
data <- read_csv("~/Desktop/Projects/Github-Repos/Need-for-Dominance/data_raw/NFC Study 2 Data.csv")
data <- subset(data, select = grep("_DO", names(data), value = TRUE, invert = TRUE))
data <- data[rowSums(is.na(data)) != ncol(data),] # Removing completely missing data
data <- subset(data, select = grep("l_1", names(data), value = TRUE, invert = TRUE))
data <- subset(data, select = grep("l_2", names(data), value = TRUE, invert = TRUE))
data <- subset(data, select = grep("l_6", names(data), value = TRUE, invert = TRUE))
data <- subset(data, select = grep("_Men", names(data), value = TRUE, invert = TRUE))
data <- subset(data, select = grep("_Women", names(data), value = TRUE, invert = TRUE))
data <- subset(data, select = grep("l_Straight", names(data), value = TRUE, invert = TRUE))
# Empty data frame to be filled later
Final_Data <- data.frame(matrix(ncol = 6, nrow = 544))
colnames(Final_Data) <- c("NFC_Scores", 
                  "SDO_Scores", 
                  "Political_ID", 
                  "Real_Scores",
                  "Stability_Scores",
                  "Evaluations_Scores")

## Item Prep
NFC <- data %>% dplyr::select(matches("NFC_")) 
Final_Data["NFC_Scores"] <- rowSums(NFC)
SDO <- data %>% dplyr::select(matches("SDO_"))
# Reverse code items 5-8 and 13-16
keys <- c(1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1)
SDO <- reverse.code(keys,SDO,mini = 1, maxi = 7)
Final_Data["SDO_Scores"] <- rowMeans(SDO)
Political_ID <- data %>% dplyr::select(matches("Self_Pol_Ideology_1"))
Final_Data["Political_ID"] <- Political_ID
Real <- data %>% dplyr::select(matches("Real"))
Final_Data["Real_Scores"] <- rowMeans(Real)
Stability <- data %>% dplyr::select(matches("SB"))
Final_Data["Stability_Scores"] <- rowMeans(Stability)
Evaluations <- data %>% dplyr::select(matches("Eval"))
Final_Data["Evaluations_Scores"] <- rowMeans(Evaluations)


### Descriptive Statistics
## Mean, SD, Range, Skewness, Kurtosis, Alpha, Omega
## NFC
NFC_Cor <- cor(NFC)
round(NFC_Cor, digits = 2)
# NFC["NFC_Skewness"]
# NFC["NFC_Kurtosis"]
# NFC["NFC_Alpha"]
# NFC["NFC_Omega"]
## SDO 
SDO_Cor <- cor(SDO)
round(SDO_Cor, digits = 2)
# NFC["NFC_Skewness"]
# NFC["NFC_Kurtosis"]
# NFC["NFC_Alpha"]
# NFC["NFC_Omega"]
## Political Identity
# NFC["NFC_Skewness"]
# NFC["NFC_Kurtosis"]
# NFC["NFC_Alpha"]
# NFC["NFC_Omega"]
## Realness
SDO_Cor <- cor(SDO)
round(SDO_Cor, digits = 2)
# Real["Real_ZScore"]
# Real["Real_Mean"]
# NFC["NFC_SD"]
# NFC["NFC_Range"]
# NFC["NFC_Skewness"]
# NFC["NFC_Kurtosis"]
# NFC["NFC_Alpha"]
# NFC["NFC_Omega"]
# Stability
SDO_Cor <- cor(Stability)
round(SDO_Cor, digits = 2)
# Stability["Stability_ZScore"]
# Stability["Stability_Mean"]
# NFC["NFC_SD"]
# NFC["NFC_Range"]
# NFC["NFC_Skewness"]
# NFC["NFC_Kurtosis"]
# NFC["NFC_Alpha"]
# NFC["NFC_Omega"]
# Evaluations
SDO_Cor <- cor(Evaluations)
round(SDO_Cor, digits = 2)
# Evaluations["Evaluations_ZScore"]
# Evaluations["Evaluations_Mean"]
# NFC["NFC_SD"]
# NFC["NFC_Range"]
# NFC["NFC_Skewness"]
# NFC["NFC_Kurtosis"]
# NFC["NFC_Alpha"]
# NFC["NFC_Omega"]

### Tables


### Plots


### Setup model
model1 <-'
  # Measurement Model
    NFC =~ NFC_Parcel_x1 + NFC_Parcel_x2 + NFC_Parcel_x3
    SDO =~ SDO_Parcel_x1 + SDO_Parcel_x2 + SDO_Parcel_x3
    Political_Identity = Pol_ID_x1
    Real =~ Real_Parcel_x1 + Real_Parcel_x2 + Real_Parcel_x3
    Stability =~ Stability_Parcel_x1 + RStability_x2 + Stability_Parcel_x3
    Evaluation =~ Evaluation_x1 + Evaluation_x2 + Evaluation_x3
  # Regression
    Real ~ NFC + SDO + Political_Identity
    Stability ~ NFC + SDO + Political_Identity
    Evaluation ~ NFC + SDO + Political_Identity
    SDO ~ NFC
    Political_Identity ~ NFC 
'

### Fit model
#f1 <- sem(model1, data=data,std.lv=T, se="boot", bootstrap=5000)

### Get summary stats
#summary(f1, fit=T, standardized=T, ci=TRUE, rsquare=T)