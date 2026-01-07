# Thesis-focused project. From 2022 to 2024, I was a graduate student
## at Carleton University and also served as a research assistant
###for Ottawa’s Coordinated Access and Referral to Services, where I led a
##### community-based research project examining school absenteeism among youth in
###### Ottawa. This work involved
####### analyzing multiple risk domains, including externalizing and internalizing
######## behaviours, family dynamics, adverse childhood experiences, and
######### psychological disorders. The project required extensive data coding,
########## management, analysis, and visualization using R, SQL, and MS Excel.

### Key Terms 
# HoA = History of Absenteeism 
# nHoA = No History of Absenteeism 
# CR = Cumulative Risks 
# PD = Psychological Disorders
# IB = Internalizing Behaviours 
# EB = Externalizing Behaviours 
# ACE = Adverse Childhood Experiences 
# FI = Family Issues 

#Opening the main Absenteeism file

library(readxl)
absenteeism_legit <- read_xlsx(file.choose()) #AbsenteeismData_Legit excel.file
head(absenteeism_legit)
View(absenteeism_legit)

install.packages("dplyr")
library(dplyr)

#Opening Primary Presenting Problems file

ppp_legit <- read_xlsx(file.choose()) #PPP_Numbered_Legit
head(ppp_legit)
View(ppp_legit)

ppp_legit <- na.omit(ppp_legit)
ppp_legit
View(ppp_legit)

#### Pivot wider 

library(tidyr)

widefile <- ppp_legit
names(widefile) <- c('ID', 'problem')
widefile$present <- 1

wide_legit <- widefile %>%
  filter(!is.na(ID)) %>%
  mutate(row = row_number()) %>%
  group_by(ID) %>%
  pivot_wider(names_from = 'problem', values_from = 'present',
              names_prefix = 'primary') %>%
  select(-c(row)) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

View(wide_legit)

###Merging the PPP_Legit and Absenteeism_Legit Files (Correct Version)

install.packages("dplyr")
library(dplyr)

merged_dataset <- absenteeism_legit %>%
  full_join(wide_legit, by = 'ID')
View(merged_dataset) #N = 407
merged_dataset
summary(merged_dataset) #(N = 404)

merged_dataset <- na.omit(merged_dataset)
merged_dataset #N = 404 (all NAs removed) (N = 406 apparently)
View(merged_dataset)

# Indexing: Dataset only includes youth between 4-18yo (removed 0-3)

indices_to_remove <- which(merged_dataset$`Age until 2018` > 3)
merged_dataset_subset <- merged_dataset[indices_to_remove, ]
View(merged_dataset_subset) # N = 398 (youth aged 4-18)

### Descriptives 

mean(merged_dataset_subset$`Age until 2018`) # Mean age = 10.52
sd(merged_dataset_subset$`Age until 2018`) # SD age = 3.45

# n = Females 
merged_dataset_subset %>%
  filter(Gender == 1) #103 Females 

merged_dataset_subset %>%
  filter(Gender == 1 & `History of Absenteeism` ==1) #Females + HoA: n = 67

merged_dataset_subset %>%
  filter(Gender == 1 & `History of Absenteeism` == 0) #Females + nHoA: n = 36

# n = Males
merged_dataset_subset %>% 
  filter(Gender == 0) #291 Males

merged_dataset_subset %>%
  filter(Gender == 0 & `History of Absenteeism` == 1) #Males + HoA: n = 166

merged_dataset_subset %>%
  filter(Gender == 0 & `History of Absenteeism` == 0) #Males + nHoA: n = 125

# n = Other
merged_dataset_subset %>% 
  filter(Gender == "Other" | Gender == 2) #4 

merged_dataset_subset %>%
  filter(Gender == 2 & `History of Absenteeism` == 0) #Other + nHOA: n = 2

merged_dataset_subset %>%
  filter(Gender == 2 & `History of Absenteeism` == 1) #Other + HOA: n = 2 

#Absenteeism Frequency

merged_dataset_subset %>%
  filter(`History of Absenteeism` ==1) #n = 235: have a HoA (new = 233?)

merged_dataset_subset %>%
  filter(`History of Absenteeism` ==0) #n = 163: nHoA

###############

sd(merged_dataset_subset$primary15 + merged_dataset_subset$primary16 + merged_dataset_subset$primary17 +
     merged_dataset_subset$primary18 + merged_dataset_subset$primary19 +
     merged_dataset_subset$primary20 + merged_dataset_subset$primary21 +
     merged_dataset_subset$primary22 + merged_dataset_subset$primary23 +
     merged_dataset_subset$primary24 + merged_dataset_subset$primary25 +
     merged_dataset_subset$primary26 + merged_dataset_subset$primary27 +
     merged_dataset_subset$primary28)

###############

166+67+2 #n = 235 (HoA)
125+36+2 #n = 163 (nHoA)
235+163 #n = 398 (total sample)

#HOA
(173/235)*100 # 73.61% (out of the 235 students who reported HoA, male students make up 73.61%)
(67/235)*100 # 28.51% (out of the 235 students who reported HoA, female students make up 28.51%)
(2/235)*100 # .85% (out of the 235 students who reported HoA, Other students make up .85%)

#nHOA
(126/163)*100 # 77.3% (out of the 163 students who reported nHoA, male students make up 77.3%)
(36/163)*100 # 22.09% (out of the 163 students who reported nHoA, male students make up 22.09%)
(2/163)*100 # 1.22% (out of the 163 students who reported nHoA, male students make up 1.22%)

#Creating a subset of the dataset for Age Frequency + HoA

age_hoa_data_legit <- merged_dataset_subset[,c("ID", "Age until 2018",
                                        "History of Absenteeism")]
age_hoa_data_legit
View(age_hoa_data_legit)

# Frequency of 4-5 y/o (Kindergarten)

age_hoa_data_legit %>%
  filter(`Age until 2018` <=5) # n = 34 

age_hoa_data_legit %>%
  filter(`Age until 2018` <=5 & `History of Absenteeism` ==1) # n = 17 (HoA)

age_hoa_data_legit %>%
  filter(`Age until 2018` <=5 & `History of Absenteeism` ==0) # n = 17 (nHoA)

(34/398)*100 # 8.54% of the sample is between 4-5 years old (in kindergarten)

# Frequency of 6-12 y/o (Elementary School)

age_hoa_data_legit %>%
  filter(`Age until 2018` >=6 & `Age until 2018` <=12) # n = 241

age_hoa_data_legit %>%
  filter(`Age until 2018` >=6 & `Age until 2018` <=12 &
           `History of Absenteeism` ==1) # n = 142 (HoA)

age_hoa_data_legit %>%
  filter(`Age until 2018` >=6 & `Age until 2018` <=12 &
           `History of Absenteeism` ==0) # n = 99 (nHoA)

(241/398)*100 #60.55%

# Frequency of 13-18 y/o (High School)

age_hoa_data_legit %>%
  filter(`Age until 2018` >=13 & `Age until 2018` <=18) # n = 123 

age_hoa_data_legit %>%
  filter(`Age until 2018` >=13 & `Age until 2018` <=18 &
           `History of Absenteeism` ==1) # n = 76

age_hoa_data_legit %>%
  filter(`Age until 2018` >=13 & `Age until 2018` <=18 &
           `History of Absenteeism` ==0) # n = 47 

(123/398)*100 #30.9%

#N = 398 (total sample)
123 + 241 + 34

#### CR Indices

# Externalizing Behaviors

ebeh <- merged_dataset_subset %>%
  rowwise %>%
  mutate(numprobs = sum(c(primary15, primary16, primary17, primary18, primary19,
                          primary20, primary21, primary22, primary23, primary24,
                          primary25, primary26, primary27, primary28), na.rm = TRUE))

table(ebeh$numprobs)
 0  1  2  3  4  5  6  7  8  9 10 11 12 
60 17 30 23 51 54 71 46 18 15  7  5  1 

#Externalizing Behavior Histogram
hist(ebeh$numprobs)
#Edited Histogram
hist(ebeh$numprobs, col = 'pink', border = 'black', 
     xlab = 'Externalizing Behavior Count', ylab = 'Youth Frequency',
     main = 'Cumulative Externalizing Behaviors')

# Internalizing Behaviors 

ibeh <- merged_dataset_subset %>%
  rowwise %>%
  mutate(numprobs = sum(c(primary43, primary44, primary48, primary49,
                          primary50), na.rm = TRUE))

table(ibeh$numprobs)
 0   1   2   3   4   5 
57 129  99  69  31  13

#Internalizing Behavior Histogram
hist(ibeh$numprobs)
#Edited Histogram
hist(ibeh$numprobs, col = 'orange', border = 'black',
     xlab = 'Internalizing Behavior Count', ylab = 'Youth Frequency',
     main = 'Cumulative Internalizing Behabviors')

# Psychological Disorders

psydisorders <- merged_dataset_subset %>%
  rowwise %>%
  mutate(numprobs = sum(c(primary2, primary6, primary7, primary11,
                          primary12, primary13), na.rm = TRUE))

### no participant have 5, or all 6 Psychological Disorders. 
table(psydisorders$numprobs)
 0   1   2   3   4 
106 108 121  48  15

#Psychological Disorders Histogram
hist(psydisorders$numprobs)
#Edited Histogram
hist(psydisorders$numprobs, col = 'red', border = 'black',
     xlab = 'Psychological Disorder Count', ylab = 'Youth Frequency',
     main = 'Cumulative Psychological Disorder Diagnoses')

# Developmental Trauma 
## w/ 6 items (D&S Abuse)

ace <- merged_dataset_subset %>%
  rowwise %>%
  mutate(numprobs = sum(c(primary35, primary36, primary37, primary40,
                          primary41, primary98), na.rm = TRUE))

# no participant reported all 6 ACEs (also, the high number of 0 makes sense, considering this is a parent-reported measure)
table(ace$numprobs)
  0   1   2   3   4   5 
284  53  31  17  10   3 

#Developmental Trauma Histogram
hist(ace$numprobs)
#Edited Histogram
hist(ace$numprobs, col = 'lightgreen', border = 'black',
     xlab = "Childhood Traumatic Events Count", ylab = 'Youth Frequency',
     main= 'Cumulative Traumatic Events')

# Family Issues

family <- merged_dataset_subset %>%
  rowwise %>%
  mutate(numprobs = sum(c(primary72, primary74, primary75,
                          primary76, primary77), na.rm = TRUE))

table(family$numprobs) #(also, the high number of 0 makes sense, considering this is a parent-reported measure)
  0   1   2   3   4   5   6 
130 100  82  50  26   9   1 

#Family Issues Histogram
hist(family$numprobs)
#Edited Histogram
hist(family$numprobs, col = 'yellow', border = 'black',
     xlab = 'Family Issue count', ylab = 'Youth Frequency',
     main = 'Cumulative Family Issues')

### Logistic Regression Models 

#Creating an Externalizing Behavior index variable of all 12 EB items
##and running the model with the EB cumulative index variable

ebeh_index <-rowSums(merged_dataset_subset[, c('primary15', 'primary16', 'primary17', 'primary18',
                                        'primary19', 'primary20', 'primary21', 'primary22', 'primary22',
                                        'primary23', 'primary24', 'primary25', 'primary26', 'primary27',
                                        'primary28')])

ebehmodel <- glm(`History of Absenteeism` ~ ebeh_index, data = merged_dataset_subset
                       , family = binomial(link = 'logit'), na.action = na.exclude)

summary(ebehmodel)

Call:
  glm(formula = `History of Absenteeism` ~ ebeh_index, family = binomial(link = "logit"), 
      data = merged_dataset_subset, na.action = na.exclude)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.4830  -1.3247   0.9573   1.0167   1.1198  

Coefficients:
            Estimate Std. Error z value   Pr(>|z|)
(Intercept)  0.13715    0.18864   0.727    0.467
ebeh_index   0.05068    0.03544   1.430    0.153

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 538.65  on 397  degrees of freedom
Residual deviance: 536.59  on 396  degrees of freedom
AIC: 540.59

Number of Fisher Scoring iterations: 4

# Calculating OR and CI for Ebehmodel

ebeh_coef_summary <- summary(ebehmodel)$coefficients
ebeh_coef_summary

ebeh_odds_ratios <- exp(ebeh_coef_summary[, "Estimate"])
ebeh_conf_intervals <- exp(confint(ebehmodel))

ebeh_odds_ratios
(Intercept)  ebeh_index 
  1.147004    1.051991 

ebeh_conf_intervals
              2.5 %   97.5 %
(Intercept) 0.7926747 1.663043
ebeh_index  0.9816523 1.128261

# Point-biserial correlation between HoA & EB index 

cor.test(merged_dataset_subset$`History of Absenteeism`, 
         ebeh_index) 
# cor = .0718
# p-value = .1526
# df = 396
# t = 1.4331
# 95% CI [-0.02, 0.17]

# Internalizing Behaviors model 

ibeh_index <- rowSums(merged_dataset_subset[, c('primary43', 'primary44', 'primary48',
                                                        'primary49', 'primary50')])

ibehmodel <- glm(`History of Absenteeism` ~ ibeh_index, data = merged_dataset_subset,
                       family = binomial(link = 'logit'), na.action = na.exclude)

summary(ibehmodel)

Call:
  glm(formula = `History of Absenteeism` ~ ibeh_index, family = binomial(link = "logit"), 
      data = merged_dataset_subset, na.action = na.exclude)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.6080  -1.2711   0.9374   1.0865   1.1651  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)  
(Intercept)  0.02906    0.17657   0.165   0.8693  
ibeh_index   0.18857    0.08196   2.301   0.0214 *
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 538.65  on 397  degrees of freedom
Residual deviance: 533.21  on 396  degrees of freedom
AIC: 537.21

Number of Fisher Scoring iterations: 4

# Calculating OR and CI for IBmodel

ibeh_coef_summary <- summary(ibehmodel)$coefficients
ibeh_coef_summary

ibeh_odds_ratios <- exp(ibeh_coef_summary[, "Estimate"])
ibeh_conf_intervals <- exp(confint(ibehmodel))

ibeh_odds_ratios
(Intercept)  ibeh_index 
  1.029483    1.207523 

ibeh_conf_intervals
              2.5 %   97.5 %
(Intercept) 0.7278853 1.455950
ibeh_index  1.0301947 1.421479

# Point-biserial correlation between HoA & IB index 

cor.test(merged_dataset_subset$`History of Absenteeism`, 
         ibeh_index)

# cor = .116
# p-value = .02
# df = 396
# t = 2.325
# 95% CI [0.018, .212]

# Psychological Disorders model 

psydisorders_index <-rowSums(merged_dataset_subset[,c('primary2', 'primary6', 'primary7',
                                                              'primary11', 'primary12', 'primary13')])

pdmodel <- glm(`History of Absenteeism` ~ psydisorders_index,
                       data = merged_dataset_subset,
                       family = binomial(link = 'logit'), na.action = na.exclude)

summary(pdmodel)

Call:
  glm(formula = `History of Absenteeism` ~ psydisorders_index, 
      family = binomial(link = "logit"), data = merged_dataset_subset, 
      na.action = na.exclude)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.3836  -1.3160   0.9843   1.0449   1.1072  

Coefficients:
                   Estimate Std. Error z value  Pr(>|z|)   
(Intercept)         0.47273    0.16440   2.875  0.00403 **
psydisorders_index -0.07634    0.09154  -0.834  0.40432   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 538.65  on 397  degrees of freedom
Residual deviance: 537.95  on 396  degrees of freedom
AIC: 541.95

Number of Fisher Scoring iterations: 4

# Calculating OR and CI for pdmodel

psydisorders_coef_summary <- summary(pdmodel)$coefficients
psydisorders_coef_summary

psydisorders_odds_ratios <- exp(psydisorders_coef_summary[, "Estimate"])
psydisorders_conf_intervals <- exp(confint(pdmodel))

psydisorders_odds_ratios
(Intercept) psydisorders_index 
 1.6043625          0.9265053 

psydisorders_conf_intervals
                       2.5 %   97.5 %
(Intercept)        1.1653719 2.222352
psydisorders_index 0.7739126 1.108738

# Point-biserial correlation between HoA & PD index 

cor.test(merged_dataset_subset$`History of Absenteeism`, 
         psydisorders_index)

# cor = -0.04
# p-value = .405
# df = 396
# t = -0.833
# 95% CI [-0.14, 0.05]

# Developmental Trauma model 

ace_index <- rowSums(merged_dataset_subset[, c('primary35', 'primary36', 'primary37',
                                                          'primary40', 'primary41', 'primary98')])

acemodel <- glm(`History of Absenteeism` ~ ace_index, data = merged_dataset_subset, 
                         family = binomial(link = 'logit'), na.action = na.exclude)

summary(acemodel)

Call:
  glm(formula = `History of Absenteeism` ~ ace_index, family = binomial(link = "logit"), 
      data = merged_dataset_subset, na.action = na.exclude)

Deviance Residuals: 
  Min      1Q  Median      3Q     Max  
-1.402  -1.328   1.008   1.034   1.034  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)   
(Intercept)  0.34760    0.11508   3.020  0.00252 **
ace_index    0.03306    0.09743   0.339  0.73439   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 538.65  on 397  degrees of freedom
Residual deviance: 538.53  on 396  degrees of freedom
AIC: 542.53

Number of Fisher Scoring iterations: 4

# Calculating OR and CI for ACEmodel

ace_coef_summary <- summary(acemodel)$coefficients
ace_coef_summary

ace_odds_ratios <- exp(ace_coef_summary[, "Estimate"])
ace_conf_intervals <- exp(confint(acemodel))

ace_odds_ratios
(Intercept)   ace_index 
  1.415660    1.033609 

ace_conf_intervals
               2.5 %   97.5 %
(Intercept) 1.1310812 1.776691
ace_index   0.8556943 1.256385

# Point-biserial correlation between HoA & PD index 

cor.test(merged_dataset_subset$`History of Absenteeism`, 
         ace_index)

# cor = 0.017
# p-value = .635
# df = 396
# t = 0.34
# 95% CI [-0.08, 0.12]

# Family Issue model_legit

family_index <- rowSums(merged_dataset_subset[, c('primary72', 'primary74', 'primary75',
                                                          'primary76', 'primary77')])

familymodel <- glm(`History of Absenteeism` ~ family_index, data = merged_dataset_subset,
                         family = binomial(link = 'logit'), na.action = na.exclude)

summary(familymodel)

Call:
  glm(formula = `History of Absenteeism` ~ family_index, family = binomial(link = "logit"), 
      data = merged_dataset_subset, na.action = na.exclude)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.5099  -1.3207   0.9736   1.0406   1.0749  

Coefficients:
               Estimate Std. Error z value Pr(>|z|)  
(Intercept)   0.24596    0.14697   1.674   0.0942 .
family_index  0.08476    0.07562   1.121   0.2623  
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 538.65  on 397  degrees of freedom
Residual deviance: 537.38  on 396  degrees of freedom
AIC: 541.38

Number of Fisher Scoring iterations: 4

# Calculating OR and CI for FamilyModel


family_coef_summary <- summary(familymodel)$coefficients
family_coef_summary

family_odds_ratios <- exp(family_coef_summary[, "Estimate"])
family_conf_intervals <- exp(confint(familymodel))

family_odds_ratios
(Intercept) family_index 
  1.278845     1.088453 

family_conf_intervals
                2.5 %   97.5 %
(Intercept)  0.9596060 1.708629
family_index 0.9395754 1.264599

# Point-biserial correlation between HoA & PD index 

cor.test(merged_dataset_subset$`History of Absenteeism`, 
         family_index)

# cor = 0.056
# p-value = .26
# df = 396
# t = 1,12
# 95% CI [-0.04, 0.15]

#Creating an Overall (Between) Cumulative index variable of all 5 indices 

# Overall model (Standardized IVs)

standardized_ebeh_index <- scale(ebeh_index)
standardized_ibeh_index <- scale(ibeh_index)
standardized_ace_index <- scale(ace_index)
standardized_psydisorders_index <- scale(psydisorders_index)
standardized_family_index <- scale(family_index)

multivariable_model <- glm(formula = `History of Absenteeism` ~ standardized_ebeh_index
                                + standardized_ibeh_index + standardized_psydisorders_index +
                                  standardized_ace_index + standardized_family_index, data = merged_dataset_subset,
                                family = binomial(link = 'logit'), na.action = na.exclude)

summary(multivariable_model)

Call:
  glm(formula = `History of Absenteeism` ~ standardized_ebeh_index + 
        standardized_ibeh_index + standardized_psydisorders_index + 
        standardized_ace_index + standardized_family_index, family = binomial(link = "logit"), 
      data = merged_dataset_subset, na.action = na.exclude)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.9017  -1.2616   0.8596   1.0315   1.3179  

Coefficients:
                                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)                      0.37834    0.10366   3.650 0.000263 ***
standardized_ebeh_index          0.17154    0.11148   1.539 0.123880    
standardized_ibeh_index          0.31026    0.11290   2.748 0.005995 ** 
standardized_psydisorders_index -0.17287    0.10831  -1.596 0.110467    
standardized_ace_index          -0.03086    0.11184  -0.276 0.782602    
standardized_family_index        0.05786    0.11666   0.496 0.619939    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 538.65  on 397  degrees of freedom
Residual deviance: 527.49  on 392  degrees of freedom
AIC: 539.49

Number of Fisher Scoring iterations: 4

# Calculating OR and CI for Multi_Model

multivariable_model_coef_summary <- summary(multivariable_model)$coefficients
multivariable_model_coef_summary

multivariable_model_odds_ratios <- exp(multi_model_coef_summary[, "Estimate"])
multivariable_model_conf_intervals <- exp(confint(multivariable_model))

multivariable_model_odds_ratios
(Intercept)         ebeh_index         ibeh_index       trauma_index 
0.8179116          1.0659359          1.2850180          0.9636997 
psydisorders_index       family_index 
 0.8730069                1.0432567 

multivariable_model_conf_intervals
                                   2.5 %   97.5 %
(Intercept)                     1.1929130 1.791664
standardized_ebeh_index         0.9551265 1.479911
standardized_ibeh_index         1.0963651 1.708248
standardized_psydisorders_index 0.6792975 1.039505
standardized_ace_index          0.7795629 1.211577
standardized_family_index       0.8433804 1.334076



