Mitch
================
Lewis
13 July 2017

\*\*\*\*Trauma Call in Darwin\*\*\*\*

*Team Activation Criteria as a predictor of severe trauma; an analysis of the sensitivity and specificity of Royal Darwin Hospital's Trauma Team Callout Criteria.*

**Background** RDH is a unique environment. We aim to describe the association of Trauma Call Criteria with patient centred outcomes in a cohort of patients screened for trauma. We also describe the prevalence of individual criteria in the population of trauma patients, and estimate the predictive ability of individual criteria for patient centred outcomes. We discuss the possible implications of simplifying the criteria as if applied to this cohort.

    ## [1] TRUE

    ## 
    ## Alive  Dead 
    ##   727    88

``` r
summary(deathpredictions)
```

    ##    SAPSIIROD         ApacheIIROD       ApacheIIIROD    
    ##  Min.   :0.004584   Min.   :0.00161   Min.   :0.00077  
    ##  1st Qu.:0.052195   1st Qu.:0.09432   1st Qu.:0.04629  
    ##  Median :0.128048   Median :0.24787   Median :0.12870  
    ##  Mean   :0.232380   Mean   :0.30795   Mean   :0.22779  
    ##  3rd Qu.:0.326364   3rd Qu.:0.48001   3rd Qu.:0.34024  
    ##  Max.   :0.975999   Max.   :0.97702   Max.   :0.98426  
    ##  NA's   :4          NA's   :50        NA's   :4

    ##    SAPSIIROD  ApacheIIROD ApacheIIIROD 
    ##    0.5234638    0.4187555    0.5340088

Of patients admitted to ICU that year, 88/815 or 10.8% of patients died, which is well below benchmark mortality as shown by the SMR using various models. Major Trauma occurred in 31.5% of those with trauma call criteria, but still seen in 8.1% of those without trauma call criteria. This is a very influential odds ratio of 5.20 (p value &lt;2.2E-16). When using a larger denominator population who had a low prior probability for major trauma the odds ratio is 17.4 (p value &lt;2.2E-16). This is a slightly trivial result as the odds for severe trauma in patients without trauma is zero, hence the odds ratio for any trauma criterion is infinite when applied to an unselected population. Henceforth we use the cohort of 784 patients who either had severe trauma or were likely to be screened for severe trauma.

``` r
table(traumata$Meets_TCC, traumata$Major_Trauma)
```

    ##        
    ##         FALSE TRUE
    ##   FALSE   257   24
    ##   TRUE    238  108

``` r
#"Intention to treat" style
```

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: traumata$Major_Trauma
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                    Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                 626     645.38              
    ## traumata$Meets_TCC  1   51.797       625     593.58 6.155e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: c(traumata$Major_Trauma, rep(FALSE, 1551 - 784))
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                                               Df Deviance Resid. Df
    ## NULL                                                           1393
    ## c(traumata$Meets_TCC, rep(FALSE, 1551 - 784))  1   215.05      1392
    ##                                               Resid. Dev  Pr(>Chi)    
    ## NULL                                              873.37              
    ## c(traumata$Meets_TCC, rep(FALSE, 1551 - 784))     658.32 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Trauma call criteria were more often seen in those who died, odds ratio 4.09, p=0.004 by Chi squared. For information, the background odds of death for those without a trauma call was 0.01 hence odds of death in those with a trauma call were still only 0.05.

``` r
table(traumata$Meets_TCC, traumata$Hospital_vital_status)
```

    ##        
    ##         Alive Dead
    ##   FALSE   279    2
    ##   TRUE    329   17

``` r
TCCpredictsdeath <- glm(traumata$Hospital_vital_status=='Dead' ~ traumata$Meets_TCC, family = "binomial")

anova(TCCpredictsdeath, test="Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: traumata$Hospital_vital_status == "Dead"
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)    
    ## NULL                                 626     170.29             
    ## traumata$Meets_TCC  1   10.919       625     159.37 0.000952 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

**Prevalence of criteria in the cohort**

``` r
## Quick glance at the cell size for comparisons, make a table, transpose the rows and columns, read off all where cell counts are 5 or fewer
cellcounts <- 
  sapply(traumata[,c(9, 11, 22:37, 39:69)], table) %>%
  tbl_df() %>%
  t()
cellcounts
```

    ##                             [,1] [,2]
    ## Multiple_victims             568   59
    ## CPR                          624    3
    ## MVAejection                  592   35
    ## MVAentrapment                611   16
    ## MVAfatality_at_scene         614   13
    ## Bicycle_v_car                625    2
    ## Ped_v_car                    602   25
    ## Pen_head                     624    3
    ## Pen_neck                     614   13
    ## Pen_torso                    577   50
    ## Crush_head                   625    2
    ## Crush_neck                   627  627
    ## Crush_torso                  619    8
    ## Burns_over_15percent         623    4
    ## Drowning                     627  627
    ## Near_Drown                   624    3
    ## Blast_Injury                 626    1
    ## Fall_morethan_3m             609   18
    ## Airway_compromise            603   22
    ## Intubated                    583   43
    ## Facial_injury                619    7
    ## Airway_burns                 625    1
    ## HR_under_50                  611    2
    ## MD_HR_under_50               617    2
    ## HR_over_120                  556   59
    ## MD_HR_over_120               562   59
    ## SBP_under_90                 563   30
    ## MD_SBP_under_90              592   30
    ## CR_over_2s                   624    1
    ## Pel_Unstab                   619    7
    ## Multiple_fractures           612   14
    ## Amputated_Limb               625    1
    ## RR_under_8                   564    1
    ## MD_RR_under_8                613    1
    ## RR_over_30                   530   37
    ## MD_RR_over_30                578   37
    ## SaO2_under_90                503   23
    ## MD_SaO2_under_90             595   23
    ## Cyanosis                     622    3
    ## Resp_distress                618    7
    ## Flail                        621    5
    ## GCS_under_14                 493   98
    ## MD_GCSunder14                521   98
    ## Agitated                     610   16
    ## Neuro_Deficit                623    3
    ## Seizure                      618    8
    ## Motor_Loss                   624    2
    ## Sens_Loss                    623    3
    ## Significant_multiple_injury  603   23

``` r
## Subset of all columns with non-empty cells
allbutempty <- names(traumata)[c(8, 9, 11, 22:30, 32:33, 35:37, 39:69)]

## Subset of all columns with cell counts more than 5
notsparsedata <- names(traumata)[c(8, 9, 11, 22:24, 26, 28, 29, 32, 37, 39:41, 45:48, 50, 51, 55:58, 59:66, 68, 69)]

## Subset of all columns with cell counts more than 5, using most complete data source and last observation carried onwards
bestdata <- names(traumata)[c(8, 9, 22:24, 26, 28, 29, 32, 37, 39:41, 46, 48, 50, 51, 56, 58, 59:61, 63:66, 68, 69)]

## The most "subjective" criteria are judged to be "significant injury to >2 body areas (69: Significant_multiple_injury)", "respiratory distress (60: Resp_distress)" and "severe facial injury (41: Facial_injury)" and were removed from bestdata for the "less subjective" subset of columns
lesssubjectivedata <- names(traumata)[c(8, 9, 22:24, 26, 28, 29, 32, 37, 39:40, 46, 48, 50, 51, 56, 58, 59, 61, 63:66, 68)]

## Data with close similarity by MeSH heading are grouped, and new predictors using Boolean "OR" are produced then added to the end of the traumata dataset.  This is the first collapse and so is Collapse 1 or CO1
traumata <- traumata %>%
  mutate(CO1_Unequal_Mass = Ped_v_car | Bicycle_v_car,
         CO1_Military = Burns_over_15percent | Blast_Injury,
         CO1_Scene_Complications = CPR | Amputated_Limb | MVAejection | MVAentrapment | Multiple_fractures,
         CO1_Scene_Epidemiology = Near_Drown| Drowning | Fall_morethan_3m | Multiple_victims | MVAfatality_at_scene,
         CO1_Penetrating_midline = Pen_head | Pen_neck | Pen_torso,
         CO1_Crushing_midline = Facial_injury | Crush_head | Crush_neck | Crush_torso | Pel_Unstab | Flail,
         CO1_Airway_criteria = Airway_compromise | Airway_burns | Intubated,
         CO1_Breathing_criteria = MD_RR_under_8 | MD_RR_over_30 | MD_SaO2_under_90 | Cyanosis | Resp_distress,
         CO1_Circ_criteria = CR_over_2s | MD_HR_under_50 | MD_HR_over_120 | MD_SBP_under_90,
         CO1_Neuro_criteria = MD_GCSunder14 | Neuro_Deficit | Seizure | Motor_Loss | Sens_Loss | Agitated)

# This is the second collapse, CO2 and selects criteria from bestdata before collapse
traumata <- traumata %>%
  mutate(CO2_Scene_Complications = MVAejection | MVAentrapment | Multiple_fractures,
         CO2_Scene_Epidemiology = Fall_morethan_3m | Multiple_victims | MVAfatality_at_scene,
         CO2_Penetrating_midline = Pen_neck | Pen_torso,
         CO2_Crushing_midline = Facial_injury | Crush_neck | Crush_torso | Pel_Unstab | Flail,
         
         CO2_Breathing_criteria = MD_RR_over_30 | MD_SaO2_under_90 | Cyanosis | Resp_distress,
         CO2_Circ_criteria = MD_HR_over_120 | MD_SBP_under_90,
         CO2_Neuro_criteria = MD_GCSunder14 | Neuro_Deficit | Seizure | Sens_Loss)
#This is the third collapse.  All possible indicators go into CO3; then The Final Collapse is based on bestdata again, simply collapsed and called The Omega.
traumata <- traumata %>% 
  mutate(CO3_mechanism = Ped_v_car | Bicycle_v_car | Blast_Injury | MVAejection |
           MVAentrapment | Near_Drown | Fall_morethan_3m | Multiple_victims |
           MVAfatality_at_scene | Airway_burns,
         CO3_injury = Amputated_Limb | Multiple_fractures | Burns_over_15percent |
           Pen_head | Pen_neck | Pen_torso | Crush_head | Crush_neck | Crush_torso |
           Pel_Unstab | Facial_injury | Intubated,
         CO3_physiology = CPR | Flail | Airway_compromise | MD_RR_over_30 |
           MD_SaO2_under_90 | Cyanosis | Resp_distress | MD_RR_under_8 | CR_over_2s |
           MD_HR_under_50 | MD_HR_over_120 | MD_SBP_under_90 |
           MD_GCSunder14 | Neuro_Deficit | Seizure | Motor_Loss | Sens_Loss | Agitated)
```

**Independence of data in each category**

``` r
# The correlation matrix of each predictor with each other predictor
predictor_correlations <- apply(traumata[,lesssubjectivedata], 2, cor, traumata[,lesssubjectivedata], use = "pair")
correlationsmax <- apply(traumata[,allbutempty], 2, cor, traumata[,allbutempty], use = "pair")


mantelhaen.test(traumata$Multiple_victims, traumata$SaO2_under_90, traumata$Major_Trauma)
```

    ## 
    ##  Mantel-Haenszel chi-squared test with continuity correction
    ## 
    ## data:  traumata$Multiple_victims and traumata$SaO2_under_90 and traumata$Major_Trauma
    ## Mantel-Haenszel X-squared = 0.49633, df = 1, p-value = 0.4811
    ## alternative hypothesis: true common odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  0.5598649 8.9143050
    ## sample estimates:
    ## common odds ratio 
    ##          2.234011

``` r
mantelhaen.test(traumata$Flail, traumata$SaO2_under_90, traumata$Major_Trauma)
```

    ## 
    ##  Mantel-Haenszel chi-squared test with continuity correction
    ## 
    ## data:  traumata$Flail and traumata$SaO2_under_90 and traumata$Major_Trauma
    ## Mantel-Haenszel X-squared = 0.044864, df = 1, p-value = 0.8323
    ## alternative hypothesis: true common odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  NaN NaN
    ## sample estimates:
    ## common odds ratio 
    ##                 0

``` r
mantelhaen.test(traumata$Major_Trauma, traumata$GCS_under_14, traumata$SaO2_under_90)
```

    ## 
    ##  Mantel-Haenszel chi-squared test with continuity correction
    ## 
    ## data:  traumata$Major_Trauma and traumata$GCS_under_14 and traumata$SaO2_under_90
    ## Mantel-Haenszel X-squared = 109.94, df = 1, p-value < 2.2e-16
    ## alternative hypothesis: true common odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##   8.757023 28.726477
    ## sample estimates:
    ## common odds ratio 
    ##          15.86059

``` r
mantelhaen.test(traumata$Major_Trauma, traumata$Intubated, traumata$SaO2_under_90)
```

    ## 
    ##  Mantel-Haenszel chi-squared test with continuity correction
    ## 
    ## data:  traumata$Major_Trauma and traumata$Intubated and traumata$SaO2_under_90
    ## Mantel-Haenszel X-squared = 99.327, df = 1, p-value < 2.2e-16
    ## alternative hypothesis: true common odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  NaN NaN
    ## sample estimates:
    ## common odds ratio 
    ##               Inf

``` r
mantelhaen.test(traumata$Major_Trauma, traumata$Multiple_victims, traumata$SaO2_under_90)
```

    ## 
    ##  Mantel-Haenszel chi-squared test with continuity correction
    ## 
    ## data:  traumata$Major_Trauma and traumata$Multiple_victims and traumata$SaO2_under_90
    ## Mantel-Haenszel X-squared = 1.8887, df = 1, p-value = 0.1694
    ## alternative hypothesis: true common odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  0.1792556 1.2003012
    ## sample estimates:
    ## common odds ratio 
    ##         0.4638542

**Contribution of Criteria: Linear regression models** Examining the informational contribution of trauma call criteria using multivariable logistic regression. Each component of the trauma call criteria is added sequentially to a multivariable model unless the data are sparse for one of the predictors: because the chi squared test is to be used this means any comparison with a cell count less than 5. Logistic regression is one of the family of general linear models, which use the value of parameters to predict a response, assuming that the relationship across the values of the parameter holds true. In each linear model there's a link function, so for a simple linear model it's the identity function, for logistic regression it's the logit function; and there's an assumption about an error structure for the data, in this case I've chosen binomial. So the model produces a set of coefficients, which in this case are the **log odds ratio** for Major\_Trauma with each of the predictors, "all else being equal". So the odds ratio is their exponent to the base e.

These are the models that are generated from the data above.

``` r
## Using bestdata
maximalmodel <- glm(formula = Major_Trauma ~ .,
                  family = "binomial", data = traumata[, bestdata])
mmsummary <- summary(maximalmodel)


## Using lesssubjectivedata
lesssubjectivemodel <- glm(formula = Major_Trauma ~ .,
                           family = "binomial", data = traumata[, lesssubjectivedata])
lssummary <- summary(lesssubjectivemodel)
#Generate the list of complete cases among the trauma call criteria, redo both models
# traumacompletecrit <- traumata[complete.cases(traumata[,bestdata]),bestdata]
wholemodelcomplete <- glm(formula = Major_Trauma ~ .,
                  family = "binomial",
                  data = traumata[complete.cases(traumata[,bestdata]),bestdata])
wmcsummary <- summary(wholemodelcomplete)

#Using the collapsed criteria in models
collapsedmodel1 <- glm(formula = Major_Trauma ~ CO1_Unequal_Mass + CO1_Military +
                        CO1_Scene_Complications + CO1_Scene_Epidemiology + 
                        CO1_Crushing_midline + CO1_Penetrating_midline + 
                        CO1_Airway_criteria + CO1_Breathing_criteria + CO1_Circ_criteria +
                        CO1_Neuro_criteria + Significant_multiple_injury,
                      family = "binomial", data = traumata)
cm1summary <- summary(collapsedmodel1)

collapsedmodel2 <- glm(formula = Major_Trauma ~ Ped_v_car +
                        CO2_Scene_Complications + CO2_Scene_Epidemiology + 
                        CO1_Crushing_midline + CO1_Penetrating_midline + 
                        Intubated + CO1_Breathing_criteria + CO1_Circ_criteria +
                        CO2_Neuro_criteria + Significant_multiple_injury,
                      family = "binomial", data = traumata)
cm2summary <- summary(collapsedmodel2)
collapsedmodel2b <- glm(formula = Major_Trauma ~ Ped_v_car +
                        CO2_Scene_Complications + CO2_Scene_Epidemiology + 
                        CO1_Crushing_midline + CO1_Penetrating_midline + 
                        (Intubated | Airway_compromise) + CO1_Breathing_criteria + CO1_Circ_criteria +
                        CO2_Neuro_criteria,
                      family = "binomial", data = traumata)
cm2bsummary <- summary(collapsedmodel2b)

collapsedmodel3 <- glm(formula = Major_Trauma ~ CO3_mechanism + CO3_injury + CO3_physiology,
                      family = "binomial", data = traumata)
cm3summary <- summary(collapsedmodel3)
# The above models were about the prediction of "Major Trauma".  To round off, a couple of models about prediction of ISS >15.

collapsedISSmodel2 <- glm(formula = ISS_over_15 ~ Ped_v_car +
                        CO2_Scene_Complications + CO2_Scene_Epidemiology + 
                        CO1_Crushing_midline + CO1_Penetrating_midline + 
                        Intubated + CO1_Breathing_criteria + CO1_Circ_criteria +
                        CO2_Neuro_criteria + Significant_multiple_injury,
                      family = "binomial", data = traumata)
ISS2summary <- summary(collapsedISSmodel2)
```

These are the tests of model: which give the best prediction, which are most affected by collinear variables?

``` r
# Sensory deficit, Neuro deficit and Facial injury have VIF>2 which means their reported standard error is much lower than their actual standard error, meaning they are much less significant than they look.  In fact a VIF over 1.5 is at least a little influential.
drop1(maximalmodel, test="LR")
```

    ## Single term deletions
    ## 
    ## Model:
    ## Major_Trauma ~ Multiple_victims + MVAejection + MVAentrapment + 
    ##     MVAfatality_at_scene + Ped_v_car + Pen_neck + Pen_torso + 
    ##     Crush_torso + Fall_morethan_3m + Airway_compromise + Intubated + 
    ##     Facial_injury + MD_HR_over_120 + MD_SBP_under_90 + Pel_Unstab + 
    ##     Multiple_fractures + MD_RR_over_30 + MD_SaO2_under_90 + Cyanosis + 
    ##     Resp_distress + Flail + MD_GCSunder14 + Agitated + Neuro_Deficit + 
    ##     Seizure + Sens_Loss + Significant_multiple_injury
    ##                             Df Deviance    AIC     LRT  Pr(>Chi)    
    ## <none>                           352.79 408.79                      
    ## Multiple_victims             1   353.97 407.97  1.1787  0.277627    
    ## MVAejection                  1   355.68 409.68  2.8912  0.089064 .  
    ## MVAentrapment                1   352.82 406.82  0.0256  0.872796    
    ## MVAfatality_at_scene         1   352.87 406.87  0.0839  0.772015    
    ## Ped_v_car                    1   353.38 407.38  0.5929  0.441281    
    ## Pen_neck                     1   352.85 406.85  0.0595  0.807257    
    ## Pen_torso                    1   353.01 407.01  0.2178  0.640722    
    ## Crush_torso                  1   352.91 406.91  0.1153  0.734147    
    ## Fall_morethan_3m             1   352.84 406.84  0.0483  0.825993    
    ## Airway_compromise            1   354.35 408.35  1.5557  0.212296    
    ## Intubated                    1   380.84 434.84 28.0514 1.181e-07 ***
    ## Facial_injury                1   352.95 406.95  0.1601  0.689099    
    ## MD_HR_over_120               1   353.39 407.39  0.6027  0.437564    
    ## MD_SBP_under_90              1   352.80 406.80  0.0098  0.921326    
    ## Pel_Unstab                   1   355.67 409.67  2.8782  0.089785 .  
    ## Multiple_fractures           1   353.74 407.74  0.9499  0.329750    
    ## MD_RR_over_30                1   354.35 408.35  1.5641  0.211060    
    ## MD_SaO2_under_90             1   360.00 414.00  7.2054  0.007269 ** 
    ## Cyanosis                     1   353.38 407.38  0.5929  0.441313    
    ## Resp_distress                1   352.79 406.79  0.0000  0.997610    
    ## Flail                        1   368.85 422.85 16.0638 6.124e-05 ***
    ## MD_GCSunder14                1   383.90 437.90 31.1097 2.438e-08 ***
    ## Agitated                     1   359.82 413.82  7.0317  0.008008 ** 
    ## Neuro_Deficit                1   352.79 406.79  0.0021  0.963338    
    ## Seizure                      1   353.01 407.01  0.2161  0.642031    
    ## Sens_Loss                    1   354.06 408.06  1.2655  0.260609    
    ## Significant_multiple_injury  1   377.60 431.60 24.8102 6.326e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
Anova(maximalmodel, test="F")
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: Major_Trauma
    ## Error estimate based on Pearson residuals 
    ## 
    ##                                 SS  Df       F    Pr(>F)    
    ## Multiple_victims              1.18   1  1.3083  0.253182    
    ## MVAejection                   2.89   1  3.2091  0.073753 .  
    ## MVAentrapment                 0.03   1  0.0285  0.866107    
    ## MVAfatality_at_scene          0.08   1  0.0932  0.760283    
    ## Ped_v_car                     0.59   1  0.6582  0.417548    
    ## Pen_neck                      0.06   1  0.0661  0.797247    
    ## Pen_torso                     0.22   1  0.2417  0.623134    
    ## Crush_torso                   0.12   1  0.1280  0.720624    
    ## Fall_morethan_3m              0.05   1  0.0536  0.816919    
    ## Airway_compromise             1.56   1  1.7268  0.189348    
    ## Intubated                    28.05   1 31.1359 3.706e-08 ***
    ## Facial_injury                 0.16   1  0.1777  0.673546    
    ## MD_HR_over_120                0.60   1  0.6689  0.413763    
    ## MD_SBP_under_90               0.01   1  0.0108  0.917164    
    ## Pel_Unstab                    2.88   1  3.1947  0.074402 .  
    ## Multiple_fractures            0.95   1  1.0543  0.304943    
    ## MD_RR_over_30                 1.56   1  1.7361  0.188153    
    ## MD_SaO2_under_90              7.21   1  7.9977  0.004847 ** 
    ## Cyanosis                      0.59   1  0.6581  0.417581    
    ## Resp_distress                 0.00   1  0.0000  0.997483    
    ## Flail                        16.06   1 17.8302 2.807e-05 ***
    ## MD_GCSunder14                31.11   1 34.5306 7.104e-09 ***
    ## Agitated                      7.03   1  7.8049  0.005384 ** 
    ## Neuro_Deficit                 0.00   1  0.0023  0.961393    
    ## Seizure                       0.22   1  0.2399  0.624495    
    ## Sens_Loss                     1.27   1  1.4047  0.236431    
    ## Significant_multiple_injury  24.81   1 27.5383 2.168e-07 ***
    ## Residuals                   518.94 576                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
vif(maximalmodel)
```

    ##            Multiple_victims                 MVAejection 
    ##                    1.213226                    1.138909 
    ##               MVAentrapment        MVAfatality_at_scene 
    ##                    1.565731                    1.182273 
    ##                   Ped_v_car                    Pen_neck 
    ##                    1.295561                    1.027412 
    ##                   Pen_torso                 Crush_torso 
    ##                    1.125489                    1.552020 
    ##            Fall_morethan_3m           Airway_compromise 
    ##                    1.049209                    2.876094 
    ##                   Intubated               Facial_injury 
    ##                    3.509619                    2.029130 
    ##              MD_HR_over_120             MD_SBP_under_90 
    ##                    1.104171                    1.178747 
    ##                  Pel_Unstab          Multiple_fractures 
    ##                    1.720664                    1.210207 
    ##               MD_RR_over_30            MD_SaO2_under_90 
    ##                    1.044210                    1.445553 
    ##                    Cyanosis               Resp_distress 
    ##                    1.000000                    1.852400 
    ##                       Flail               MD_GCSunder14 
    ##                    1.000000                    1.111961 
    ##                    Agitated               Neuro_Deficit 
    ##                    1.136306                    1.750567 
    ##                     Seizure                   Sens_Loss 
    ##                    1.063687                    1.751967 
    ## Significant_multiple_injury 
    ##                    1.395604

``` r
Anova(lesssubjectivemodel, test="F")
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: Major_Trauma
    ## Error estimate based on Pearson residuals 
    ## 
    ##                          SS  Df       F    Pr(>F)    
    ## Multiple_victims       1.22   1  1.3134  0.252255    
    ## MVAejection            3.14   1  3.3752  0.066698 .  
    ## MVAentrapment          0.01   1  0.0125  0.910875    
    ## MVAfatality_at_scene   0.97   1  1.0477  0.306472    
    ## Ped_v_car              0.49   1  0.5217  0.470401    
    ## Pen_neck               0.12   1  0.1277  0.720985    
    ## Pen_torso              0.20   1  0.2182  0.640565    
    ## Crush_torso            0.17   1  0.1794  0.672024    
    ## Fall_morethan_3m       0.08   1  0.0827  0.773755    
    ## Airway_compromise      1.51   1  1.6199  0.203616    
    ## Intubated             29.96   1 32.2233 2.175e-08 ***
    ## MD_HR_over_120         2.10   1  2.2632  0.133022    
    ## MD_SBP_under_90        1.75   1  1.8876  0.170005    
    ## Pel_Unstab             5.29   1  5.6884  0.017398 *  
    ## Multiple_fractures     0.54   1  0.5860  0.444295    
    ## MD_RR_over_30          2.41   1  2.5942  0.107803    
    ## MD_SaO2_under_90       9.03   1  9.7118  0.001922 ** 
    ## Cyanosis               0.77   1  0.8232  0.364630    
    ## Flail                 16.69   1 17.9549 2.632e-05 ***
    ## MD_GCSunder14         29.04   1 31.2321 3.529e-08 ***
    ## Agitated               6.94   1  7.4631  0.006490 ** 
    ## Neuro_Deficit          0.02   1  0.0187  0.891309    
    ## Seizure                0.15   1  0.1634  0.686201    
    ## Sens_Loss              0.96   1  1.0369  0.308976    
    ## Residuals            538.28 579                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
Anova(wholemodelcomplete, test="F")
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: Major_Trauma
    ## Error estimate based on Pearson residuals 
    ## 
    ##                                 SS  Df       F    Pr(>F)    
    ## Multiple_victims              1.18   1  1.3083  0.253182    
    ## MVAejection                   2.89   1  3.2091  0.073753 .  
    ## MVAentrapment                 0.03   1  0.0285  0.866107    
    ## MVAfatality_at_scene          0.08   1  0.0932  0.760283    
    ## Ped_v_car                     0.59   1  0.6582  0.417548    
    ## Pen_neck                      0.06   1  0.0661  0.797247    
    ## Pen_torso                     0.22   1  0.2417  0.623134    
    ## Crush_torso                   0.12   1  0.1280  0.720624    
    ## Fall_morethan_3m              0.05   1  0.0536  0.816919    
    ## Airway_compromise             1.56   1  1.7268  0.189348    
    ## Intubated                    28.05   1 31.1359 3.706e-08 ***
    ## Facial_injury                 0.16   1  0.1777  0.673546    
    ## MD_HR_over_120                0.60   1  0.6689  0.413763    
    ## MD_SBP_under_90               0.01   1  0.0108  0.917164    
    ## Pel_Unstab                    2.88   1  3.1947  0.074402 .  
    ## Multiple_fractures            0.95   1  1.0543  0.304943    
    ## MD_RR_over_30                 1.56   1  1.7361  0.188153    
    ## MD_SaO2_under_90              7.21   1  7.9977  0.004847 ** 
    ## Cyanosis                      0.59   1  0.6581  0.417581    
    ## Resp_distress                 0.00   1  0.0000  0.997483    
    ## Flail                        16.06   1 17.8302 2.807e-05 ***
    ## MD_GCSunder14                31.11   1 34.5306 7.104e-09 ***
    ## Agitated                      7.03   1  7.8049  0.005384 ** 
    ## Neuro_Deficit                 0.00   1  0.0023  0.961393    
    ## Seizure                       0.22   1  0.2399  0.624495    
    ## Sens_Loss                     1.27   1  1.4047  0.236431    
    ## Significant_multiple_injury  24.81   1 27.5383 2.168e-07 ***
    ## Residuals                   518.94 576                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
vif(lesssubjectivemodel)
```

    ##     Multiple_victims          MVAejection        MVAentrapment 
    ##             1.220513             1.156406             1.122563 
    ## MVAfatality_at_scene            Ped_v_car             Pen_neck 
    ##             1.136808             1.576191             1.027549 
    ##            Pen_torso          Crush_torso     Fall_morethan_3m 
    ##             1.115034             1.211722             1.043547 
    ##    Airway_compromise            Intubated       MD_HR_over_120 
    ##             2.449765             2.449765             1.119164 
    ##      MD_SBP_under_90           Pel_Unstab   Multiple_fractures 
    ##             1.218358             1.686580             1.058865 
    ##        MD_RR_over_30     MD_SaO2_under_90             Cyanosis 
    ##             1.065123             1.142197             1.000000 
    ##                Flail        MD_GCSunder14             Agitated 
    ##             1.000000             1.110817             1.074786 
    ##        Neuro_Deficit              Seizure            Sens_Loss 
    ##             1.832112             1.052514             1.831227

``` r
vif(wholemodelcomplete)
```

    ##            Multiple_victims                 MVAejection 
    ##                    1.213226                    1.138909 
    ##               MVAentrapment        MVAfatality_at_scene 
    ##                    1.565731                    1.182273 
    ##                   Ped_v_car                    Pen_neck 
    ##                    1.295561                    1.027412 
    ##                   Pen_torso                 Crush_torso 
    ##                    1.125489                    1.552020 
    ##            Fall_morethan_3m           Airway_compromise 
    ##                    1.049209                    2.876094 
    ##                   Intubated               Facial_injury 
    ##                    3.509619                    2.029130 
    ##              MD_HR_over_120             MD_SBP_under_90 
    ##                    1.104171                    1.178747 
    ##                  Pel_Unstab          Multiple_fractures 
    ##                    1.720664                    1.210207 
    ##               MD_RR_over_30            MD_SaO2_under_90 
    ##                    1.044210                    1.445553 
    ##                    Cyanosis               Resp_distress 
    ##                    1.000000                    1.852400 
    ##                       Flail               MD_GCSunder14 
    ##                    1.000000                    1.111961 
    ##                    Agitated               Neuro_Deficit 
    ##                    1.136306                    1.750567 
    ##                     Seizure                   Sens_Loss 
    ##                    1.063687                    1.751967 
    ## Significant_multiple_injury 
    ##                    1.395604

``` r
# The collapsed models give the results that they report
vif(collapsedmodel1)
```

    ##            CO1_Unequal_Mass                CO1_Military 
    ##                    1.049375                    1.026239 
    ##     CO1_Scene_Complications      CO1_Scene_Epidemiology 
    ##                    1.063707                    1.065615 
    ##        CO1_Crushing_midline     CO1_Penetrating_midline 
    ##                    1.042578                    1.102106 
    ##         CO1_Airway_criteria      CO1_Breathing_criteria 
    ##                    1.086899                    1.035632 
    ##           CO1_Circ_criteria          CO1_Neuro_criteria 
    ##                    1.065500                    1.116627 
    ## Significant_multiple_injury 
    ##                    1.000000

``` r
vif(collapsedmodel2)
```

    ##                   Ped_v_car     CO2_Scene_Complications 
    ##                    1.034282                    1.078086 
    ##      CO2_Scene_Epidemiology        CO1_Crushing_midline 
    ##                    1.072120                    1.040982 
    ##     CO1_Penetrating_midline                   Intubated 
    ##                    1.089983                    1.000000 
    ##      CO1_Breathing_criteria           CO1_Circ_criteria 
    ##                    1.030388                    1.064444 
    ##          CO2_Neuro_criteria Significant_multiple_injury 
    ##                    1.047088                    1.000000

``` r
vif(collapsedmodel3)
```

    ##  CO3_mechanism     CO3_injury CO3_physiology 
    ##       1.012054       1.012969       1.006666

``` r
# The model with the lowest Akaike Information Criterion AIC is the preferred model
c(maximalmodel$aic, wholemodelcomplete$aic, lesssubjectivemodel$aic, collapsedmodel1$aic, collapsedmodel2$aic, collapsedmodel3$aic)
```

    ## [1] 408.7904 408.7904 430.2283 412.0195 402.3764 473.6269

``` r
# The model with the lowest BIC is preferred
c(BIC(maximalmodel), BIC(wholemodelcomplete), BIC(lesssubjectivemodel), BIC(collapsedmodel1), BIC(collapsedmodel2), BIC(collapsedmodel3))
```

    ## [1] 532.0905 532.0905 540.3177 464.8624 450.8157 491.2610

``` r
#Collapsedmodel2 is preferred by BIC and AIC
```

The collapsed criteria have an unquenchable influence of their most important members as seen in the model containing all criteria. Looks like there's no way to cut it that doesn't retain their dominance. One curious feature is that including CPR in "scene complications" doesn't alter either one's significance or influence. Another curious thing is that Intubated doesn't achieve statistical significance in this model, despite being mathematically coupled with the outcome!! What the hell do you have to do to gerrymander a predictor in this cohort?

The next models are tedious and painful but give some idea of the predictive contribution of parameters alone. In the first, only the commonest items are kept and result in a likelihood little lower than with the verbose model *in this sample*. That may not be the case elsewhere, of course; but it is the case with the data we have. The most important terms in *this* model, with log odds ratios of over 20, are a significant injury to more than one area, having been intubated, having a "flail chest" whatever that's worth and near drowning. In the presence of these, the other odds are less impressive. Look below at bulkminimisedmodel to see how this is perhaps not as trustworthy as it seems, then check the coefficients of inversemodel, in which all of the significant predictors from wholemodel have been removed. Agitation, for example, had OR e^(0.32)=1.38, when it's doing more of the heavy lifting that was previously taken by hypoxia or tachypnoea it carries OR e^(1.87)=6.46.

The performance of the models in predicting the outcome "Major\_Trauma" is compared using the likelihood ratio test. The Likelihood is the probability of the data under a hypothesis. In all of the calculations below that is the null hypothesis that the odds ratio is 1. There are limitations to these comparisons. Firstly, every study like this is subject to sampling error even after excluding the sparse data and some of the fitted probabilities are 1 or 0: perfect prediction. These predictors are not used because they're not true. It's particularly annoying that 240 observations are deleted due to missingness of one or more variable even after dropping the sparse cells.

Secondly, all else is not equal. People don't have a middling version of CPR, and the contribution of 2 long bone fractures in the presence of CPR is not independent, it's very much less important than in the absence of CPR. Or more important, who knows? The point is that all models have limitations imposed by the size of the dataset used in deriving the model.

Thirdly there are some obviously contributory ones that are excluded from the model, such as CPR and capillary refill time. A further model needs to be built, the "clinical preference model" where we go down the list and get some colleagues to say think are most useful, and which are least useful. Then we keep the important ones in and drop the rest one by one.

Now with the minimised model a couple of expected things happen: the log likelihood is lower by 22. This is a big difference. So the p value is 4x10^-8. Fair enough, the more elaborate model predicts outcomes more closely. But the Variance Inflation Factor has gone up only very slightly for all the remaining factors because the absolute difference between a likelihood of e^(-184) and a likelihood of e^(-162) is not very much, and there are fewer places for unmeasured variance to hide. These are good things for a model.

``` r
#Close off data manipulations and save current database
write_csv(traumata, paste("traumata", Sys.Date(),".csv", sep = ""))
```
