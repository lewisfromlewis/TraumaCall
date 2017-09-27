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

Of patients admitted to ICU that year, 88/815 or 10.8% of patients died, which is well below benchmark mortality as shown by the SMR using various models. Major Trauma occurred in 31.5% of those with trauma call criteria, but still seen in 8.1% of those without trauma call criteria. This is a very influential odds ratio of 5.20 (p value &lt;2.2E-16). When using a larger denominator population who may not have been screened for major trauma the odds ratio is 17.4 (p value &lt;2.2E-16). This is a slightly trivial result as the odds for severe trauma in patients without trauma is zero, hence the odds ratio for any trauma criterion is infinite when applied to an unselected population. Henceforth we use the cohort of 784 patients who either had severe trauma or were likely to be screened for severe trauma.

``` r
table(traumata$Meets_TCC, traumata$Major_Trauma)
```

    ##        
    ##         FALSE TRUE
    ##   FALSE   327   29
    ##   TRUE    293  135

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
    ## NULL                                 783     804.19              
    ## traumata$Meets_TCC  1   69.567       782     734.62 < 2.2e-16 ***
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
    ## NULL                                                           1550
    ## c(traumata$Meets_TCC, rep(FALSE, 1551 - 784))  1   244.04      1549
    ##                                               Resid. Dev  Pr(>Chi)    
    ## NULL                                             1046.96              
    ## c(traumata$Meets_TCC, rep(FALSE, 1551 - 784))     802.92 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Trauma call criteria were more often seen in those who died, odds ratio 4.09, p=0.004 by Chi squared. For information, the background odds of death for those without a trauma call was 0.01 hence odds of death in those with a trauma call were still only 0.05.

``` r
table(traumata$Meets_TCC, traumata$Hospital_vital_status)
```

    ##        
    ##         Alive Dead
    ##   FALSE   352    4
    ##   TRUE    409   19

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
    ## NULL                                 783     207.65            
    ## traumata$Meets_TCC  1   8.2829       782     199.37 0.004002 **
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
    ## Multiple_victims             714   70
    ## CPR                          781    3
    ## MVAejection                  744   40
    ## MVAentrapment                759   25
    ## MVAfatality_at_scene         768   16
    ## Bicycle_v_car                781    3
    ## Ped_v_car                    755   29
    ## Pen_head                     781    3
    ## Pen_neck                     769   15
    ## Pen_torso                    720   64
    ## Crush_head                   782    2
    ## Crush_neck                   784  784
    ## Crush_torso                  776    8
    ## Burns_over_15percent         779    5
    ## Drowning                     784  784
    ## Near_Drown                   780    4
    ## Blast_Injury                 783    1
    ## Fall_morethan_3m             758   26
    ## Airway_compromise            753   29
    ## Intubated                    733   50
    ## Facial_injury                774    9
    ## Airway_burns                 782    1
    ## HR_under_50                  761    4
    ## MD_HR_under_50               771    4
    ## HR_over_120                  689   77
    ## MD_HR_over_120               700   77
    ## SBP_under_90                 708   37
    ## MD_SBP_under_90              740   37
    ## CR_over_2s                   780    2
    ## Pel_Unstab                   772   11
    ## Multiple_fractures           766   17
    ## Amputated_Limb               781    1
    ## RR_under_8                   701    1
    ## MD_RR_under_8                766    1
    ## RR_over_30                   662   43
    ## MD_RR_over_30                726   43
    ## SaO2_under_90                632   29
    ## MD_SaO2_under_90             745   29
    ## Cyanosis                     779    3
    ## Resp_distress                774    8
    ## Flail                        776    7
    ## GCS_under_14                 616  123
    ## MD_GCSunder14                651  123
    ## Agitated                     762   21
    ## Neuro_Deficit                777    6
    ## Seizure                      772   11
    ## Motor_Loss                   779    4
    ## Sens_Loss                    777    6
    ## Significant_multiple_injury  751   32

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

``` r
# This is a doozy.  The correlation matrix of each predictor with each other predictor
predictor_correlations <- apply(traumata[,lesssubjectivedata], 2, cor, traumata[,lesssubjectivedata], use = "pair")

mantelhaen.test(traumata$Multiple_victims, traumata$SaO2_under_90, traumata$Major_Trauma)
```

    ## 
    ##  Mantel-Haenszel chi-squared test with continuity correction
    ## 
    ## data:  traumata$Multiple_victims and traumata$SaO2_under_90 and traumata$Major_Trauma
    ## Mantel-Haenszel X-squared = 4.1469, df = 1, p-value = 0.04171
    ## alternative hypothesis: true common odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##   1.240874 10.953922
    ## sample estimates:
    ## common odds ratio 
    ##          3.686793

``` r
mantelhaen.test(traumata$Flail, traumata$SaO2_under_90, traumata$Major_Trauma)
```

    ## 
    ##  Mantel-Haenszel chi-squared test with continuity correction
    ## 
    ## data:  traumata$Flail and traumata$SaO2_under_90 and traumata$Major_Trauma
    ## Mantel-Haenszel X-squared = 0.38032, df = 1, p-value = 0.5374
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
    ## Mantel-Haenszel X-squared = 119.69, df = 1, p-value < 2.2e-16
    ## alternative hypothesis: true common odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##   7.687249 21.646141
    ## sample estimates:
    ## common odds ratio 
    ##          12.89958

``` r
mantelhaen.test(traumata$Major_Trauma, traumata$Intubated, traumata$SaO2_under_90)
```

    ## 
    ##  Mantel-Haenszel chi-squared test with continuity correction
    ## 
    ## data:  traumata$Major_Trauma and traumata$Intubated and traumata$SaO2_under_90
    ## Mantel-Haenszel X-squared = 119.79, df = 1, p-value < 2.2e-16
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
    ## Mantel-Haenszel X-squared = 0.56569, df = 1, p-value = 0.452
    ## alternative hypothesis: true common odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  0.3056076 1.5019944
    ## sample estimates:
    ## common odds ratio 
    ##         0.6775108

**Contribution of Criteria: Linear regression models** Examining the informational contribution of trauma call criteria using multivariable logistic regression. Each component of the trauma call criteria is added sequentially to a multivariable model unless the data are sparse for one of the predictors: because the chi squared test is to be used this means any comparison with a cell count less than 5. Logistic regression is one of the family of general linear models, which use the value of parameters to predict a response, assuming that the relationship across the values of the parameter holds true. In each linear model there's a link function, so for a simple linear model it's the identity function, for logistic regression it's the logit function; and there's an assumption about an error structure for the data, in this case I've chosen binomial. So the model produces a set of coefficients, which in this case are the **log odds ratio** for Major\_Trauma with each of the predictors, "all else being equal". So the odds ratio is their exponent to the base e.

These are the models that are generated from the data above.

``` r
## Using bestdata
maximalmodel <- glm(formula = Major_Trauma ~ .,
                  family = "binomial", data = traumata[, bestdata])
mmsummary <- summary(maximalmodel)
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
    ##                             Df Deviance    AIC    LRT  Pr(>Chi)    
    ## <none>                           448.85 504.85                     
    ## Multiple_victims             1   449.28 503.28  0.427 0.5134632    
    ## MVAejection                  1   453.02 507.02  4.171 0.0411200 *  
    ## MVAentrapment                1   450.72 504.72  1.864 0.1721466    
    ## MVAfatality_at_scene         1   448.92 502.92  0.069 0.7934966    
    ## Ped_v_car                    1   448.93 502.93  0.081 0.7765579    
    ## Pen_neck                     1   448.93 502.93  0.079 0.7790671    
    ## Pen_torso                    1   449.00 503.00  0.145 0.7031986    
    ## Crush_torso                  1   449.29 503.29  0.436 0.5089062    
    ## Fall_morethan_3m             1   448.89 502.89  0.043 0.8348964    
    ## Airway_compromise            1   448.85 502.85  0.000 0.9849140    
    ## Intubated                    1   480.23 534.23 31.375 2.127e-08 ***
    ## Facial_injury                1   449.01 503.01  0.157 0.6919862    
    ## MD_HR_over_120               1   448.99 502.99  0.136 0.7120036    
    ## MD_SBP_under_90              1   448.85 502.85  0.000 0.9923162    
    ## Pel_Unstab                   1   454.37 508.37  5.522 0.0187801 *  
    ## Multiple_fractures           1   449.59 503.59  0.737 0.3905868    
    ## MD_RR_over_30                1   450.08 504.08  1.225 0.2683393    
    ## MD_SaO2_under_90             1   461.65 515.65 12.804 0.0003459 ***
    ## Cyanosis                     1   450.07 504.07  1.217 0.2699200    
    ## Resp_distress                1   449.42 503.42  0.570 0.4501008    
    ## Flail                        1   472.17 526.17 23.320 1.372e-06 ***
    ## MD_GCSunder14                1   479.05 533.05 30.203 3.891e-08 ***
    ## Agitated                     1   452.15 506.15  3.298 0.0693482 .  
    ## Neuro_Deficit                1   449.08 503.08  0.228 0.6332740    
    ## Seizure                      1   449.16 503.16  0.308 0.5786420    
    ## Sens_Loss                    1   451.26 505.26  2.410 0.1205697    
    ## Significant_multiple_injury  1   481.41 535.41 32.555 1.159e-08 ***
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
    ## Multiple_victims              0.43   1  0.4677 0.4942853    
    ## MVAejection                   4.17   1  4.5682 0.0329046 *  
    ## MVAentrapment                 1.86   1  2.0416 0.1534756    
    ## MVAfatality_at_scene          0.07   1  0.0750 0.7842002    
    ## Ped_v_car                     0.08   1  0.0882 0.7665433    
    ## Pen_neck                      0.08   1  0.0862 0.7691582    
    ## Pen_torso                     0.15   1  0.1590 0.6902085    
    ## Crush_torso                   0.44   1  0.4779 0.4896160    
    ## Fall_morethan_3m              0.04   1  0.0476 0.8273951    
    ## Airway_compromise             0.00   1  0.0004 0.9842177    
    ## Intubated                    31.38   1 34.3623 6.943e-09 ***
    ## Facial_injury                 0.16   1  0.1719 0.6785637    
    ## MD_HR_over_120                0.14   1  0.1493 0.6993575    
    ## MD_SBP_under_90               0.00   1  0.0001 0.9919615    
    ## Pel_Unstab                    5.52   1  6.0476 0.0141574 *  
    ## Multiple_fractures            0.74   1  0.8073 0.3692196    
    ## MD_RR_over_30                 1.23   1  1.3419 0.2470860    
    ## MD_SaO2_under_90             12.80   1 14.0226 0.0001949 ***
    ## Cyanosis                      1.22   1  1.3330 0.2486458    
    ## Resp_distress                 0.57   1  0.6247 0.4295626    
    ## Flail                        23.32   1 25.5402 5.489e-07 ***
    ## MD_GCSunder14                30.20   1 33.0784 1.304e-08 ***
    ## Agitated                      3.30   1  3.6124 0.0577457 .  
    ## Neuro_Deficit                 0.23   1  0.2493 0.6177050    
    ## Seizure                       0.31   1  0.3378 0.5612819    
    ## Sens_Loss                     2.41   1  2.6393 0.1046805    
    ## Significant_multiple_injury  32.55   1 35.6541 3.687e-09 ***
    ## Residuals                   662.89 726                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
vif(maximalmodel)
```

    ##            Multiple_victims                 MVAejection 
    ##                    1.156208                    1.112399 
    ##               MVAentrapment        MVAfatality_at_scene 
    ##                    1.263645                    1.133627 
    ##                   Ped_v_car                    Pen_neck 
    ##                    1.286698                    1.022398 
    ##                   Pen_torso                 Crush_torso 
    ##                    1.121157                    1.295626 
    ##            Fall_morethan_3m           Airway_compromise 
    ##                    1.063420                    1.061102 
    ##                   Intubated               Facial_injury 
    ##                    1.549347                    2.030081 
    ##              MD_HR_over_120             MD_SBP_under_90 
    ##                    1.106141                    1.210248 
    ##                  Pel_Unstab          Multiple_fractures 
    ##                    1.518890                    1.134666 
    ##               MD_RR_over_30            MD_SaO2_under_90 
    ##                    1.049402                    1.318177 
    ##                    Cyanosis               Resp_distress 
    ##                    1.000000                    1.513016 
    ##                       Flail               MD_GCSunder14 
    ##                    1.000000                    1.147364 
    ##                    Agitated               Neuro_Deficit 
    ##                    1.094014                    2.622279 
    ##                     Seizure                   Sens_Loss 
    ##                    1.107421                    2.634202 
    ## Significant_multiple_injury 
    ##                    1.480736

``` r
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

The collapsed criteria have an unquenchable influence of their most important members as seen in the model containing all criteria. Looks like there's no way to cut it that doesn't retain their dominance. One curious feature is that including CPR in "scene complications" doesn't alter either one's significance or influence. Another curious thing is that Intubated doesn't achieve statistical significance in this model, despite being mathematically coupled with the outcome!! What the hell do you have to do to gerrymander a predictor in this cohort?

The next models are tedious and painful but give some idea of the predictive contribution of parameters alone. In the first, only the commonest items are kept and result in a likelihood little lower than with the verbose model *in this sample*. That may not be the case elsewhere, of course; but it is the case with the data we have. The most important terms in *this* model, with log odds ratios of over 20, are a significant injury to more than one area, having been intubated, having a "flail chest" whatever that's worth and near drowning. In the presence of these, the other odds are less impressive. Look below at bulkminimisedmodel to see how this is perhaps not as trustworthy as it seems, then check the coefficients of inversemodel, in which all of the significant predictors from wholemodel have been removed. Agitation, for example, had OR e^(0.32)=1.38, when it's doing more of the heavy lifting that was previously taken by hypoxia or tachypnoea it carries OR e^(1.87)=6.46.

The performance of the models in predicting the outcome "Major\_Trauma" is compared using the likelihood ratio test. The Likelihood is the probability of the data under a hypothesis. In all of the calculations below that is the null hypothesis that the odds ratio is 1. There are limitations to these comparisons. Firstly, every study like this is subject to sampling error even after excluding the sparse data and some of the fitted probabilities are 1 or 0: perfect prediction. These predictors are not used because they're not true. It's particularly annoying that 240 observations are deleted due to missingness of one or more variable even after dropping the sparse cells.

Secondly, all else is not equal. People don't have a middling version of CPR, and the contribution of 2 long bone fractures in the presence of CPR is not independent, it's very much less important than in the absence of CPR. Or more important, who knows? The point is that all models have limitations imposed by the size of the dataset used in deriving the model.

Thirdly there are some obviously contributory ones that are excluded from the model, such as CPR and capillary refill time. A further model needs to be built, the "clinical preference model" where we go down the list and get some colleagues to say think are most useful, and which are least useful. Then we keep the important ones in and drop the rest one by one.

Now with the minimised model a couple of expected things happen: the log likelihood is lower by 22. This is a big difference. So the p value is 4x10^-8. Fair enough, the more elaborate model predicts outcomes more closely. But the Variance Inflation Factor has gone up only very slightly for all the remaining factors because the absolute difference between a likelihood of e^(-184) and a likelihood of e^(-162) is not very much, and there are fewer places for unmeasured variance to hide. These are good things for a model.

``` r
#Close off data manipulations and save current databases
write_csv(traumata, paste("traumata", Sys.Date(),".csv", sep = ""))
write_csv(Backgroundrate, "Backgroundrate.csv")
```
