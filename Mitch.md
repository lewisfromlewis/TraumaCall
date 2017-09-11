Mitch
================
Lewis
13 July 2017

**Trauma Team Activation Criteria as a predictor of severe trauma; an analysis of the sensitivity and specificity of Royal Darwin Hospital's Trauma Team Callout Criteria.**

*Project Description* *Background* Studies have shown that patients who have suffered severe trauma have improved outcomes when a multidisciplinary team and attendant resources are mobilised from other tasks on patient arrival1,2. In seeking to maximise the benefit of hospital behaviour at the population level, the benefit to severely injured trauma patients is balanced against the effect of removing resources from other areas. This balance is unlikely to be the same in every trauma centre

Trauma team composition is not closely specified at a national or international level and therefore differs between institutions. Criteria for trauma team activation also differ mildly between institutions (refs?), as do the characteristics of the patients who arrive at the hospital with trauma and are screened for trauma team activation. It is inherent in the service specifications that the greatest population benefit would be seen a hospital with a higher proportion of severely injured patients arriving in the early phase of trauma, plus both a low rate and a constant and low illness severity of patients presenting with non traumatic illness. This logic underlies the recommendations of many mature health systems for a single trauma centre covering a primary population of 2.5 million (refs?).

Our institution, Royal Darwin Hospital, is a remote hospital covering a large geographical area with a small population (ref). There is a high incidence of severe trauma but long retrieval times are common (ref from Kath's work; if influential we could add a column of "injury time" and do a quick dirty analysis of that) and there is a high population burden of non traumatic illness which is both seasonally varying and unpredictably varying (ref). The "Trauma Team Callout Criteria" (TTCC) at Royal Darwin Hospital (RDH) include pre hospital (mechanism of injury) and clinical criteria for activation. Mechanism of injury criteria have low predictive value for severe injury3-5 and may lead to a high rate of over-triage for RDH's trauma patients.

``` r
library(tidyverse)
library(lubridate)
library(readxl)
library(car)
library(lmtest)
## Next line is run once then replaced with the .csv file in the repo,as the subsequent line: 
## traumata <- read_xlsx("TraumadataV48.xlsx", sheet = 1, n_max = 784)
traumata <- read_csv("traumata.csv")
```

The severity of the whole cohort for that year is indicated by the median Risk of Death based on the ANZICS APACHEIII score is given below. The various risk functions give varying risks of death as summarised below, and the standardised mortality ratio across the year is from 0.42 to 0.53 (0.53 using APACHEIII).

``` r
## Next line is run once then replaced with the .csv file in the repo
## Backgroundrate <- read_excel("./Cameron M/DeidentNonop2015.xlsx", sheet = 1)
Backgroundrate <- read_csv('Backgroundrate.csv')
deathtable <- table(Backgroundrate$ICUVitalStatus)
deathrate <- (deathtable[2]/deathtable[1])
deathpredictions <- Backgroundrate[,c(31, 33, 37)]
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

``` r
predicteddeaths <- sapply(deathpredictions, sum, na.rm=T)
deathprobabilities <- sapply(deathpredictions, sum, na.rm=T)/dim(deathpredictions)[1]
SMR <- deathrate/deathprobabilities
print(SMR)
```

    ##    SAPSIIROD  ApacheIIROD ApacheIIIROD 
    ##    0.5234638    0.4187555    0.5340088

Major trauma occurred in 31.5% of those with trauma call criteria, but still seen in 8.1% of those without trauma call criteria. This is a very influential odds ratio of 5.20 (p value 2.2E-16).

``` r
table(traumata$`Meets TCC`, traumata$`Major Trauma`)
```

    ##      
    ##        no yes
    ##   no  327  29
    ##   yes 293 135

``` r
TCCpredictsmajor <- glm(traumata$`Major Trauma`=='yes' ~ traumata$`Meets TCC`, family="binomial")
anova(TCCpredictsmajor, test="Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: traumata$`Major Trauma` == "yes"
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                      Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                   783     804.19              
    ## traumata$`Meets TCC`  1   69.567       782     734.62 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Trauma call criteria were more often seen in those who died, odds ratio 4.09, p=0.004 by Chi squared. For information, the background odds of death for those without a trauma call was 0.01 hence odds of death in those with a trauma call were still only 0.05.

``` r
table(traumata$`Meets TCC`, traumata$`Discharge Status`)
```

    ##      
    ##       Alive Dead
    ##   no    352    4
    ##   yes   409   19

``` r
TCCpredictsdeath <- glm(traumata$`Discharge Status`=='Dead' ~ traumata$`Meets TCC`, family = "binomial")
anova(TCCpredictsdeath, test="Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: traumata$`Discharge Status` == "Dead"
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                      Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                                   783     207.65            
    ## traumata$`Meets TCC`  1   8.2829       782     199.37 0.004002 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

**Contribution of Criteria** Examining the informational contribution of trauma call criteria using multivariable logistic regression. Each component of the trauma call criteria is added sequentially to a multivariable model unless the data are sparse for one of the predictors: because the chi squared test is to be used this means any comparison with a cell count less than 5. Logistic regression is one of the family of general linear models, which use the value of parameters to predict a response, assuming that the relationship across the values of the parameter holds true. In each linear model there's a link function, so for a simple linear model it's the identity function, for logistic regression it's the logit function; and there's an assumption about an error structure for the data, in this case I've chosen binomial. So the model produces a set of coefficients, which in this case are the **log odds ratio** for major trauma with each of the predictors, "all else being equal". So the odds ratio is their exponent to the base e.

``` r
## Quick glance at the cell size for comparisons:
sparsity <- sapply(traumata[,22:62], table)
str(sparsity)
```

    ## List of 41
    ##  $ MVA ejection         : 'table' int [1:2(1d)] 744 40
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ MVA entrapment       : 'table' int [1:2(1d)] 759 25
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ MVA fatality at scene: 'table' int [1:2(1d)] 768 16
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Bicycle vs car       : 'table' int [1:2(1d)] 781 3
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Ped vs car           : 'table' int [1:2(1d)] 755 29
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Pen head             : 'table' int [1:2(1d)] 781 3
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Pen neck             : 'table' int [1:2(1d)] 769 15
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Pen torso            : 'table' int [1:2(1d)] 720 64
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Crush head           : 'table' int [1:2(1d)] 782 2
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Crush neck           : 'table' int [1(1d)] 784
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr "no"
    ##  $ Crush torso          : 'table' int [1:2(1d)] 776 8
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Burns >15%           : 'table' int [1:2(1d)] 779 5
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Drowning             : 'table' int [1(1d)] 784
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr "no"
    ##  $ Near Drown           : 'table' int [1:2(1d)] 780 4
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Blast Injury         : 'table' int [1:2(1d)] 783 1
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Fall >3m             : 'table' int [1:2(1d)] 758 26
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Height?              : 'table' int [1:9(1d)] 1 7 5 1 4 5 1 2 1
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:9] "2" "3" "3.1" "3.5" ...
    ##  $ Air Comp             : 'table' int [1:2(1d)] 753 29
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Intubated            : 'table' int [1:2(1d)] 733 50
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Sev fac inj          : 'table' int [1:2(1d)] 774 9
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Inhal Burns          : 'table' int [1:2(1d)] 782 1
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ HR<50                : 'table' int [1:2(1d)] 761 4
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ HR>120               : 'table' int [1:2(1d)] 689 77
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ SBP<90               : 'table' int [1:2(1d)] 708 37
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ CR<2s                : 'table' int [1:2(1d)] 780 2
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Pel Unstab           : 'table' int [1:2(1d)] 772 11
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ >2 LB #s             : 'table' int [1:2(1d)] 766 17
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Amput Limb           : 'table' int [1:2(1d)] 781 1
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ RR<8                 : 'table' int [1:2(1d)] 701 1
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ RR>30                : 'table' int [1:2(1d)] 662 43
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ SaO2<90              : 'table' int [1:2(1d)] 632 29
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Cyanosis             : 'table' int [1:2(1d)] 779 3
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Resp distr           : 'table' int [1:2(1d)] 774 8
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Flail                : 'table' int [1:2(1d)] 776 7
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ GCS =13              : 'table' int [1:2(1d)] 616 123
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Agitated             : 'table' int [1:2(1d)] 762 21
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Neuro Def            : 'table' int [1:2(1d)] 777 6
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Seizure              : 'table' int [1:2(1d)] 772 11
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Motor Loss           : 'table' int [1:2(1d)] 779 4
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Sens Loss            : 'table' int [1:2(1d)] 777 6
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"
    ##  $ Sig Inj =2           : 'table' int [1:2(1d)] 751 32
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : chr [1:2] "no" "yes"

``` r
## Excluded by this method (all but Neuro Def had cell counts 5 or fewer; neuro def was 6 but collinear with motor def): CPR + `Motor Loss` + `Sens Loss` + `Neuro Def` + Cyanosis + `RR<8` + `Amput Limb` + `CR<2s` + `HR<50` + `Inhal Burns` + `Blast Injury` + Drowning + `Crush head` + `Crush neck` + `Pen head` + `Bicycle vs car`

wholemodel <- glm(formula = `Major Trauma`=='yes' ~ `multi?` + 
                    `MVA ejection` + `MVA entrapment` + 
                    `MVA fatality at scene` + 
                    `Ped vs car` + `Pen neck` + 
                    `Pen torso` + `Crush torso` + 
                    `Burns >15%` + `Near Drown` + 
                    `Fall >3m` + `Air Comp` + Intubated + 
                    `Sev fac inj` + 
                    `HR>120` + `SBP<90` + `Pel Unstab` + 
                    `RR>30` + `SaO2<90` + 
                    `Resp distr` + Flail + `GCS ≤13` + Agitated + 
                    Seizure + `Sig Inj ≥2` , 
                  family = "binomial", data = traumata)
wholemodel
```

    ## 
    ## Call:  glm(formula = `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + 
    ##     `MVA entrapment` + `MVA fatality at scene` + `Ped vs car` + 
    ##     `Pen neck` + `Pen torso` + `Crush torso` + `Burns >15%` + 
    ##     `Near Drown` + `Fall >3m` + `Air Comp` + Intubated + `Sev fac inj` + 
    ##     `HR>120` + `SBP<90` + `Pel Unstab` + `RR>30` + `SaO2<90` + 
    ##     `Resp distr` + Flail + `GCS =13` + Agitated + Seizure + `Sig Inj =2`, 
    ##     family = "binomial", data = traumata)
    ## 
    ## Coefficients:
    ##                (Intercept)                 `multi?`Yes  
    ##                  -2.386730                   -0.815062  
    ##          `MVA ejection`yes         `MVA entrapment`yes  
    ##                   0.546222                    1.204976  
    ## `MVA fatality at scene`yes             `Ped vs car`yes  
    ##                   0.597657                  -16.119362  
    ##              `Pen neck`yes              `Pen torso`yes  
    ##                   0.003385                    0.179047  
    ##           `Crush torso`yes             `Burns >15%`yes  
    ##                 -18.179338                    0.956604  
    ##            `Near Drown`yes               `Fall >3m`yes  
    ##                  20.484763                    0.183024  
    ##              `Air Comp`yes                Intubatedyes  
    ##                  -0.302175                   46.929072  
    ##           `Sev fac inj`yes                 `HR>120`yes  
    ##                 -14.127002                   -0.019908  
    ##                `SBP<90`yes             `Pel Unstab`yes  
    ##                  -0.688546                  -14.464150  
    ##                 `RR>30`yes                `SaO2<90`yes  
    ##                   0.228598                    3.333962  
    ##            `Resp distr`yes                    Flailyes  
    ##                  -2.066586                   22.795052  
    ##               `GCS =13`yes                 Agitatedyes  
    ##                   2.106583                    0.321155  
    ##                 Seizureyes             `Sig Inj =2`yes  
    ##                  -0.685841                   50.437178  
    ## 
    ## Degrees of Freedom: 543 Total (i.e. Null);  518 Residual
    ##   (240 observations deleted due to missingness)
    ## Null Deviance:       528 
    ## Residual Deviance: 324   AIC: 376

Some model diagnostic, as follow, show this is a slightly flaky model. Have I missed any of the trauma call criteria? There are a lot even just in this list. Drop1 reanalyses the model after dropping each one in order. Analysis of variance is the general term for analysing the contribution of each predictor to the total variability in the response.

The inverse model is the one excluding those predictors with most evidence for association based on the F test. Finally, the reduced dataset of only complete cases has to be used for the likelihood ratio test of goodness of model fit, because of the assumption that one model is "nested" in the other.

``` r
drop1(wholemodel, test="LR")
```

    ## Single term deletions
    ## 
    ## Model:
    ## `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + `MVA entrapment` + 
    ##     `MVA fatality at scene` + `Ped vs car` + `Pen neck` + `Pen torso` + 
    ##     `Crush torso` + `Burns >15%` + `Near Drown` + `Fall >3m` + 
    ##     `Air Comp` + Intubated + `Sev fac inj` + `HR>120` + `SBP<90` + 
    ##     `Pel Unstab` + `RR>30` + `SaO2<90` + `Resp distr` + Flail + 
    ##     `GCS =13` + Agitated + Seizure + `Sig Inj =2`
    ##                         Df Deviance    AIC     LRT  Pr(>Chi)    
    ## <none>                       324.00 376.00                      
    ## `multi?`                 1   325.84 375.84  1.8367 0.1753417    
    ## `MVA ejection`           1   324.59 374.59  0.5906 0.4422026    
    ## `MVA entrapment`         1   325.47 375.47  1.4653 0.2260814    
    ## `MVA fatality at scene`  1   324.40 374.40  0.3958 0.5292908    
    ## `Ped vs car`             1   326.32 376.32  2.3196 0.1277526    
    ## `Pen neck`               1   324.00 374.00  0.0000 0.9975026    
    ## `Pen torso`              1   324.12 374.12  0.1204 0.7285501    
    ## `Crush torso`            1   324.53 374.53  0.5250 0.4687112    
    ## `Burns >15%`             1   324.58 374.58  0.5767 0.4475973    
    ## `Near Drown`             1   325.55 375.55  1.5543 0.2125048    
    ## `Fall >3m`               1   324.06 374.06  0.0576 0.8103355    
    ## `Air Comp`               1   324.06 374.06  0.0599 0.8066490    
    ## Intubated                1   348.20 398.20 24.1966 8.699e-07 ***
    ## `Sev fac inj`            1   324.18 374.18  0.1756 0.6751854    
    ## `HR>120`                 1   324.00 374.00  0.0014 0.9705163    
    ## `SBP<90`                 1   324.40 374.40  0.4016 0.5262640    
    ## `Pel Unstab`             1   324.18 374.18  0.1756 0.6751852    
    ## `RR>30`                  1   324.08 374.08  0.0841 0.7718821    
    ## `SaO2<90`                1   337.45 387.45 13.4468 0.0002454 ***
    ## `Resp distr`             1   325.17 375.17  1.1685 0.2797166    
    ## Flail                    1   338.08 388.08 14.0803 0.0001752 ***
    ## `GCS =13`                1   351.83 401.83 27.8293 1.325e-07 ***
    ## Agitated                 1   324.11 374.11  0.1085 0.7418427    
    ## Seizure                  1   324.54 374.54  0.5383 0.4631484    
    ## `Sig Inj =2`             1   340.29 390.29 16.2893 5.437e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
Anova(wholemodel, test="F")
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: `Major Trauma` == "yes"
    ## Error estimate based on Pearson residuals 
    ## 
    ##                             SS  Df       F    Pr(>F)    
    ## `multi?`                  1.84   1  1.9486 0.1633322    
    ## `MVA ejection`            0.59   1  0.6266 0.4289821    
    ## `MVA entrapment`          1.47   1  1.5547 0.2130103    
    ## `MVA fatality at scene`   0.40   1  0.4199 0.5172850    
    ## `Ped vs car`              2.32   1  2.4610 0.1173147    
    ## `Pen neck`                0.00   1  0.0000 0.9974288    
    ## `Pen torso`               0.12   1  0.1278 0.7208807    
    ## `Crush torso`             0.53   1  0.5570 0.4558031    
    ## `Burns >15%`              0.58   1  0.6119 0.4344359    
    ## `Near Drown`              1.55   1  1.6490 0.1996654    
    ## `Fall >3m`                0.06   1  0.0611 0.8048514    
    ## `Air Comp`                0.06   1  0.0636 0.8010630    
    ## Intubated                24.20   1 25.6716 5.640e-07 ***
    ## `Sev fac inj`             0.18   1  0.1863 0.6661932    
    ## `HR>120`                  0.00   1  0.0014 0.9696461    
    ## `SBP<90`                  0.40   1  0.4261 0.5142076    
    ## `Pel Unstab`              0.18   1  0.1863 0.6661931    
    ## `RR>30`                   0.08   1  0.0892 0.7653499    
    ## `SaO2<90`                13.45   1 14.2666 0.0001771 ***
    ## `Resp distr`              1.17   1  1.2397 0.2660461    
    ## Flail                    14.08   1 14.9387 0.0001252 ***
    ## `GCS =13`                27.83   1 29.5257 8.506e-08 ***
    ## Agitated                  0.11   1  0.1151 0.7345167    
    ## Seizure                   0.54   1  0.5711 0.4501702    
    ## `Sig Inj =2`             16.29   1 17.2823 3.770e-05 ***
    ## Residuals               488.24 518                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
vif(wholemodel)
```

    ##                `multi?`          `MVA ejection`        `MVA entrapment` 
    ##                1.194602                1.092663                1.426140 
    ## `MVA fatality at scene`            `Ped vs car`              `Pen neck` 
    ##                1.163065                2.071638                1.041139 
    ##             `Pen torso`           `Crush torso`            `Burns >15%` 
    ##                1.145071                1.000000                1.036914 
    ##            `Near Drown`              `Fall >3m`              `Air Comp` 
    ##                1.000000                1.053420                1.071200 
    ##               Intubated           `Sev fac inj`                `HR>120` 
    ##                2.857133                2.202900                1.121274 
    ##                `SBP<90`            `Pel Unstab`                 `RR>30` 
    ##                1.049957                2.075182                1.089439 
    ##               `SaO2<90`            `Resp distr`                   Flail 
    ##                1.482421                1.878886                1.000000 
    ##               `GCS =13`                Agitated                 Seizure 
    ##                1.218462                1.094922                1.151768 
    ##            `Sig Inj =2` 
    ##                2.492588

``` r
inversemodel <- update(wholemodel, drop.terms(wholemodel$terms, c(3, 10, 12, 13, 18, 19, 21, 22, 25)))
## lrtest once further models built.  This will need a new data frame containing only complete cases.
```

The next models are tedious and painful but give some idea of the predictive contribution of parameters alone. In the first, only the commonest items are kept and result in a likelihood little lower than with the verbose model *in this sample*. That may not be the case elsewhere, of course; but it is the case with the data we have. The most important terms in *this* model, with log odds ratios of over 20, are a significant injury to more than one area, having been intubated, having a "flail chest" whatever that's worth and near drowning. In the presence of these, the other odds are less impressive. Look below at bulkminimisedmodel to see how this is perhaps not as trustworthy as it seems, then check the coefficients of inversemodel, in which all of the significant predictors from wholemodel have been removed. Agitation, for example, had OR e^(0.32)=1.38, when it's doing more of the heavy lifting that was previously taken by hypoxia or tachypnoea it carries OR e^(1.87)=6.46.

``` r
## Only the commonest items are kept
bulkmodel <- glm(formula = `Major Trauma`=='yes' ~ `multi?` + 
                    `MVA ejection` + `MVA entrapment` + 
                    `MVA fatality at scene` + 
                    `Ped vs car` + `Pen neck` + 
                    `Pen torso` + 
                    `Burns >15%` + `Near Drown` + 
                    `Fall >3m` + `Air Comp` + Intubated + 
                    `Sev fac inj` + 
                    `HR>120` + `SBP<90` + `Pel Unstab` + 
                    `RR>30` + `SaO2<90` + 
                    `Resp distr` + Flail + `GCS ≤13` + Agitated + 
                    Seizure + `Sig Inj ≥2` , 
                  family = "binomial", data = traumata)
bulkmodel
```

    ## 
    ## Call:  glm(formula = `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + 
    ##     `MVA entrapment` + `MVA fatality at scene` + `Ped vs car` + 
    ##     `Pen neck` + `Pen torso` + `Burns >15%` + `Near Drown` + 
    ##     `Fall >3m` + `Air Comp` + Intubated + `Sev fac inj` + `HR>120` + 
    ##     `SBP<90` + `Pel Unstab` + `RR>30` + `SaO2<90` + `Resp distr` + 
    ##     Flail + `GCS =13` + Agitated + Seizure + `Sig Inj =2`, family = "binomial", 
    ##     data = traumata)
    ## 
    ## Coefficients:
    ##                (Intercept)                 `multi?`Yes  
    ##                  -2.397302                   -0.811207  
    ##          `MVA ejection`yes         `MVA entrapment`yes  
    ##                   0.554518                    1.212784  
    ## `MVA fatality at scene`yes             `Ped vs car`yes  
    ##                   0.603020                  -16.108139  
    ##              `Pen neck`yes              `Pen torso`yes  
    ##                   0.009536                    0.186748  
    ##            `Burns >15%`yes             `Near Drown`yes  
    ##                   0.965861                   20.492644  
    ##              `Fall >3m`yes               `Air Comp`yes  
    ##                   0.191498                   -0.299531  
    ##               Intubatedyes            `Sev fac inj`yes  
    ##                  46.902176                  -14.115575  
    ##                `HR>120`yes                 `SBP<90`yes  
    ##                  -0.016595                   -0.687444  
    ##            `Pel Unstab`yes                  `RR>30`yes  
    ##                 -14.452691                    0.231253  
    ##               `SaO2<90`yes             `Resp distr`yes  
    ##                   3.341452                   -2.075477  
    ##                   Flailyes                `GCS =13`yes  
    ##                  22.807772                    2.114653  
    ##                Agitatedyes                  Seizureyes  
    ##                   0.322882                   -0.683620  
    ##            `Sig Inj =2`yes  
    ##                  50.422978  
    ## 
    ## Degrees of Freedom: 543 Total (i.e. Null);  519 Residual
    ##   (240 observations deleted due to missingness)
    ## Null Deviance:       528 
    ## Residual Deviance: 324.5     AIC: 374.5

``` r
Anova(bulkmodel, test="LR")
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: `Major Trauma` == "yes"
    ##                         LR Chisq Df Pr(>Chisq)    
    ## `multi?`                  1.8134  1  0.1781034    
    ## `MVA ejection`            0.6073  1  0.4358119    
    ## `MVA entrapment`          1.4811  1  0.2236028    
    ## `MVA fatality at scene`   0.4019  1  0.5260891    
    ## `Ped vs car`              2.2974  1  0.1295929    
    ## `Pen neck`                0.0001  1  0.9929710    
    ## `Pen torso`               0.1307  1  0.7177111    
    ## `Burns >15%`              0.5868  1  0.4436763    
    ## `Near Drown`              1.5604  1  0.2116039    
    ## `Fall >3m`                0.0629  1  0.8020100    
    ## `Air Comp`                0.0588  1  0.8083727    
    ## Intubated                24.1924  1  8.717e-07 ***
    ## `Sev fac inj`             0.1738  1  0.6767292    
    ## `HR>120`                  0.0009  1  0.9754454    
    ## `SBP<90`                  0.3994  1  0.5273883    
    ## `Pel Unstab`              0.1738  1  0.6767291    
    ## `RR>30`                   0.0859  1  0.7695154    
    ## `SaO2<90`                13.4945  1  0.0002393 ***
    ## `Resp distr`              1.1766  1  0.2780455    
    ## Flail                    14.1231  1  0.0001712 ***
    ## `GCS =13`                28.0149  1  1.204e-07 ***
    ## Agitated                  0.1095  1  0.7406976    
    ## Seizure                   0.5346  1  0.4646925    
    ## `Sig Inj =2`             16.2731  1  5.484e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
drop1(bulkmodel)
```

    ## Single term deletions
    ## 
    ## Model:
    ## `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + `MVA entrapment` + 
    ##     `MVA fatality at scene` + `Ped vs car` + `Pen neck` + `Pen torso` + 
    ##     `Burns >15%` + `Near Drown` + `Fall >3m` + `Air Comp` + Intubated + 
    ##     `Sev fac inj` + `HR>120` + `SBP<90` + `Pel Unstab` + `RR>30` + 
    ##     `SaO2<90` + `Resp distr` + Flail + `GCS =13` + Agitated + 
    ##     Seizure + `Sig Inj =2`
    ##                         Df Deviance    AIC
    ## <none>                       324.53 374.53
    ## `multi?`                 1   326.34 374.34
    ## `MVA ejection`           1   325.13 373.13
    ## `MVA entrapment`         1   326.01 374.01
    ## `MVA fatality at scene`  1   324.93 372.93
    ## `Ped vs car`             1   326.82 374.82
    ## `Pen neck`               1   324.53 372.53
    ## `Pen torso`              1   324.66 372.66
    ## `Burns >15%`             1   325.11 373.11
    ## `Near Drown`             1   326.09 374.09
    ## `Fall >3m`               1   324.59 372.59
    ## `Air Comp`               1   324.58 372.58
    ## Intubated                1   348.72 396.72
    ## `Sev fac inj`            1   324.70 372.70
    ## `HR>120`                 1   324.53 372.53
    ## `SBP<90`                 1   324.92 372.92
    ## `Pel Unstab`             1   324.70 372.70
    ## `RR>30`                  1   324.61 372.61
    ## `SaO2<90`                1   338.02 386.02
    ## `Resp distr`             1   325.70 373.70
    ## Flail                    1   338.65 386.65
    ## `GCS =13`                1   352.54 400.54
    ## Agitated                 1   324.63 372.63
    ## Seizure                  1   325.06 373.06
    ## `Sig Inj =2`             1   340.80 388.80

``` r
vif(bulkmodel)
```

    ##                `multi?`          `MVA ejection`        `MVA entrapment` 
    ##                1.195985                1.093123                1.427094 
    ## `MVA fatality at scene`            `Ped vs car`              `Pen neck` 
    ##                1.163704                2.071997                1.041491 
    ##             `Pen torso`            `Burns >15%`            `Near Drown` 
    ##                1.145853                1.037077                1.000000 
    ##              `Fall >3m`              `Air Comp`               Intubated 
    ##                1.053305                1.071366                2.858182 
    ##           `Sev fac inj`                `HR>120`                `SBP<90` 
    ##                2.203613                1.122027                1.050390 
    ##            `Pel Unstab`                 `RR>30`               `SaO2<90` 
    ##                2.075290                1.089769                1.482869 
    ##            `Resp distr`                   Flail               `GCS =13` 
    ##                1.880336                1.000000                1.218935 
    ##                Agitated                 Seizure            `Sig Inj =2` 
    ##                1.095107                1.152005                2.492718

``` r
lrtest(wholemodel, bulkmodel)
```

    ## Likelihood ratio test
    ## 
    ## Model 1: `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + `MVA entrapment` + 
    ##     `MVA fatality at scene` + `Ped vs car` + `Pen neck` + `Pen torso` + 
    ##     `Crush torso` + `Burns >15%` + `Near Drown` + `Fall >3m` + 
    ##     `Air Comp` + Intubated + `Sev fac inj` + `HR>120` + `SBP<90` + 
    ##     `Pel Unstab` + `RR>30` + `SaO2<90` + `Resp distr` + Flail + 
    ##     `GCS =13` + Agitated + Seizure + `Sig Inj =2`
    ## Model 2: `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + `MVA entrapment` + 
    ##     `MVA fatality at scene` + `Ped vs car` + `Pen neck` + `Pen torso` + 
    ##     `Burns >15%` + `Near Drown` + `Fall >3m` + `Air Comp` + Intubated + 
    ##     `Sev fac inj` + `HR>120` + `SBP<90` + `Pel Unstab` + `RR>30` + 
    ##     `SaO2<90` + `Resp distr` + Flail + `GCS =13` + Agitated + 
    ##     Seizure + `Sig Inj =2`
    ##   #Df  LogLik Df Chisq Pr(>Chisq)
    ## 1  26 -162.00                    
    ## 2  25 -162.26 -1 0.525     0.4687

The performance of the models in predicting the outcome "Major Trauma" is compared using the likelihood ratio test. The Likelihood is the probability of the data under a hypothesis. In all of the calculations below that is the null hypothesis that the odds ratio is 1. There are limitations to these comparisons. Firstly, every study like this is subject to sampling error even after excluding the sparse data and some of the fitted probabilities are 1 or 0: perfect prediction. These predictors are not used because they're not true. It's particularly annoying that 240 observations are deleted due to missingness of one or more variable even after dropping the sparse cells.

Secondly, all else is not equal. People don't have a middling version of CPR, and the contribution of 2 long bone fractures in the presence of CPR is not independent, it's very much less important than in the absence of CPR. Or more important, who knows? The point is that all models have limitations imposed by the size of the dataset used in deriving the model.

Thirdly there are some obviously contributory ones that are excluded from the model, such as CPR and capillary refill time. A further model needs to be built, the "clinical preference model" where we go down the list and get some colleagues to say think are most useful, and which are least useful. Then we keep the important ones in and drop the rest one by one.

``` r
## Remove those with VIF>2:  `Sig Inj ≥2` + `Pel Unstab` + `Sev fac inj` +  Intubated + `Ped vs car`
bulkmodelminimised <- glm(formula = `Major Trauma`=='yes' ~ `multi?` + 
                    `MVA ejection` + `MVA entrapment` + 
                    `MVA fatality at scene` + 
                    `Pen neck` + 
                    `Pen torso` + 
                    `Burns >15%` + `Near Drown` + 
                    `Fall >3m` + `Air Comp` +
                    `HR>120` + `SBP<90` +
                    `RR>30` + `SaO2<90` + 
                    `Resp distr` + Flail + `GCS ≤13` + Agitated + 
                    Seizure, 
                  family = "binomial", data = traumata)
bulkmodelminimised
```

    ## 
    ## Call:  glm(formula = `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + 
    ##     `MVA entrapment` + `MVA fatality at scene` + `Pen neck` + 
    ##     `Pen torso` + `Burns >15%` + `Near Drown` + `Fall >3m` + 
    ##     `Air Comp` + `HR>120` + `SBP<90` + `RR>30` + `SaO2<90` + 
    ##     `Resp distr` + Flail + `GCS =13` + Agitated + Seizure, family = "binomial", 
    ##     data = traumata)
    ## 
    ## Coefficients:
    ##                (Intercept)                 `multi?`Yes  
    ##                  -2.364891                   -0.740776  
    ##          `MVA ejection`yes         `MVA entrapment`yes  
    ##                   0.581043                    1.760339  
    ## `MVA fatality at scene`yes               `Pen neck`yes  
    ##                   0.399738                    0.068319  
    ##             `Pen torso`yes             `Burns >15%`yes  
    ##                  -0.001567                    0.797563  
    ##            `Near Drown`yes               `Fall >3m`yes  
    ##                  16.238189                    0.048544  
    ##              `Air Comp`yes                 `HR>120`yes  
    ##                   1.435532                   -0.029468  
    ##                `SBP<90`yes                  `RR>30`yes  
    ##                   0.593704                    0.794847  
    ##               `SaO2<90`yes             `Resp distr`yes  
    ##                   3.566357                   -2.802702  
    ##                   Flailyes                `GCS =13`yes  
    ##                  19.078332                    2.368810  
    ##                Agitatedyes                  Seizureyes  
    ##                   1.074463                   -1.006202  
    ## 
    ## Degrees of Freedom: 543 Total (i.e. Null);  524 Residual
    ##   (240 observations deleted due to missingness)
    ## Null Deviance:       528 
    ## Residual Deviance: 369   AIC: 409

``` r
anova(bulkmodelminimised, test="Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: `Major Trauma` == "yes"
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                         Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                      543     527.97              
    ## `multi?`                 1    0.934       542     527.03 0.3337640    
    ## `MVA ejection`           1    3.329       541     523.70 0.0680647 .  
    ## `MVA entrapment`         1    6.916       540     516.79 0.0085434 ** 
    ## `MVA fatality at scene`  1    0.004       539     516.78 0.9476930    
    ## `Pen neck`               1    0.730       538     516.05 0.3927427    
    ## `Pen torso`              1    1.427       537     514.62 0.2323013    
    ## `Burns >15%`             1    0.003       536     514.62 0.9562712    
    ## `Near Drown`             1    6.680       535     507.94 0.0097520 ** 
    ## `Fall >3m`               1    1.043       534     506.90 0.3071057    
    ## `Air Comp`               1   25.142       533     481.76 5.326e-07 ***
    ## `HR>120`                 1    2.344       532     479.41 0.1257928    
    ## `SBP<90`                 1    5.836       531     473.58 0.0156989 *  
    ## `RR>30`                  1   15.096       530     458.48 0.0001022 ***
    ## `SaO2<90`                1   16.414       529     442.07 5.091e-05 ***
    ## `Resp distr`             1    0.194       528     441.87 0.6595180    
    ## Flail                    1   15.587       527     426.29 7.880e-05 ***
    ## `GCS =13`                1   54.437       526     371.85 1.605e-13 ***
    ## Agitated                 1    1.462       525     370.39 0.2265428    
    ## Seizure                  1    1.400       524     368.99 0.2367844    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
lrtest(bulkmodel, bulkmodelminimised)
```

    ## Likelihood ratio test
    ## 
    ## Model 1: `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + `MVA entrapment` + 
    ##     `MVA fatality at scene` + `Ped vs car` + `Pen neck` + `Pen torso` + 
    ##     `Burns >15%` + `Near Drown` + `Fall >3m` + `Air Comp` + Intubated + 
    ##     `Sev fac inj` + `HR>120` + `SBP<90` + `Pel Unstab` + `RR>30` + 
    ##     `SaO2<90` + `Resp distr` + Flail + `GCS =13` + Agitated + 
    ##     Seizure + `Sig Inj =2`
    ## Model 2: `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + `MVA entrapment` + 
    ##     `MVA fatality at scene` + `Pen neck` + `Pen torso` + `Burns >15%` + 
    ##     `Near Drown` + `Fall >3m` + `Air Comp` + `HR>120` + `SBP<90` + 
    ##     `RR>30` + `SaO2<90` + `Resp distr` + Flail + `GCS =13` + 
    ##     Agitated + Seizure
    ##   #Df  LogLik Df  Chisq Pr(>Chisq)    
    ## 1  25 -162.26                         
    ## 2  20 -184.49 -5 44.462  1.867e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
lrtest(wholemodel, bulkmodelminimised)
```

    ## Likelihood ratio test
    ## 
    ## Model 1: `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + `MVA entrapment` + 
    ##     `MVA fatality at scene` + `Ped vs car` + `Pen neck` + `Pen torso` + 
    ##     `Crush torso` + `Burns >15%` + `Near Drown` + `Fall >3m` + 
    ##     `Air Comp` + Intubated + `Sev fac inj` + `HR>120` + `SBP<90` + 
    ##     `Pel Unstab` + `RR>30` + `SaO2<90` + `Resp distr` + Flail + 
    ##     `GCS =13` + Agitated + Seizure + `Sig Inj =2`
    ## Model 2: `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + `MVA entrapment` + 
    ##     `MVA fatality at scene` + `Pen neck` + `Pen torso` + `Burns >15%` + 
    ##     `Near Drown` + `Fall >3m` + `Air Comp` + `HR>120` + `SBP<90` + 
    ##     `RR>30` + `SaO2<90` + `Resp distr` + Flail + `GCS =13` + 
    ##     Agitated + Seizure
    ##   #Df  LogLik Df  Chisq Pr(>Chisq)    
    ## 1  26 -162.00                         
    ## 2  20 -184.49 -6 44.986  4.709e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
vif(bulkmodelminimised)
```

    ##                `multi?`          `MVA ejection`        `MVA entrapment` 
    ##                1.226004                1.104676                1.368707 
    ## `MVA fatality at scene`              `Pen neck`             `Pen torso` 
    ##                1.158801                1.040262                1.122155 
    ##            `Burns >15%`            `Near Drown`              `Fall >3m` 
    ##                1.036571                1.000002                1.032397 
    ##              `Air Comp`                `HR>120`                `SBP<90` 
    ##                1.064476                1.147697                1.044163 
    ##                 `RR>30`               `SaO2<90`            `Resp distr` 
    ##                1.125571                1.458660                1.743918 
    ##                   Flail               `GCS =13`                Agitated 
    ##                1.000004                1.200472                1.134829 
    ##                 Seizure 
    ##                1.187744

Now with the minimised model a couple of expected things happen: the log likelihood is lower by 22. This is a big difference. So the p value is 4x10^-8. Fair enough, the more elaborate model predicts outcomes more closely. But the Variance Inflation Factor has gone up only very slightly for all the remaining factors because the absolute difference between a likelihood of e^(-184) and a likelihood of e^(-162) is not very much, and there are fewer places for unmeasured variance to hide. These are good things for a model.

``` r
#Close off data manipulations and save current databases
write_csv(traumata, paste("traumata", "Sys.Date()",".csv"))
write_csv(Backgroundrate, "Backgroundrate.csv")
```
