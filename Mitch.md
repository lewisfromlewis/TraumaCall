Mitch
================
Lewis
13 July 2017

***Trauma Call in Darwin*** *Team Activation Criteria as a predictor of severe trauma; an analysis of the sensitivity and specificity of Royal Darwin Hospital's Trauma Team Callout Criteria.*

**Background** Studies have shown that patients who have suffered severe trauma have improved outcomes when a multidisciplinary team and attendant resources ("Trauma Team") are mobilised from other tasks on patient arrival to the Emergency Department (ED) 1,2. In seeking to maximise the benefit of hospital behaviour at the population level, the benefit to severely injured trauma patients is balanced against the effect of removing resources from other areas. This balance is unlikely to be the same in every trauma centre. For example, in a hospital with a higher proportion of severely injured patients who arrive in the early phase of trauma, plus both a low *rate* and a constant and low illness *severity* of patients presenting with non traumatic illness, the prior probability of severe trauma that is considered by the person performing triage is high, and the impact of Trauma Team callouts on service for other patients is low, easily predicted and easily mitigated. This logic underlies the recommendations of many mature health systems for a single trauma centre covering a primary population of 2.5 million (refs?).

Trauma team composition is not closely specified at a national or international level and therefore differs between institutions. Criteria for trauma team activation also differ mildly between institutions (refs?), as do the characteristics of the patients who arrive at the hospital with trauma and are screened for trauma team activation. The literature also presents a divergent set of clinical and administrative outcomes. The rules by which the Trauma Team is activated have been evaluated by determining their accuracy in the diagnosis of "Severe Trauma" (table / box). Some studies find that mechanism of injury criteria have low accuracy in predicting severe injury 3-5.

Our institution, Royal Darwin Hospital (RDH), is a remote hospital covering a large geographical area with a small population (ref). There is a high incidence of severe trauma but long retrieval times are common \# (ref from Kath's work; if influential we could add a column of "injury time" and do a quick dirty analysis of that) and there is a high population burden of non traumatic illness which is both seasonally varying and unpredictably varying (ref). A dedicated Trauma Service reviews presentations twice daily to identify patients who may have trauma as a reason for admission, maintains a register of current and previous trauma patients and coordinates ongoing care of trauma patients using a nurse led model with close consultant input and 24-hour access to a senior surgical trainee designated as a Trauma Fellow. Trauma patients admitted to ICU are additionally recorded on a binational registry of all ICU patients.

The "Trauma Team Callout Criteria" (TTCC) at Royal Darwin Hospital (RDH) is a list of conditions based on pre hospital mechanism of injury and clinical observations on arrival at hospital. If any one or more of the TTCC are present then a response is activated which is single step and essentially hospital wide (fig1). A second, lower level of response is initiated on meeting less stringent criteria (fig2). Our aim, using standard definitions from the literature, was to evaluate the performance of the TTCC, including estimation of the contribution that individual criteria make to the prediction of Severe Trauma in the unique cohort of patients whom we serve. \# I'm more convinced that the Kohn paper didn't use appropriate statistical methodology. I'll check with even worse regression nerds than I am. The problems are about the probability of measuring one thing if another thing is present, and then constructing a ROC curve as if the number of positive criteria that are needed for an activation can be altered, when they can't.

``` r
library(tidyverse)
library(readxl)
library(car)
library(lmtest)
## Next line is run once then replaced with the .csv file in the repo,as the subsequent line: 
## traumata <- read_xlsx("TraumadataV48.xlsx", sheet = 1, n_max = 784)
traumata <- read_csv("traumata.csv")
```

***Methods*** **Population** A list of all patients presenting to RDH ED in the calendar year of 2015 formed the sample frame. By linkage with the Trauma Registry, ICU Registry, administrative data on mortality and Operating Theatre records we identified 428 patients who had activated a Trauma Call, out of 1712 trauma patients presenting to the Emergency Department and screened using the TTCC. A further 29 patients were confirmed as Severe Trauma but did not meet the TTCC, of whom 14 also did not meet the second tier criteria (CONSORT table). One decision we faced in this evaluation was the choice of the denominator population. The diagnostic contingency table (table) will generate higher values for negative predictive value and specificity as the proportion of patients screened in addition to those with severe trauma.

***Results*** **Population** The severity of the ICU cohort for that year is indicated by the median Risk of Death based on the ANZICS APACHEIII score is given below. The various risk functions give varying risks of death as summarised below, and the standardised mortality ratio across the year is from 0.42 to 0.53 (0.53 using APACHEIII).

``` r
## Set the scene.
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
# screened <- cbind(c(1094, 293), c(29, 135))
# screenedlong <- cbind(c(traumata$`Meets TCC`, rep("no", 1094-327)), c(traumata$`Major Trauma`, rep("no", 1094-293)))
# screenglm <- glm(screened[1], screened[2])
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

``` r
# chisq.test(screened)
```

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
sparsity
```

    ## $`MVA ejection`
    ## 
    ##  no yes 
    ## 744  40 
    ## 
    ## $`MVA entrapment`
    ## 
    ##  no yes 
    ## 759  25 
    ## 
    ## $`MVA fatality at scene`
    ## 
    ##  no yes 
    ## 768  16 
    ## 
    ## $`Bicycle vs car`
    ## 
    ##  no yes 
    ## 781   3 
    ## 
    ## $`Ped vs car`
    ## 
    ##  no yes 
    ## 755  29 
    ## 
    ## $`Pen head`
    ## 
    ##  no yes 
    ## 781   3 
    ## 
    ## $`Pen neck`
    ## 
    ##  no yes 
    ## 769  15 
    ## 
    ## $`Pen torso`
    ## 
    ##  no yes 
    ## 720  64 
    ## 
    ## $`Crush head`
    ## 
    ##  no yes 
    ## 782   2 
    ## 
    ## $`Crush neck`
    ## 
    ##  no 
    ## 784 
    ## 
    ## $`Crush torso`
    ## 
    ##  no yes 
    ## 776   8 
    ## 
    ## $`Burns >15%`
    ## 
    ##  no yes 
    ## 779   5 
    ## 
    ## $Drowning
    ## 
    ##  no 
    ## 784 
    ## 
    ## $`Near Drown`
    ## 
    ##  no yes 
    ## 780   4 
    ## 
    ## $`Blast Injury`
    ## 
    ##  no yes 
    ## 783   1 
    ## 
    ## $`Fall >3m`
    ## 
    ##  no yes 
    ## 758  26 
    ## 
    ## $`Height?`
    ## 
    ##    2    3  3.1  3.5    4    5    7   10 4000 
    ##    1    7    5    1    4    5    1    2    1 
    ## 
    ## $`Air Comp`
    ## 
    ##  no yes 
    ## 753  29 
    ## 
    ## $Intubated
    ## 
    ##  no yes 
    ## 733  50 
    ## 
    ## $`Sev fac inj`
    ## 
    ##  no yes 
    ## 774   9 
    ## 
    ## $`Inhal Burns`
    ## 
    ##  no yes 
    ## 782   1 
    ## 
    ## $`HR<50`
    ## 
    ##  no yes 
    ## 761   4 
    ## 
    ## $`HR>120`
    ## 
    ##  no yes 
    ## 689  77 
    ## 
    ## $`SBP<90`
    ## 
    ##  no yes 
    ## 708  37 
    ## 
    ## $`CR<2s`
    ## 
    ##  no yes 
    ## 780   2 
    ## 
    ## $`Pel Unstab`
    ## 
    ##  no yes 
    ## 772  11 
    ## 
    ## $`>2 LB #s`
    ## 
    ##  no yes 
    ## 766  17 
    ## 
    ## $`Amput Limb`
    ## 
    ##  no yes 
    ## 781   1 
    ## 
    ## $`RR<8`
    ## 
    ##  no yes 
    ## 701   1 
    ## 
    ## $`RR>30`
    ## 
    ##  no yes 
    ## 662  43 
    ## 
    ## $`SaO2<90`
    ## 
    ##  no yes 
    ## 632  29 
    ## 
    ## $Cyanosis
    ## 
    ##  no yes 
    ## 779   3 
    ## 
    ## $`Resp distr`
    ## 
    ##  no yes 
    ## 774   8 
    ## 
    ## $Flail
    ## 
    ##  no yes 
    ## 776   7 
    ## 
    ## $GCSunder14
    ## 
    ##  no yes 
    ## 616 123 
    ## 
    ## $Agitated
    ## 
    ##  no yes 
    ## 762  21 
    ## 
    ## $`Neuro Def`
    ## 
    ##  no yes 
    ## 777   6 
    ## 
    ## $Seizure
    ## 
    ##  no yes 
    ## 772  11 
    ## 
    ## $`Motor Loss`
    ## 
    ##  no yes 
    ## 779   4 
    ## 
    ## $`Sens Loss`
    ## 
    ##  no yes 
    ## 777   6 
    ## 
    ## $`Sig Inj =2`
    ## 
    ##  no yes 
    ## 751  32

``` r
## Excluded by this method (all but Neuro Def had cell counts 5 or fewer; neuro def was 6 but collinear with motor def): CPR + `Motor Loss` + `Sens Loss` + `Neuro Def` + Cyanosis + `RR<8` + `Amput Limb` + `CR<2s` + `HR<50` + `Inhal Burns` + `Blast Injury` + Drowning + `Crush head` + `Crush neck` + `Pen head` + `Bicycle vs car`
TCClist <- as.data.frame(traumata[,c(8,9, 22:30, 32, 33, 35:37, 39:62)])
wholemodel <- glm(formula = `Major Trauma`=='yes' ~ .,
                  family = "binomial", data = TCClist)
# I can't bring myself to delete this after typing the fucker in: wholemodel <- glm(formula = `Major Trauma`=='yes' ~ `multi?` + `MVA ejection` + `MVA entrapment` + `MVA fatality at scene` + `Ped vs car` + `Pen neck` + `Pen torso` + `Crush torso` + `Burns >15%` + `Near Drown` + `Fall >3m` + `Air Comp` + Intubated + `Sev fac inj` + `HR>120` + `SBP<90` + `Pel Unstab` + `RR>30` + `SaO2<90` + `Resp distr` + Flail + `GCSunder14` + Agitated + Seizure + `Sig Inj ≥2` , family = "binomial", data = traumata)
wholemodel
```

    ## 
    ## Call:  glm(formula = `Major Trauma` == "yes" ~ ., family = "binomial", 
    ##     data = TCClist)
    ## 
    ## Coefficients:
    ##                (Intercept)                 `multi?`Yes  
    ##                   -2.38560                    -0.79931  
    ##          `MVA ejection`yes         `MVA entrapment`yes  
    ##                    0.52409                     1.24100  
    ## `MVA fatality at scene`yes         `Bicycle vs car`yes  
    ##                    0.65751                   -16.07044  
    ##            `Ped vs car`yes               `Pen head`yes  
    ##                  -16.32552                   -18.24413  
    ##              `Pen neck`yes              `Pen torso`yes  
    ##                    0.01122                     0.34133  
    ##            `Crush head`yes            `Crush torso`yes  
    ##                  -17.90137                   -18.18047  
    ##            `Burns >15%`yes             `Near Drown`yes  
    ##                    1.33161                    27.85273  
    ##          `Blast Injury`yes               `Fall >3m`yes  
    ##                  -19.51208                    -0.37319  
    ##              `Air Comp`yes                Intubatedyes  
    ##                   -0.25775                    35.10201  
    ##           `Sev fac inj`yes            `Inhal Burns`yes  
    ##                   -0.27910                   -11.89260  
    ##                 `HR<50`yes                 `HR>120`yes  
    ##                   -1.69756                    -0.53999  
    ##                `SBP<90`yes                  `CR<2s`yes  
    ##                   -0.58902                   -33.37284  
    ##            `Pel Unstab`yes               `>2 LB #s`yes  
    ##                  -14.40012                   -16.10972  
    ##            `Amput Limb`yes                   `RR<8`yes  
    ##                   23.49166                    -6.11352  
    ##                 `RR>30`yes                `SaO2<90`yes  
    ##                    0.35504                     3.50952  
    ##                Cyanosisyes             `Resp distr`yes  
    ##                   -7.51563                    -2.31211  
    ##                   Flailyes               GCSunder14yes  
    ##                   22.93482                     2.06782  
    ##                Agitatedyes              `Neuro Def`yes  
    ##                    0.49336                   -33.37284  
    ##                 Seizureyes             `Motor Loss`yes  
    ##                   -0.72740                    41.50533  
    ##             `Sens Loss`yes             `Sig Inj =2`yes  
    ##                   15.19237                    66.56313  
    ## 
    ## Degrees of Freedom: 540 Total (i.e. Null);  501 Residual
    ##   (243 observations deleted due to missingness)
    ## Null Deviance:       520.9 
    ## Residual Deviance: 308.6     AIC: 388.6

Some model diagnostic, as follow, show this is a slightly flaky model. Have I missed any of the trauma call criteria? There are a lot even just in this list. Drop1 reanalyses the model after dropping each one in order. Analysis of variance is the general term for analysing the contribution of each predictor to the total variability in the response.

The inverse model is the one excluding those predictors with most evidence for association based on the F test. Finally, the reduced dataset of only complete cases has to be used for the likelihood ratio test of goodness of model fit, because of the assumption that one model is "nested" in the other. Doing all this, which is quite standard, the inverse model performs almost exactly as well as the full bhoona. This suggests that most of the trauma call criteria are correlated with one another and that at least most of them are redundant, given the rest. At the bottom are the most tedious of all, which I really need a willing slave to do: the one by one correlations of each commonly occurring criterion with the others, stratified by Major Trauma definitions.

``` r
drop1(wholemodel, test="LR")
```

    ## Single term deletions
    ## 
    ## Model:
    ## `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + `MVA entrapment` + 
    ##     `MVA fatality at scene` + `Bicycle vs car` + `Ped vs car` + 
    ##     `Pen head` + `Pen neck` + `Pen torso` + `Crush head` + `Crush torso` + 
    ##     `Burns >15%` + `Near Drown` + `Blast Injury` + `Fall >3m` + 
    ##     `Air Comp` + Intubated + `Sev fac inj` + `Inhal Burns` + 
    ##     `HR<50` + `HR>120` + `SBP<90` + `CR<2s` + `Pel Unstab` + 
    ##     `>2 LB #s` + `Amput Limb` + `RR<8` + `RR>30` + `SaO2<90` + 
    ##     Cyanosis + `Resp distr` + Flail + GCSunder14 + Agitated + 
    ##     `Neuro Def` + Seizure + `Motor Loss` + `Sens Loss` + `Sig Inj =2`
    ##                         Df Deviance    AIC     LRT  Pr(>Chi)    
    ## <none>                       308.64 388.64                      
    ## `multi?`                 1   310.36 388.36  1.7186 0.1898669    
    ## `MVA ejection`           1   309.18 387.18  0.5440 0.4607826    
    ## `MVA entrapment`         1   310.17 388.17  1.5355 0.2152885    
    ## `MVA fatality at scene`  1   309.10 387.10  0.4644 0.4955781    
    ## `Bicycle vs car`         1   308.81 386.81  0.1758 0.6750316    
    ## `Ped vs car`             1   310.90 388.90  2.2629 0.1325085    
    ## `Pen head`               1   309.20 387.20  0.5569 0.4555234    
    ## `Pen neck`               1   308.64 386.64  0.0001 0.9917395    
    ## `Pen torso`              1   309.07 387.07  0.4299 0.5120194    
    ## `Crush head`             1   308.64 386.64  0.0000 0.9997054    
    ## `Crush torso`            1   309.16 387.16  0.5255 0.4685195    
    ## `Burns >15%`             1   309.64 387.64  1.0007 0.3171450    
    ## `Near Drown`             1   308.69 386.69  0.0556 0.8135117    
    ## `Blast Injury`           1   309.16 387.16  0.5215 0.4702144    
    ## `Fall >3m`               1   308.81 386.81  0.1727 0.6777258    
    ## `Air Comp`               1   308.68 386.68  0.0433 0.8350907    
    ## Intubated                1   327.68 405.68 19.0398 1.280e-05 ***
    ## `Sev fac inj`            1   308.64 386.64  0.0000 0.9999586    
    ## `Inhal Burns`            1   308.64 386.64  0.0000 0.9998826    
    ## `HR<50`                  1   308.64 386.64  0.0000 1.0000000    
    ## `HR>120`                 1   309.42 387.42  0.7768 0.3781113    
    ## `SBP<90`                 1   308.94 386.94  0.2969 0.5858279    
    ## `CR<2s`                  1   308.64 386.64  0.0000 0.9998985    
    ## `Pel Unstab`             1   308.81 386.81  0.1758 0.6750308    
    ## `>2 LB #s`               1   309.51 387.51  0.8727 0.3502035    
    ## `Amput Limb`             1   314.23 392.23  5.5959 0.0180022 *  
    ## `RR<8`                   1   308.64 386.64  0.0000 0.9999764    
    ## `RR>30`                  1   308.84 386.84  0.1971 0.6570884    
    ## `SaO2<90`                1   323.18 401.18 14.5431 0.0001370 ***
    ## Cyanosis                 1   308.64 386.64  0.0000 0.9999908    
    ## `Resp distr`             1   310.07 388.07  1.4326 0.2313378    
    ## Flail                    1   323.55 401.55 14.9093 0.0001128 ***
    ## GCSunder14               1   334.25 412.25 25.6079 4.183e-07 ***
    ## Agitated                 1   308.89 386.89  0.2525 0.6153378    
    ## `Neuro Def`              1   308.64 386.64  0.0000 0.9999200    
    ## Seizure                  1   309.23 387.23  0.5932 0.4411881    
    ## `Motor Loss`             1   311.58 389.58  2.9450 0.0861443 .  
    ## `Sens Loss`              1   308.64 386.64  0.0000 0.9999504    
    ## `Sig Inj =2`             1   321.95 399.95 13.3082 0.0002642 ***
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
    ## `multi?`                  1.72   1  1.8256 0.1772592    
    ## `MVA ejection`            0.54   1  0.5778 0.4475170    
    ## `MVA entrapment`          1.54   1  1.6310 0.2021500    
    ## `MVA fatality at scene`   0.46   1  0.4933 0.4827901    
    ## `Bicycle vs car`          0.18   1  0.1867 0.6658551    
    ## `Ped vs car`              2.26   1  2.4037 0.1216824    
    ## `Pen head`                0.56   1  0.5915 0.4421936    
    ## `Pen neck`                0.00   1  0.0001 0.9914906    
    ## `Pen torso`               0.43   1  0.4567 0.4994868    
    ## `Crush head`              0.00   1  0.0000 0.9996965    
    ## `Crush torso`             0.53   1  0.5582 0.4553524    
    ## `Burns >15%`              1.00   1  1.0629 0.3030408    
    ## `Near Drown`              0.06   1  0.0591 0.8080069    
    ## `Blast Injury`            0.52   1  0.5539 0.4570695    
    ## `Fall >3m`                0.17   1  0.1834 0.6686151    
    ## `Air Comp`                0.04   1  0.0460 0.8302008    
    ## Intubated                19.04   1 20.2245 8.566e-06 ***
    ## `Sev fac inj`             0.00   1  0.0000 0.9999573    
    ## `Inhal Burns`             0.00   1  0.0000 0.9998790    
    ## `HR<50`                   0.00   1  0.0000 1.0000000    
    ## `HR>120`                  0.78   1  0.8252 0.3641095    
    ## `SBP<90`                  0.30   1  0.3154 0.5746476    
    ## `CR<2s`                   0.00   1  0.0000 0.9998954    
    ## `Pel Unstab`              0.18   1  0.1867 0.6658544    
    ## `>2 LB #s`                0.87   1  0.9270 0.3361023    
    ## `Amput Limb`              5.60   1  5.9441 0.0151129 *  
    ## `RR<8`                    0.00   1  0.0000 0.9999757    
    ## `RR>30`                   0.20   1  0.2093 0.6474815    
    ## `SaO2<90`                14.54   1 15.4480 9.673e-05 ***
    ## Cyanosis                  0.00   1  0.0000 0.9999905    
    ## `Resp distr`              1.43   1  1.5218 0.2179320    
    ## Flail                    14.91   1 15.8370 7.924e-05 ***
    ## GCSunder14               25.61   1 27.2013 2.687e-07 ***
    ## Agitated                  0.25   1  0.2682 0.6047807    
    ## `Neuro Def`               0.00   1  0.0000 0.9999176    
    ## Seizure                   0.59   1  0.6301 0.4276944    
    ## `Motor Loss`              2.94   1  3.1282 0.0775554 .  
    ## `Sens Loss`               0.00   1  0.0000 0.9999489    
    ## `Sig Inj =2`             13.31   1 14.1363 0.0001901 ***
    ## Residuals               471.65 501                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
vif(wholemodel)
```

    ##                `multi?`          `MVA ejection`        `MVA entrapment` 
    ##                1.196795                1.094691                1.428352 
    ## `MVA fatality at scene`        `Bicycle vs car`            `Ped vs car` 
    ##                1.172913                1.174260                1.880635 
    ##              `Pen head`              `Pen neck`             `Pen torso` 
    ##                1.000000                1.039635                1.134748 
    ##            `Crush head`           `Crush torso`            `Burns >15%` 
    ##                1.135363                1.000000                1.041575 
    ##            `Near Drown`          `Blast Injury`              `Fall >3m` 
    ##            44465.324334                1.000000                1.076300 
    ##              `Air Comp`               Intubated           `Sev fac inj` 
    ##                1.071636                1.926144                1.470940 
    ##           `Inhal Burns`                 `HR<50`                `HR>120` 
    ##                1.042253                1.025218                1.132184 
    ##                `SBP<90`                 `CR<2s`            `Pel Unstab` 
    ##                1.048823                2.117457                2.055923 
    ##              `>2 LB #s`            `Amput Limb`                  `RR<8` 
    ##                2.167165                1.000000            15373.549739 
    ##                 `RR>30`               `SaO2<90`                Cyanosis 
    ##                1.103130                1.517104            59837.874068 
    ##            `Resp distr`                   Flail              GCSunder14 
    ##                1.897773                1.000000                1.219561 
    ##                Agitated             `Neuro Def`                 Seizure 
    ##                1.118765                6.117457                1.155110 
    ##            `Motor Loss`             `Sens Loss`            `Sig Inj =2` 
    ##                2.000000                6.176185                3.774011

``` r
inversemodel <- update(wholemodel, drop.terms(wholemodel$terms, c(3, 10, 12, 13, 18, 19, 21, 22, 25)))
#Generate the list of complete cases among the trauma call criteria, redo both models
traumacompletecrit <- TCClist[complete.cases(TCClist),]
wholemodelcomplete <- glm(formula = `Major Trauma`=='yes' ~ .,
                  family = "binomial", data = traumacompletecrit)
inversemodelcomplete <- update(wholemodelcomplete, drop.terms(wholemodelcomplete$terms, c(3, 10, 12, 13, 18, 19, 21, 22, 25)))
lrtest(wholemodelcomplete, inversemodelcomplete)
```

    ## Likelihood ratio test
    ## 
    ## Model 1: `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + `MVA entrapment` + 
    ##     `MVA fatality at scene` + `Bicycle vs car` + `Ped vs car` + 
    ##     `Pen head` + `Pen neck` + `Pen torso` + `Crush head` + `Crush torso` + 
    ##     `Burns >15%` + `Near Drown` + `Blast Injury` + `Fall >3m` + 
    ##     `Air Comp` + Intubated + `Sev fac inj` + `Inhal Burns` + 
    ##     `HR<50` + `HR>120` + `SBP<90` + `CR<2s` + `Pel Unstab` + 
    ##     `>2 LB #s` + `Amput Limb` + `RR<8` + `RR>30` + `SaO2<90` + 
    ##     Cyanosis + `Resp distr` + Flail + GCSunder14 + Agitated + 
    ##     `Neuro Def` + Seizure + `Motor Loss` + `Sens Loss` + `Sig Inj =2`
    ## Model 2: `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + `MVA fatality at scene` + 
    ##     `Bicycle vs car` + `Ped vs car` + `Pen head` + `Pen neck` + 
    ##     `Pen torso` + `Crush torso` + `Blast Injury` + `Fall >3m` + 
    ##     `Air Comp` + Intubated + `HR<50` + `CR<2s` + `Pel Unstab` + 
    ##     `Amput Limb` + `RR<8` + `RR>30` + `SaO2<90` + Cyanosis + 
    ##     `Resp distr` + Flail + GCSunder14 + Agitated + `Neuro Def` + 
    ##     Seizure + `Motor Loss` + `Sens Loss` + `Sig Inj =2`
    ##   #Df  LogLik Df Chisq Pr(>Chisq)
    ## 1  40 -154.32                    
    ## 2  31 -156.68 -9 4.713     0.8586

``` r
mantelhaen.test(traumata$`Major Trauma`, traumata$`multi?`, traumata$`SaO2<90`)
```

    ## 
    ##  Mantel-Haenszel chi-squared test with continuity correction
    ## 
    ## data:  traumata$`Major Trauma` and traumata$`multi?` and traumata$`SaO2<90`
    ## Mantel-Haenszel X-squared = 0.56569, df = 1, p-value = 0.452
    ## alternative hypothesis: true common odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  0.3056076 1.5019944
    ## sample estimates:
    ## common odds ratio 
    ##         0.6775108

``` r
mantelhaen.test(traumata$`Major Trauma`, traumata$Flail, traumata$`SaO2<90`)
```

    ## 
    ##  Mantel-Haenszel chi-squared test with continuity correction
    ## 
    ## data:  traumata$`Major Trauma` and traumata$Flail and traumata$`SaO2<90`
    ## Mantel-Haenszel X-squared = 22.938, df = 1, p-value = 1.673e-06
    ## alternative hypothesis: true common odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  NaN NaN
    ## sample estimates:
    ## common odds ratio 
    ##               Inf

``` r
mantelhaen.test(traumata$`Major Trauma`, traumata$`GCSunder14`, traumata$`SaO2<90`)
```

    ## 
    ##  Mantel-Haenszel chi-squared test with continuity correction
    ## 
    ## data:  traumata$`Major Trauma` and traumata$GCSunder14 and traumata$`SaO2<90`
    ## Mantel-Haenszel X-squared = 119.69, df = 1, p-value < 2.2e-16
    ## alternative hypothesis: true common odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##   7.687249 21.646141
    ## sample estimates:
    ## common odds ratio 
    ##          12.89958

``` r
mantelhaen.test(traumata$`Major Trauma`, traumata$Intubated, traumata$`SaO2<90`)
```

    ## 
    ##  Mantel-Haenszel chi-squared test with continuity correction
    ## 
    ## data:  traumata$`Major Trauma` and traumata$Intubated and traumata$`SaO2<90`
    ## Mantel-Haenszel X-squared = 119.79, df = 1, p-value < 2.2e-16
    ## alternative hypothesis: true common odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  NaN NaN
    ## sample estimates:
    ## common odds ratio 
    ##               Inf

``` r
mantelhaen.test(traumata$`Major Trauma`, traumata$`multi?`, traumata$`SaO2<90`)
```

    ## 
    ##  Mantel-Haenszel chi-squared test with continuity correction
    ## 
    ## data:  traumata$`Major Trauma` and traumata$`multi?` and traumata$`SaO2<90`
    ## Mantel-Haenszel X-squared = 0.56569, df = 1, p-value = 0.452
    ## alternative hypothesis: true common odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  0.3056076 1.5019944
    ## sample estimates:
    ## common odds ratio 
    ##         0.6775108

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
    `Resp distr` + Flail + `GCSunder14` + Agitated + 
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
    ##     Flail + GCSunder14 + Agitated + Seizure + `Sig Inj =2`, family = "binomial", 
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
    ##                   Flailyes               GCSunder14yes  
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
    ## GCSunder14               28.0149  1  1.204e-07 ***
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
    ##     `SaO2<90` + `Resp distr` + Flail + GCSunder14 + Agitated + 
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
    ## GCSunder14               1   352.54 400.54
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
    ##            `Resp distr`                   Flail              GCSunder14 
    ##                1.880336                1.000000                1.218935 
    ##                Agitated                 Seizure            `Sig Inj =2` 
    ##                1.095107                1.152005                2.492718

``` r
bulkmodelcomplete <- glm(formula = `Major Trauma`=='yes' ~ `multi?` +  
    `MVA ejection` + `MVA entrapment` + 
    `MVA fatality at scene` + 
    `Ped vs car` + `Pen neck` + 
    `Pen torso` + 
    `Burns >15%` + `Near Drown` + 
    `Fall >3m` + `Air Comp` + Intubated + 
    `Sev fac inj` + 
    `HR>120` + `SBP<90` + `Pel Unstab` + 
    `RR>30` + `SaO2<90` + 
    `Resp distr` + Flail + `GCSunder14` + Agitated + 
    Seizure + `Sig Inj ≥2` , 
                  family = "binomial", data = traumacompletecrit)
lrtest(wholemodelcomplete, bulkmodelcomplete)
```

    ## Likelihood ratio test
    ## 
    ## Model 1: `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + `MVA entrapment` + 
    ##     `MVA fatality at scene` + `Bicycle vs car` + `Ped vs car` + 
    ##     `Pen head` + `Pen neck` + `Pen torso` + `Crush head` + `Crush torso` + 
    ##     `Burns >15%` + `Near Drown` + `Blast Injury` + `Fall >3m` + 
    ##     `Air Comp` + Intubated + `Sev fac inj` + `Inhal Burns` + 
    ##     `HR<50` + `HR>120` + `SBP<90` + `CR<2s` + `Pel Unstab` + 
    ##     `>2 LB #s` + `Amput Limb` + `RR<8` + `RR>30` + `SaO2<90` + 
    ##     Cyanosis + `Resp distr` + Flail + GCSunder14 + Agitated + 
    ##     `Neuro Def` + Seizure + `Motor Loss` + `Sens Loss` + `Sig Inj =2`
    ## Model 2: `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + `MVA entrapment` + 
    ##     `MVA fatality at scene` + `Ped vs car` + `Pen neck` + `Pen torso` + 
    ##     `Burns >15%` + `Near Drown` + `Fall >3m` + `Air Comp` + Intubated + 
    ##     `Sev fac inj` + `HR>120` + `SBP<90` + `Pel Unstab` + `RR>30` + 
    ##     `SaO2<90` + `Resp distr` + Flail + GCSunder14 + Agitated + 
    ##     Seizure + `Sig Inj =2`
    ##   #Df  LogLik  Df  Chisq Pr(>Chisq)
    ## 1  40 -154.32                      
    ## 2  25 -161.25 -15 13.867     0.5357

The performance of the models in predicting the outcome "Major Trauma" is compared using the likelihood ratio test. The Likelihood is the probability of the data under a hypothesis. In all of the calculations below that is the null hypothesis that the odds ratio is 1. There are limitations to these comparisons. Firstly, every study like this is subject to sampling error even after excluding the sparse data and some of the fitted probabilities are 1 or 0: perfect prediction. These predictors are not used because they're not true. It's particularly annoying that 240 observations are deleted due to missingness of one or more variable even after dropping the sparse cells.

Secondly, all else is not equal. People don't have a middling version of CPR, and the contribution of 2 long bone fractures in the presence of CPR is not independent, it's very much less important than in the absence of CPR. Or more important, who knows? The point is that all models have limitations imposed by the size of the dataset used in deriving the model.

Thirdly there are some obviously contributory ones that are excluded from the model, such as CPR and capillary refill time. A further model needs to be built, the "clinical preference model" where we go down the list and get some colleagues to say think are most useful, and which are least useful. Then we keep the important ones in and drop the rest one by one.

``` r
## Remove those with VIF>2:  `Sig Inj ≥2` + `Pel Unstab` + `Sev fac inj` +  Intubated + `Ped vs car`
bulkmodelminimised <- glm(formula =`Major Trauma`=='yes' ~ `multi?` + 
                    `MVA ejection` + `MVA entrapment` + 
                    `MVA fatality at scene` + 
                    `Pen neck` + 
                    `Pen torso` + 
                    `Burns >15%` + `Near Drown` + 
                    `Fall >3m` + `Air Comp` +
                    `HR>120` + `SBP<90` +
                    `RR>30` + `SaO2<90` + 
                    `Resp distr` + Flail + `GCSunder14` + Agitated + 
                    Seizure, 
                  family = "binomial", data = traumacompletecrit)
bulkmodelminimised
```

    ## 
    ## Call:  glm(formula = `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + 
    ##     `MVA entrapment` + `MVA fatality at scene` + `Pen neck` + 
    ##     `Pen torso` + `Burns >15%` + `Near Drown` + `Fall >3m` + 
    ##     `Air Comp` + `HR>120` + `SBP<90` + `RR>30` + `SaO2<90` + 
    ##     `Resp distr` + Flail + GCSunder14 + Agitated + Seizure, family = "binomial", 
    ##     data = traumacompletecrit)
    ## 
    ## Coefficients:
    ##                (Intercept)                 `multi?`Yes  
    ##                   -2.35554                    -0.70695  
    ##          `MVA ejection`yes         `MVA entrapment`yes  
    ##                    0.50329                     1.75398  
    ## `MVA fatality at scene`yes               `Pen neck`yes  
    ##                    0.40908                     0.06495  
    ##             `Pen torso`yes             `Burns >15%`yes  
    ##                    0.03269                     0.82463  
    ##            `Near Drown`yes               `Fall >3m`yes  
    ##                   16.26968                     0.04868  
    ##              `Air Comp`yes                 `HR>120`yes  
    ##                    1.47889                    -0.15390  
    ##                `SBP<90`yes                  `RR>30`yes  
    ##                    0.62468                     0.78242  
    ##               `SaO2<90`yes             `Resp distr`yes  
    ##                    3.58429                    -2.84483  
    ##                   Flailyes               GCSunder14yes  
    ##                   19.12321                     2.30362  
    ##                Agitatedyes                  Seizureyes  
    ##                    1.11938                    -0.98441  
    ## 
    ## Degrees of Freedom: 540 Total (i.e. Null);  521 Residual
    ## Null Deviance:       520.9 
    ## Residual Deviance: 366.8     AIC: 406.8

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
    ## NULL                                      540     520.86              
    ## `multi?`                 1    0.844       539     520.02 0.3582839    
    ## `MVA ejection`           1    2.297       538     517.72 0.1296164    
    ## `MVA entrapment`         1    6.895       537     510.83 0.0086429 ** 
    ## `MVA fatality at scene`  1    0.006       536     510.82 0.9370693    
    ## `Pen neck`               1    0.709       535     510.11 0.3998120    
    ## `Pen torso`              1    1.362       534     508.75 0.2431986    
    ## `Burns >15%`             1    0.004       533     508.75 0.9481653    
    ## `Near Drown`             1    6.718       532     502.03 0.0095445 ** 
    ## `Fall >3m`               1    1.093       531     500.94 0.2958810    
    ## `Air Comp`               1   25.649       530     475.29 4.096e-07 ***
    ## `HR>120`                 1    1.492       529     473.79 0.2218928    
    ## `SBP<90`                 1    5.905       528     467.89 0.0150973 *  
    ## `RR>30`                  1   14.276       527     453.61 0.0001579 ***
    ## `SaO2<90`                1   17.207       526     436.41 3.352e-05 ***
    ## `Resp distr`             1    0.213       525     436.19 0.6445721    
    ## Flail                    1   15.889       524     420.31 6.716e-05 ***
    ## GCSunder14               1   50.517       523     369.79 1.182e-12 ***
    ## Agitated                 1    1.632       522     368.16 0.2014535    
    ## Seizure                  1    1.332       521     366.82 0.2484805    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
lrtest(wholemodel, bulkmodelminimised)
```

    ## Likelihood ratio test
    ## 
    ## Model 1: `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + `MVA entrapment` + 
    ##     `MVA fatality at scene` + `Bicycle vs car` + `Ped vs car` + 
    ##     `Pen head` + `Pen neck` + `Pen torso` + `Crush head` + `Crush torso` + 
    ##     `Burns >15%` + `Near Drown` + `Blast Injury` + `Fall >3m` + 
    ##     `Air Comp` + Intubated + `Sev fac inj` + `Inhal Burns` + 
    ##     `HR<50` + `HR>120` + `SBP<90` + `CR<2s` + `Pel Unstab` + 
    ##     `>2 LB #s` + `Amput Limb` + `RR<8` + `RR>30` + `SaO2<90` + 
    ##     Cyanosis + `Resp distr` + Flail + GCSunder14 + Agitated + 
    ##     `Neuro Def` + Seizure + `Motor Loss` + `Sens Loss` + `Sig Inj =2`
    ## Model 2: `Major Trauma` == "yes" ~ `multi?` + `MVA ejection` + `MVA entrapment` + 
    ##     `MVA fatality at scene` + `Pen neck` + `Pen torso` + `Burns >15%` + 
    ##     `Near Drown` + `Fall >3m` + `Air Comp` + `HR>120` + `SBP<90` + 
    ##     `RR>30` + `SaO2<90` + `Resp distr` + Flail + GCSunder14 + 
    ##     Agitated + Seizure
    ##   #Df  LogLik  Df  Chisq Pr(>Chisq)    
    ## 1  40 -154.32                          
    ## 2  20 -183.41 -20 58.186  1.354e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
vif(bulkmodelminimised)
```

    ##                `multi?`          `MVA ejection`        `MVA entrapment` 
    ##                1.230123                1.099475                1.367815 
    ## `MVA fatality at scene`              `Pen neck`             `Pen torso` 
    ##                1.159932                1.039138                1.125445 
    ##            `Burns >15%`            `Near Drown`              `Fall >3m` 
    ##                1.035698                1.000002                1.031846 
    ##              `Air Comp`                `HR>120`                `SBP<90` 
    ##                1.067609                1.169377                1.045699 
    ##                 `RR>30`               `SaO2<90`            `Resp distr` 
    ##                1.126538                1.461701                1.744554 
    ##                   Flail              GCSunder14                Agitated 
    ##                1.000004                1.204690                1.139431 
    ##                 Seizure 
    ##                1.187680

Now with the minimised model a couple of expected things happen: the log likelihood is lower by 22. This is a big difference. So the p value is 4x10^-8. Fair enough, the more elaborate model predicts outcomes more closely. But the Variance Inflation Factor has gone up only very slightly for all the remaining factors because the absolute difference between a likelihood of e^(-184) and a likelihood of e^(-162) is not very much, and there are fewer places for unmeasured variance to hide. These are good things for a model.

``` r
#Close off data manipulations and save current databases
write_csv(traumata, paste("traumata", "Sys.Date()",".csv"))
write_csv(traumacompletecrit, "traumacompletecriteria.csv")
write_csv(Backgroundrate, "Backgroundrate.csv")
```
