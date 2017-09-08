Mitch
================
Lewis
13 July 2017

**Trauma Team Activation Criteria as a predictor of severe trauma; an analysis of the sensitivity and specificity of Royal Darwin Hospital's Trauma Team Callout Criteria.**

*Project Description* *Background* Studies have shown that patients who have suffered severe trauma have improved outcomes when a multidisciplinary team and attendant resources are mobilised from other tasks on patient arrival1,2. In seeking to maximise the benefit of hospital behaviour at the population level, the benefit to severely injured trauma patients is balanced against the effect of removing resources from other areas. This balance is unlikely to be the same in every trauma centre

Trauma team composition is not closely specified at a national or international level and therefore differs between institutions. Criteria for trauma team activation also differ mildly between institutions (refs?), as do the characteristics of the patients who arrive at the hospital with trauma and are screened for trauma team activation. It is inherent in the service specifications that the greatest population benefit would be seen a hospital with a higher proportion of severely injured patients arriving in the early phase of trauma, plus both a low rate and a constant and low illness severity of patients presenting with non traumatic illness. This logic underlies the recommendations of many mature health systems for a single trauma centre covering a primary population of 2.5 million (refs?).

Our institution, Royal Darwin Hospital, is a remote hospital covering a large geographical area with a small population (ref). There is a high incidence of severe trauma but long retrieval times are common (ref from Kath's work; if influential we could add a column of "injury time" and do a quick dirty analysis of that) and there is a high population burden of non traumatic illness which is both seasonally varying and unpredictably varying (ref). The "Trauma Team Callout Criteria" (TTCC) at Royal Darwin Hospital (RDH) include pre hospital (mechanism of injury) and clinical criteria for activation. Mechanism of injury criteria have low predictive value for severe injury3-5 and may lead to a high rate of over-triage for RDH's trauma patients.

``` r
## Set up the workspace
setwd("C:/Users/lewis/Documents/CICM/CICM Formal Projects/Cameron M/TraumaCall")
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(readxl)
library(car)
```

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

``` r
library(lmtest)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
## Next line is run once then replaced with the .csv file in the repo,as the subsequent line: 
## traumata <- read_xlsx("TraumadataV48.xlsx", sheet = 1, n_max = 784)
traumata <- read_csv("traumata.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   `Project ID` = col_double(),
    ##   `ED Arrival Date` = col_datetime(format = ""),
    ##   `Age (Years)` = col_integer(),
    ##   `Triage Category` = col_integer(),
    ##   Pulse = col_integer(),
    ##   Resp = col_integer(),
    ##   SBP = col_integer(),
    ##   SaO2 = col_integer(),
    ##   GCS = col_integer(),
    ##   pulse = col_integer(),
    ##   resp = col_integer(),
    ##   sbp = col_integer(),
    ##   SaO2__1 = col_integer(),
    ##   GCS__1 = col_integer(),
    ##   `Height?` = col_double(),
    ##   ISS = col_integer()
    ## )

    ## See spec(...) for full column specifications.

The severity of the whole cohort for that year is indicated by the median Risk of Death based on the ANZICS APACHEIII score is given below. The various risk functions give varying risks of death as summarised below, and the standardised mortality ratio across the year is from 0.42 to 0.53 (0.53 using APACHEIII).

``` r
## Next line is run once then replaced with the .csv file in the repo
## Backgroundrate <- read_excel("./Cameron M/DeidentNonop2015.xlsx", sheet = 1)
Backgroundrate <- read_csv('Backgroundrate.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   `Date of Birth` = col_datetime(format = ""),
    ##   Age = col_integer(),
    ##   HospAdmDateTime = col_datetime(format = ""),
    ##   HospDisDateTime = col_datetime(format = ""),
    ##   ICUAdmDateTime = col_datetime(format = ""),
    ##   ICUDiscDateTime = col_datetime(format = ""),
    ##   VentHours = col_double(),
    ##   CVVH = col_double(),
    ##   ICULOS = col_double(),
    ##   SAPSIIScore = col_integer(),
    ##   SAPSIIROD = col_double(),
    ##   ApacheIIScore = col_integer(),
    ##   ApacheIIROD = col_double(),
    ##   Apache3Code = col_double(),
    ##   ApacheIIIScore = col_integer(),
    ##   ApacheIIIROD = col_double(),
    ##   PrincipalICUDiagnosisCode = col_integer(),
    ##   UnderlyingICUDiagnosisCode = col_integer(),
    ##   PIM2Score = col_double(),
    ##   PIM2ROD = col_double()
    ##   # ... with 41 more columns
    ## )

    ## See spec(...) for full column specifications.

``` r
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

*Values of Criteria* Examining the informational contribution of trauma call criteria using multivariable logistic regression. Each component of the trauma call criteria is added sequentially to a multivariable model unless the data are sparse for one of the predictors: because the chi squared test is to be used this means any comparison with a cell count less than 5. Logistic regression is one of the family of general linear models, which use the value of parameters to predict a response, assuming that the relationship across the values of the parameter holds true. In each linear model there's a link function, so for a simple linear model it's the identity function, for logistic regression it's the logit function; and there's an assumption about an error structure for the data, in this case I've chosen binomial. So the model produces a set of coefficients, which in this case are the log odds ratio for major trauma with each of the predictors, "all else being equal".

The performance of the models in predicting the outcome "Major Trauma" is compared using the likelihood ratio test. The Likelihood is the probability of the data under a hypothesis. In all of the calculations below that is the null hypothesis that the odds ratio is 1. There are limitations to these comparisons. Firstly, every study like this is subject to sampling error even after excluding the sparse data and some of the fitted probabilities are 1 or 0: perfect prediction. These predictors are not used because they're not true. It's particularly annoying that 240 observations are deleted due to missingness of one or more variable even after dropping the sparse cells.

Secondly, all else is not equal. People don't have a middling version of CPR, and the contribution of 2 long bone fractures in the presence of CPR is not independent, it's very much less important than in the absence of CPR. Or more important, who knows? The point is that all models have limitations imposed by the size of the dataset used in deriving the model.

Thirdly there are some obviously contributory ones that are excluded from the model, such as CPR and capillary refill time. A further model needs to be built, the "clinical preference model" where we go down the list and get some colleagues to say think are most useful, and which are least useful. Then we keep the important ones in and drop the rest one by one.

``` r
## Quick glance at the cell size for comparisons:
str(sapply(traumata[,22:62], table))
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
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
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

``` r
drop1(wholemodel, test="Chisq")
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

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
anova(wholemodel, test="Chisq")
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
    ## `Ped vs car`             1    1.738       538     515.04 0.1874258    
    ## `Pen neck`               1    0.660       537     514.38 0.4164440    
    ## `Pen torso`              1    1.223       536     513.16 0.2688430    
    ## `Crush torso`            1    1.231       535     511.93 0.2671758    
    ## `Burns >15%`             1    0.006       534     511.92 0.9371023    
    ## `Near Drown`             1    6.773       533     505.15 0.0092555 ** 
    ## `Fall >3m`               1    1.172       532     503.98 0.2790913    
    ## `Air Comp`               1   23.569       531     480.41 1.205e-06 ***
    ## Intubated                1   63.440       530     416.97 1.653e-15 ***
    ## `Sev fac inj`            1    1.503       529     415.47 0.2201394    
    ## `HR>120`                 1    1.856       528     413.61 0.1731421    
    ## `SBP<90`                 1    2.274       527     411.34 0.1315556    
    ## `Pel Unstab`             1    1.847       526     409.49 0.1741508    
    ## `RR>30`                  1   10.282       525     399.21 0.0013432 ** 
    ## `SaO2<90`                1   11.403       524     387.80 0.0007331 ***
    ## `Resp distr`             1    0.105       523     387.70 0.7455072    
    ## Flail                    1   18.884       522     368.81 1.389e-05 ***
    ## `GCS =13`                1   27.811       521     341.00 1.338e-07 ***
    ## Agitated                 1    0.239       520     340.76 0.6245990    
    ## Seizure                  1    0.475       519     340.29 0.4908463    
    ## `Sig Inj =2`             1   16.289       518     324.00 5.437e-05 ***
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
## lrtest once further models built
```

``` r
#Close off data manipulations and save current databases
write_csv(traumata, paste("traumata", "Sys.Date()",".csv"))
write_csv(Backgroundrate, "Backgroundrate.csv")
```
