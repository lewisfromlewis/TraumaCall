# Trauma Call Criteria at RDH

### Background
Studies have shown that patients who have suffered severe trauma have improved outcomes when a multidisciplinary team and attendant resources ("Trauma Team") are mobilised from other tasks on patient arrival to the Emergency Department (ED) 1,2.  In seeking to maximise the benefit of hospital behaviour at the population level, the benefit to severely injured trauma patients is balanced against the effect of removing resources from other areas.  This balance is unlikely to be the same in every trauma centre.  For example, in a hospital with a higher proportion of severely injured patients who arrive in the early phase of trauma, plus both a low *rate* and a constant and low illness *severity* of patients presenting with non traumatic illness, the prior probability of severe trauma that is considered by the person performing triage is high, and the impact of Trauma Team callouts on service for other patients is low, easily predicted and easily mitigated.  This logic underlies the recommendations of many mature health systems for a single trauma centre covering a primary population of 2.5 million (refs?).

Trauma team composition is not closely specified at a national or international level and therefore differs between institutions.  Criteria for trauma team activation also differ mildly between institutions (refs?), as do the characteristics of the patients who arrive at the hospital with trauma and are screened for trauma team activation. The literature also presents a divergent set of clinical and administrative outcomes. The rules by which the Trauma Team is activated have been evaluated by determining their accuracy in the diagnosis of "Severe Trauma" (table / box). Some studies find that mechanism of injury criteria have low accuracy in predicting severe injury 3-5.

Our institution, Royal Darwin Hospital (RDH), is a remote hospital covering a large geographical area with a small population (ref).  There is a high incidence of severe trauma but long retrieval times are common (ref) and there is a high population burden of non traumatic illness which is both seasonally varying and unpredictably varying (ref).  A dedicated Trauma Service reviews presentations twice daily to identify patients who may have trauma as a reason for admission, maintains a register of current and previous trauma patients and coordinates ongoing care of trauma patients using a nurse led model with close consultant input and 24-hour access to a senior surgical trainee designated as a Trauma Fellow.  Trauma patients admitted to ICU are additionally recorded on a binational registry of all ICU patients.

The "Trauma Team Callout Criteria" (TTCC) at Royal Darwin Hospital (RDH) is a list of conditions based on pre hospital mechanism of injury and clinical observations on arrival at hospital.  If any one or more of the TTCC are present then a response is activated which is single step and essentially hospital wide (fig1).  A second, lower level of response is initiated on meeting less stringent criteria (fig2).  Our aim, using standard definitions from the literature, was to evaluate the performance of the TTCC, including estimation of the contribution that individual criteria make to the prediction of Severe Trauma in the unique cohort of patients whom we serve.



```{r}
# The severity of the ICU cohort for that year is indicated by the median Risk of Death based on the ANZICS APACHEIII score is given below.  The various risk functions give varying risks of death as summarised below, and the standardised mortality ratio across the year is from 0.42 to 0.53 (0.53 using APACHEIII).
## Next line is run once then replaced with the .csv file in the repo,as the subsequent line: 
## Backgroundrate <- read_excel("./Cameron M/DeidentNonop2015.xlsx", sheet = 1)
Backgroundrate <- read_csv('Backgroundrate.csv')
deathtable <- table(Backgroundrate$ICUVitalStatus)
deathrate <- (deathtable[2]/deathtable[1])
deathpredictions <- Backgroundrate[,c(31, 33, 37)]
summary(deathpredictions)
predicteddeaths <- sapply(deathpredictions, sum, na.rm=T)
deathprobabilities <- sapply(deathpredictions, sum, na.rm=T)/dim(Deathpredictions)[1]
SMR <- deathrate/deathprobabilities
print(SMR)
```
### Methods
**Population**
A list of all patients presenting to RDH ED in the calendar year of 2015 formed the sample frame.  By linkage with the Trauma Registry, ICU Registry, administrative data on mortality and Operating Theatre records we identified 428 patients who had activated a Trauma Call, out of 1712 trauma patients presenting to the Emergency Department and screened using the TTCC.  A further 29 patients were confirmed as Severe Trauma but did not meet the TTCC, of whom 14 also did not meet the second tier criteria (CONSORT table). One decision we faced in this evaluation was the choice of the denominator population.  The diagnostic contingency table (table) will generate higher values for negative predictive value and specificity as the number of patients in addition to those with severe trauma increases.  Given that trauma call criteria are not formally applied to all patients who have sustained a mechanical injury, the predictive value of interest is the ability of the criteria to activate a trauma team when necessary and to avoid such activation when not necessary.  For that reason we chose a denominator population of 784, composed of ...
For comparison the ICU outcomes for that year were calculated.

## Results
**Population**
The severity of the ICU cohort for that year is indicated by the median Risk of Death based on the ANZICS APACHEIII score is given below.  The various risk functions give varying risks of death as summarised below, and the standardised mortality ratio across the year is from 0.42 to 0.53 (0.53 using APACHEIII).

**Prevalence of criteria in the population**
