# TraumaCall
Studies have shown that patients who have suffered severe trauma have improved outcomes when a multidisciplinary team and attendant resources are mobilised from other tasks on patient arrival1,2.  In seeking to maximise the benefit of hospital behaviour at the population level, the benefit to severely injured trauma patients is balanced against the effect of removing resources from other areas.  This balance is unlikely to be the same in every trauma centre

Trauma team composition is not closely specified at a national or international level and therefore differs between institutions.  Criteria for trauma team activation also differ mildly between institutions (refs?), as do the characteristics of the patients who arrive at the hospital with trauma and are screened for trauma team activation.  It is inherent in the service specifications that the greatest population benefit would be seen a hospital with a higher proportion of severely injured patients arriving in the early phase of trauma, plus both a low rate and a constant and low illness severity of patients presenting with non traumatic illness.  This logic underlies the recommendations of many mature health systems for a single trauma centre covering a primary population of 2.5 million (refs?).

Our institution, Royal Darwin Hospital, is a remote hospital covering a large geographical area with a small population (ref).  There is a high incidence of severe trauma but long retrieval times are common (ref from Kath's work; if influential we could add a column of "injury time" and do a quick dirty analysis of that) and there is a high population burden of non traumatic illness which is both seasonally varying and unpredictably varying (ref).  The "Trauma Team Callout Criteria" (TTCC) at Royal Darwin Hospital (RDH) include pre hospital (mechanism of injury) and clinical criteria for activation.  Mechanism of injury criteria have low predictive value for severe injury3-5 and may lead to a high rate of over-triage for RDH's trauma patients.

The severity of the whole cohort for that year is indicated by the median Risk of Death based on the ANZICS APACHEIII score is given below.  The various risk functions give varying risks of death as summarised below, and the standardised mortality ratio across the year is from 0.42 to 0.53 (0.53 using APACHEIII).

```{r}echo(OFF)
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
