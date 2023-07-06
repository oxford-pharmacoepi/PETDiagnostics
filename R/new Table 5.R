## table 5 - use case
library(CodelistGenerator)
library(PatientProfiles)
library(Capr)
library(DrugUtilisation)
library(dplyr)
library(tidyr)
library(here)

antiepileptics_codes <- as.list(getDescendants(cdm, conceptId = 21604390)[1])
nsaids_codes <- as.list(getDescendants(cdm, conceptId = 21604303)[1])
antidepressants_codes <- as.list(getDescendants(cdm, conceptId = 21604686)[1])
obstr_resp_dis_rx_codes <- as.list(getDescendants(cdm, conceptId = 21603248)[1])

medications <- c(antiepileptics_codes,nsaids_codes,
                 antidepressants_codes,obstr_resp_dis_rx_codes)

names(medications) <- c("antiepileptics","nsaids",
                        "antidepressants","asthma")

print(medications)

## adding the medications table to the existing cdm
cdm <- generateDrugUtilisationCohortSet(cdm, "medications", medications)

head(cdm$medications, n = 5)


######## condition codes


concept_sets <- list(
  anxiety = cs(descendants(441542)),
  depression = cs(descendants(444100)),
  hypertension = cs(descendants(316866)),
  DM = cs(descendants(201820)),
  epilepsy = cs(descendants(380378)),
  asthma = cs(descendants(317009)),
  renal_impairment = cs(descendants(4030518)),
  pre_eclampsia = cs(descendants(c(443700, 439393, 141084))),
  gestational_diabetes = cs(descendants(c(4024659, 43020791, 438480))),
  pregnancy_induced_hypertension = cs(descendants(4167493))
)


condition_cohort_template <- function(concept_set) {
  cohort(entry = entry(condition(concept_set), primaryCriteriaLimit = "All"),
         exit = exit(fixedExit("startDate", 0L)))
}


cohortsConditions <- list()

for (i in 1:length(concept_sets)) {
  cohortsConditions[[i]] <- condition_cohort_template(concept_sets[[i]])
}

# Lets add names to the list of cohorts
names(cohortsConditions) <- names(concept_sets)

cdm <- generateCohortSet(cdm, cohortsConditions, "cohort_conditions", overwrite = TRUE)

head(cdm$cohort_conditions, n = 5)


## Cohort intersection
# We can also see whether individuals had an exposure or condition in some time period relative to their cohort start date using the `flagCohortPresence` function.
# In this case we'll look at two time periods, before their cohort start date and between cohort start date and end date.


## make mothertable into a "generated cohort set"



cdm$pregnancy_cohort <- cdm$mothertable %>%
                                   select("person_id","pregnancy_start_date","pregnancy_end_date") %>%
                                   rename(cohort_start_date = pregnancy_start_date,
                                          cohort_end_date = pregnancy_end_date,
                                          subject_id = person_id) %>%
  mutate(cohort_definition_id = 1) %>% relocate(cohort_definition_id)



DBI::dbWriteTable(con, "pregnancy_cohort_set",
                  dplyr::tibble(cohort_definition_id = c(1),
                                cohort_name = c("pregnancy")), overwrite=T)

DBI::dbWriteTable(con, "pregnancy_cohort_count",
                  cdm$pregnancy_cohort %>%
                    group_by(cohort_definition_id) %>%
                    tally(name = "number_records") %>%
                    collect() %>%
                    inner_join(cdm$pregnancy_cohort %>%
                                 select(subject_id, cohort_definition_id) %>%
                                 distinct() %>%
                                 group_by(cohort_definition_id) %>%
                                 tally(name = "number_subjects") %>%
                                 collect(),
                               by = "cohort_definition_id"), overwrite =T)

cdm$pregnancy_cohort_set <- con %>% dplyr::tbl(CDMConnector::in_schema("main",
                                                                       "pregnancy_cohort_set"))
cdm$pregnancy_cohort_count <- con %>% dplyr::tbl(CDMConnector::in_schema("main",
                                                                         "pregnancy_cohort_count"))

cdm$pregnancy_cohort <- newGeneratedCohortSet(cdm$pregnancy_cohort,
                                              cohortSetRef = cdm$pregnancy_cohort_set,
                                              cohortCountRef = cdm$pregnancy_cohort_count
                                              )

#### addAge
age <- cdm$pregnancy_cohort %>%
  addAge(
    cdm = cdm,
    indexDate = "cohort_start_date"
  ) %>%
  summarise(mean_age = mean(age),
            sd_age = sd(age))


#### get the intersection
##### during pregnancy: time window before end, after start, then say "both TRUE"

antiepileptics_during_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "medications",
    targetCohortId = 1,
    indexDate = "cohort_end_date",
    window = c(-Inf, -1),
    nameStyle = "exposure_end"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "medications",
    targetCohortId = 1,
    indexDate = "cohort_start_date",
    window = c(0, Inf),
    nameStyle = "exposure_start"
  ) %>%
  select("exposure_start","exposure_end") %>%
  filter(exposure_start == 1,
         exposure_end == 1) %>%
  summarise(antiepileptics_during = sum(exposure_start))

##### before pregnancy: time window before start

antiepileptics_before_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "medications",
    targetCohortId = 2,
    indexDate = "cohort_start_date",
    window = c(-Inf, -1),
    nameStyle = "exposure_history"
  ) %>%
  select("exposure_history") %>%
  filter(exposure_history == 1) %>%
  summarise(antiepileptics_before = sum(exposure_history))

##### during pregnancy: time window before end, after start, then say "both TRUE"

nsaids_during_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "medications",
    targetCohortId = 2,
    indexDate = "cohort_end_date",
    window = c(-Inf, -1),
    nameStyle = "exposure_end"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "medications",
    targetCohortId = 1,
    indexDate = "cohort_start_date",
    window = c(0, Inf),
    nameStyle = "exposure_start"
  ) %>%
  select("exposure_start","exposure_end") %>%
  filter(exposure_start == 1,
         exposure_end == 1) %>%
  summarise(nsaids_during = sum(exposure_start))

##### before pregnancy: time window before start

nsaids_before_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "medications",
    targetCohortId = 1,
    indexDate = "cohort_start_date",
    window = c(-Inf, -1),
    nameStyle = "exposure_history"
  ) %>%
  select("exposure_history") %>%
  filter(exposure_history == 1) %>%
  summarise(nsaids_before = sum(exposure_history))



##### during pregnancy: time window before end, after start, then say "both TRUE"

antidepressants_during_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "medications",
    targetCohortId = 3,
    indexDate = "cohort_end_date",
    window = c(-Inf, -1),
    nameStyle = "exposure_end"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "medications",
    targetCohortId = 1,
    indexDate = "cohort_start_date",
    window = c(0, Inf),
    nameStyle = "exposure_start"
  ) %>%
  select("exposure_start","exposure_end") %>%
  filter(exposure_start == 1,
         exposure_end == 1) %>%
  summarise(antidepressants_during = sum(exposure_start))

##### before pregnancy: time window before start

antidepressants_before_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "medications",
    targetCohortId = 3,
    indexDate = "cohort_start_date",
    window = c(-Inf, -1),
    nameStyle = "exposure_history"
  ) %>%
  select("exposure_history") %>%
  filter(exposure_history == 1) %>%
  summarise(antidepressants_before = sum(exposure_history))


##### during pregnancy: time window before end, after start, then say "both TRUE"

obstr_resp_dis_rx_during_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "medications",
    targetCohortId = 4,
    indexDate = "cohort_end_date",
    window = c(-Inf, -1),
    nameStyle = "exposure_end"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "medications",
    targetCohortId = 1,
    indexDate = "cohort_start_date",
    window = c(0, Inf),
    nameStyle = "exposure_start"
  ) %>%
  select("exposure_start","exposure_end") %>%
  filter(exposure_start == 1,
         exposure_end == 1) %>%
  summarise(obstr_resp_dis_rx_during = sum(exposure_start))

##### before pregnancy: time window before start

obstr_resp_dis_rx_before_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "medications",
    targetCohortId = 4,
    indexDate = "cohort_start_date",
    window = c(-Inf, -1),
    nameStyle = "exposure_history"
  ) %>%
  select("exposure_history") %>%
  filter(exposure_history == 1) %>%
  summarise(obstr_resp_dis_rx_before = sum(exposure_history))


############## CONDITIONS -----------------------------------------------
##during pregnancy: time window before end, after start, then say "both TRUE"

anxiety_during_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 1,
    indexDate = "cohort_end_date",
    window = c(-Inf, -1),
    nameStyle = "conditions_end"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 1,
    indexDate = "cohort_start_date",
    window = c(0, Inf),
    nameStyle = "conditions_start"
  ) %>%
  select("conditions_start","conditions_end") %>%
  filter(conditions_start == 1,
         conditions_end == 1) %>%
  summarise(anxiety_during = sum(conditions_start))


##### before pregnancy: time window before start

anxiety_before_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 1,
    indexDate = "cohort_start_date",
    window = c(-Inf, -1),
    nameStyle = "condition_history"
  ) %>%
  select("condition_history") %>%
  filter(condition_history == 1) %>%
  summarise(anxiety_before = sum(condition_history))


##during pregnancy: time window before end, after start, then say "both TRUE"

depression_during_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 2,
    indexDate = "cohort_end_date",
    window = c(-Inf, -1),
    nameStyle = "conditions_end"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 2,
    indexDate = "cohort_start_date",
    window = c(0, Inf),
    nameStyle = "conditions_start"
  ) %>%
  select("conditions_start","conditions_end") %>%
  filter(conditions_start == 1,
         conditions_end == 1) %>%
  summarise(depression_during = sum(conditions_start))


##### before pregnancy: time window before start

depression_before_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 2,
    indexDate = "cohort_start_date",
    window = c(-Inf, -1),
    nameStyle = "condition_history"
  ) %>%
  select("condition_history") %>%
  filter(condition_history == 1) %>%
  summarise(depression_before = sum(condition_history))



##during pregnancy: time window before end, after start, then say "both TRUE"

hypertension_during_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 3,
    indexDate = "cohort_end_date",
    window = c(-Inf, -1),
    nameStyle = "conditions_end"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 1,
    indexDate = "cohort_start_date",
    window = c(0, Inf),
    nameStyle = "conditions_start"
  ) %>%
  select("conditions_start","conditions_end") %>%
  filter(conditions_start == 1,
         conditions_end == 1) %>%
  summarise(hypertension_during = sum(conditions_start))


##### before pregnancy: time window before start

hypertension_before_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 3,
    indexDate = "cohort_start_date",
    window = c(-Inf, -1),
    nameStyle = "condition_history"
  ) %>%
  select("condition_history") %>%
  filter(condition_history == 1) %>%
  summarise(hypertension_before = sum(condition_history))



##during pregnancy: time window before end, after start, then say "both TRUE"

DM_during_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 4,
    indexDate = "cohort_end_date",
    window = c(-Inf, -1),
    nameStyle = "conditions_end"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 1,
    indexDate = "cohort_start_date",
    window = c(0, Inf),
    nameStyle = "conditions_start"
  ) %>%
  select("conditions_start","conditions_end") %>%
  filter(conditions_start == 1,
         conditions_end == 1) %>%
  summarise(DM_during = sum(conditions_start))


##### before pregnancy: time window before start

DM_before_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 4,
    indexDate = "cohort_start_date",
    window = c(-Inf, -1),
    nameStyle = "condition_history"
  ) %>%
  select("condition_history") %>%
  filter(condition_history == 1) %>%
  summarise(DM_before = sum(condition_history))



##during pregnancy: time window before end, after start, then say "both TRUE"

epilepsy_during_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 5,
    indexDate = "cohort_end_date",
    window = c(-Inf, -1),
    nameStyle = "conditions_end"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 1,
    indexDate = "cohort_start_date",
    window = c(0, Inf),
    nameStyle = "conditions_start"
  ) %>%
  select("conditions_start","conditions_end") %>%
  filter(conditions_start == 1,
         conditions_end == 1) %>%
  summarise(epilepsy_during = sum(conditions_start))


##### before pregnancy: time window before start

epilepsy_before_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 5,
    indexDate = "cohort_start_date",
    window = c(-Inf, -1),
    nameStyle = "condition_history"
  ) %>%
  select("condition_history") %>%
  filter(condition_history == 1) %>%
  summarise(epilepsy_before = sum(condition_history))


##during pregnancy: time window before end, after start, then say "both TRUE"

asthma_during_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 6,
    indexDate = "cohort_end_date",
    window = c(-Inf, -1),
    nameStyle = "conditions_end"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 1,
    indexDate = "cohort_start_date",
    window = c(0, Inf),
    nameStyle = "conditions_start"
  ) %>%
  select("conditions_start","conditions_end") %>%
  filter(conditions_start == 1,
         conditions_end == 1) %>%
  summarise(asthma_during = sum(conditions_start))


##### before pregnancy: time window before start

asthma_before_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 6,
    indexDate = "cohort_start_date",
    window = c(-Inf, -1),
    nameStyle = "condition_history"
  ) %>%
  select("condition_history") %>%
  filter(condition_history == 1) %>%
  summarise(asthma_before = sum(condition_history))

##during pregnancy: time window before end, after start, then say "both TRUE"

renal_impairment_during_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 7,
    indexDate = "cohort_end_date",
    window = c(-Inf, -1),
    nameStyle = "conditions_end"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 1,
    indexDate = "cohort_start_date",
    window = c(0, Inf),
    nameStyle = "conditions_start"
  ) %>%
  select("conditions_start","conditions_end") %>%
  filter(conditions_start == 1,
         conditions_end == 1) %>%
  summarise(renal_impairment_during = sum(conditions_start))


##### before pregnancy: time window before start

renal_impairment_before_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 7,
    indexDate = "cohort_start_date",
    window = c(-Inf, -1),
    nameStyle = "condition_history"
  ) %>%
  select("condition_history") %>%
  filter(condition_history == 1) %>%
  summarise(renal_impairment_before = sum(condition_history))

##during pregnancy: time window before end, after start, then say "both TRUE"

pre_eclampsia_during_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 8,
    indexDate = "cohort_end_date",
    window = c(-Inf, -1),
    nameStyle = "conditions_end"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 1,
    indexDate = "cohort_start_date",
    window = c(0, Inf),
    nameStyle = "conditions_start"
  ) %>%
  select("conditions_start","conditions_end") %>%
  filter(conditions_start == 1,
         conditions_end == 1) %>%
  summarise(pre_eclampsia_during = sum(conditions_start))


##### before pregnancy: time window before start

pre_eclampsia_before_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 8,
    indexDate = "cohort_start_date",
    window = c(-Inf, -1),
    nameStyle = "condition_history"
  ) %>%
  select("condition_history") %>%
  filter(condition_history == 1) %>%
  summarise(pre_eclampsia_before = sum(condition_history))

##during pregnancy: time window before end, after start, then say "both TRUE"

gestational_diabetes_during_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 9,
    indexDate = "cohort_end_date",
    window = c(-Inf, -1),
    nameStyle = "conditions_end"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 1,
    indexDate = "cohort_start_date",
    window = c(0, Inf),
    nameStyle = "conditions_start"
  ) %>%
  select("conditions_start","conditions_end") %>%
  filter(conditions_start == 1,
         conditions_end == 1) %>%
  summarise(gestational_diabetes_during = sum(conditions_start))


##### before pregnancy: time window before start

gestational_diabetes_before_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 9,
    indexDate = "cohort_start_date",
    window = c(-Inf, -1),
    nameStyle = "condition_history"
  ) %>%
  select("condition_history") %>%
  filter(condition_history == 1) %>%
  summarise(gestational_diabetes_before = sum(condition_history))


##during pregnancy: time window before end, after start, then say "both TRUE"

pregnancy_induced_hypertension_during_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 10,
    indexDate = "cohort_end_date",
    window = c(-Inf, -1),
    nameStyle = "conditions_end"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 1,
    indexDate = "cohort_start_date",
    window = c(0, Inf),
    nameStyle = "conditions_start"
  ) %>%
  select("conditions_start","conditions_end") %>%
  filter(conditions_start == 1,
         conditions_end == 1) %>%
  summarise(pregnancy_induced_hypertension_during = sum(conditions_start))


##### before pregnancy: time window before start

pregnancy_induced_hypertension_before_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    targetCohortId = 10,
    indexDate = "cohort_start_date",
    window = c(-Inf, -1),
    nameStyle = "condition_history"
  ) %>%
  select("condition_history") %>%
  filter(condition_history == 1) %>%
  summarise(pregnancy_induced_hypertension_before = sum(condition_history))


#### combine all the information


table5 <- cbind(as_tibble(age),
                as_tibble(antidepressants_before_pregnancy),
                as_tibble(antidepressants_during_pregnancy),
                as_tibble(antiepileptics_before_pregnancy),
                as_tibble(antiepileptics_during_pregnancy),
                as_tibble(nsaids_before_pregnancy),
                as_tibble(nsaids_during_pregnancy),
                as_tibble(obstr_resp_dis_rx_before_pregnancy),
                as_tibble(obstr_resp_dis_rx_during_pregnancy),
                as_tibble(anxiety_before_pregnancy),
                as_tibble(anxiety_during_pregnancy),
                as_tibble(asthma_before_pregnancy),
                as_tibble(asthma_during_pregnancy),
                as_tibble(depression_before_pregnancy),
                as_tibble(depression_during_pregnancy),
                as_tibble(DM_before_pregnancy),
                as_tibble(DM_during_pregnancy),
                as_tibble(epilepsy_before_pregnancy),
                as_tibble(epilepsy_during_pregnancy),
                as_tibble(gestational_diabetes_before_pregnancy),
                as_tibble(gestational_diabetes_during_pregnancy),
                as_tibble(hypertension_before_pregnancy),
                as_tibble(hypertension_during_pregnancy),
                as_tibble(pre_eclampsia_before_pregnancy),
                as_tibble(pre_eclampsia_during_pregnancy),
                as_tibble(pregnancy_induced_hypertension_before_pregnancy),
                as_tibble(pregnancy_induced_hypertension_during_pregnancy),
                as_tibble(renal_impairment_before_pregnancy),
                as_tibble(renal_impairment_during_pregnancy)
                ) %>% pivot_longer(everything())

write.csv(table5, file = here::here("table5.csv"))
