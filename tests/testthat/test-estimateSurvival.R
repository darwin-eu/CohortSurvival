compareNA <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

test_that("mgus example: no Competing risk", {
  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
    targetCohortTable = "mgus_diagnosis",
    targetCohortId = 1,
    outcomeCohortTable = "death_cohort",
    outcomeCohortId = 1,
    timeGap = 7
  )
  expect_true(tibble::is_tibble(surv))
  expect_true(all(c(
    "cdm_name","result_type",
    "group_name","group_level",
    "strata_name","strata_level",
    "variable","variable_level",
    "estimate_type",
    "variable_type",
    "time",
    "analysis_type",
    "estimate") %in%
      colnames(surv)))

  expect_true(surv %>% dplyr::select(variable) %>% dplyr::pull() %>% unique() == "Outcome")
  expect_true(all(compareNA(surv %>% dplyr::select(time) %>% dplyr::pull() %>% unique(), c(0:424, NA))))
  expect_true(surv %>% dplyr::select(analysis_type) %>% dplyr::pull() %>% unique() == "Single event")

  expect_true(surv %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::select(variable_level) %>% dplyr::pull() %>% unique() == "timeGap 7")
  expect_true(all(surv %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::select(time) %>% dplyr::pull() %>% unique() == c(seq(0, 424, by = 7), 424)))

  # mgus example: Competing risk
  survCR <- estimateSurvival(cdm,
    targetCohortTable = "mgus_diagnosis",
    targetCohortId = 1,
    outcomeCohortTable = "progression",
    outcomeCohortId = 1,
    competingOutcomeCohortTable = "death_cohort",
    competingOutcomeCohortId = 1
  )

  expect_true(all(colnames(surv) == colnames(survCR)))

  expect_true(tibble::is_tibble(survCR))

  # note, we don't return summary for Competing risk (tbd)

  expect_true(all(survCR %>%
                    dplyr::select(outcome) %>%
                    dplyr::pull() %>%
                    unique() %in%
                    c("death_cohort", "progression", "outcome", "competing outcome", "none")))

  expect_true(all(compareNA(survCR %>%
                    dplyr::select(time) %>%
                    dplyr::pull() %>%
                    unique(), c(0:424, NA))))

  expect_true(survCR %>%
                dplyr::select(analysis_type) %>%
                dplyr::pull() %>%
                unique() == "Competing risk")

  expect_true(all(survCR %>% dplyr::filter(estimate_type == "Survival events") %>%
                    dplyr::select(variable_level) %>%
                    dplyr::pull() %>%
                    unique() %in% c("timeGap 1", "timeGap 7", "timeGap 30", "timeGap 365")))

  expect_true(all(survCR %>% dplyr::filter(estimate_type == "Survival events") %>%
                    dplyr::select(time) %>%
                    dplyr::pull() %>% unique() == c(0:424)))

  expect_true(nrow(survCR %>%
    dplyr::filter(.data$outcome == "death_cohort") %>%
    dplyr::collect())>=1)
  expect_true(nrow(survCR %>%
                     dplyr::filter(.data$outcome == "progression") %>%
                     dplyr::collect())>=1)

  expect_true(all(c("death_cohort", "progression") %in%
                (survCR %>%
                dplyr::pull("variable_level") %>%
                unique())))


  CDMConnector::cdmDisconnect(cdm)
})

test_that("mgus example: no Competing risk, strata", {
  cdm <- mockMGUS2cdm()
  cdm[["mgus_diagnosis"]] <- cdm[["mgus_diagnosis"]] %>%
    dplyr::mutate(mspike_r = round(mspike, digits = 0))
  surv <- estimateSurvival(cdm,
    targetCohortTable = "mgus_diagnosis",
    targetCohortId = 1,
    outcomeCohortTable = "death_cohort",
    outcomeCohortId = 1,
    timeGap = c(1, 10, 100),
    strata = list(
      "age_gr" = c("age"),
      "sex" = c("sex"),
      "age and sex" = c("age", "sex"),
      "mspike rounded" = c("mspike_r")
    )
  )
  expect_true(tibble::is_tibble(surv))

  expect_true(all(surv %>% dplyr::select(outcome) %>% dplyr::pull() %>% unique() %in% c("death_cohort", "outcome")))
  expect_true(all(compareNA(surv %>% dplyr::select(time) %>% dplyr::pull() %>% unique(), c(0:424, NA))))
  expect_true(surv %>% dplyr::select(analysis_type) %>% dplyr::pull() %>% unique() == "Single event")
  expect_true(all(surv %>% dplyr::select(strata_name) %>% dplyr::pull() %>% unique() %in%
                    c("Overall", "sex", "age", "mspike_r", "age and sex")))
  expect_true(all(surv %>% dplyr::select(strata_level) %>% dplyr::pull() %>% unique() %in% c(
    "M", "F", 0, 1, 2, 3, c(24:96), "Overall",
    paste(expand.grid(c(24:96), c("M", "F"))$Var1, expand.grid(c(24:96), c("M", "F"))$Var2, sep = " and ")
  )))

  expect_true(all(surv %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::select(variable_level) %>% dplyr::pull() %>% unique() %in% c("timeGap 1", "timeGap 10", "timeGap 100")))
  expect_true(all(surv %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::select(time) %>% dplyr::pull() %>% unique() == c(0:424)))
  expect_true(all(surv %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::select(strata_name) %>% dplyr::pull() %>% unique() %in%
                    c("Overall", "sex", "age", "mspike_r", "age and sex")))
  expect_true(all(surv %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::select(strata_level) %>% dplyr::pull() %>% unique() %in% c(
    "M", "F", 0, 1, 2, 3, c(24:96), "Overall",
    paste(expand.grid(c(24:96), c("M", "F"))$Var1, expand.grid(c(24:96), c("M", "F"))$Var2, sep = " and ")
  )))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("mgus example: Competing risk, strata", {
  cdm <- mockMGUS2cdm()
  cdm[["mgus_diagnosis"]] <- cdm[["mgus_diagnosis"]] %>%
    dplyr::mutate(mspike_r = round(mspike, digits = 0))
  survCR <- estimateSurvival(cdm,
                             targetCohortTable = "mgus_diagnosis",
                             targetCohortId = 1,
                             outcomeCohortTable = "progression",
                             outcomeCohortId = 1,
                             competingOutcomeCohortTable = "death_cohort",
                             competingOutcomeCohortId = 1,
                             strata = list(
                               "age" = c("age"),
                               "sex" = c("sex"),
                               "age and sex" = c("age", "sex"),
                               "mspike rounded" = c("mspike_r")
                             )
  )

  expect_true(tibble::is_tibble(survCR))
  expect_true(all(survCR %>% dplyr::select(outcome) %>% dplyr::pull() %>% unique() %in% c("death_cohort", "progression", "outcome", "competing outcome","none")))
  expect_true(all(compareNA(survCR %>% dplyr::select(time) %>% dplyr::pull() %>% unique(), c(0:424,NA))))
  expect_true(survCR %>% dplyr::select(analysis_type) %>% dplyr::pull() %>% unique() == "Competing risk")
  expect_true(all(survCR %>% dplyr::select(strata_name) %>% dplyr::pull() %>% unique() %in% c("Overall", "sex", "age", "mspike_r", "age and sex")))
  expect_true(all(survCR %>% dplyr::select(strata_level) %>% dplyr::pull() %>% unique() %in% c(
    "M", "F", 0, 1, 2, 3, c(24:96), "Overall",
    paste(expand.grid(c(24:96), c("M", "F"))$Var1, expand.grid(c(24:96), c("M", "F"))$Var2, sep = " and ")
  )))

  expect_true(all(survCR %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::select(variable_level) %>% dplyr::pull() %>% unique() %in% c("timeGap 1", "timeGap 7", "timeGap 30", "timeGap 365")))
  expect_true(all(survCR %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::select(time) %>% dplyr::pull() %>% unique() == c(0:424)))
  expect_true(all(survCR %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::select(strata_name) %>% dplyr::pull() %>% unique() %in% c("Overall", "sex", "age", "mspike_r", "age and sex")))
  expect_true(all(survCR %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::select(strata_level) %>% dplyr::pull() %>% unique() %in% c(
    "M", "F", 0, 1, 2, 3, c(24:96), "Overall",
    paste(expand.grid(c(24:96), c("M", "F"))$Var1, expand.grid(c(24:96), c("M", "F"))$Var2, sep = " and ")
  )))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("funcionality with created dataset", {
  exposure_cohort <- dplyr::tibble(
    subject_id = c(1, 2, 3),
    cohort_definition_id = c(1, 1, 1),
    cohort_start_date = c(
      as.Date("2020-01-01"),
      as.Date("2020-02-03"),
      as.Date("2020-05-01")
    ),
    cohort_end_date = c(
      as.Date("2020-01-31"),
      as.Date("2022-02-03"),
      as.Date("2021-06-28")
    ),
    age_group = c("20;29", "20;29", "60;69"),
    sex = c("Female", "Male", "Female"),
    blood_type = c("A", "B", "B")
  )
  outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 1, 2, 3, 3),
    cohort_start_date = c(
      as.Date("2020-01-10"),
      as.Date("2020-02-02"),
      as.Date("2020-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    ),
    cohort_end_date = c(
      as.Date("2020-01-10"),
      as.Date("2020-02-02"),
      as.Date("2020-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    )
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 1, 1),
    person_id = c(1, 2, 3),
    observation_period_start_date = c(
      as.Date("2007-03-21"),
      as.Date("2006-09-09"),
      as.Date("1980-07-20")
    ),
    observation_period_end_date = c(
      as.Date("2022-09-08"),
      as.Date("2023-01-03"),
      as.Date("2023-05-20")
    )
  )

  cdm <- PatientProfiles::mockPatientProfiles(
    exposure_cohort = exposure_cohort,
    cohort1 = outcome_cohort,
    observation_period = observation_period
  )

  # No competing events
  surv <- estimateSurvival(cdm,
    targetCohortTable = "exposure_cohort",
    outcomeCohortTable = "cohort1",
    minCellCount = 1
  )

  expect_true(all(compareNA(surv %>% dplyr::select(time) %>% dplyr::pull() %>%
                    unique(), c(0:31, NA))))
  expect_true(all(surv %>%
                    dplyr::filter(variable_type == "n_risk") %>%
                    dplyr::select(estimate) %>%
                    dplyr::pull() ==
                    c(rep(3, 7), rep(2, 3), rep(1, 22))))
  expect_true(all(surv %>%
                    dplyr::filter(variable_type == "estimate")  %>%
    dplyr::filter(estimate_type == "Survival probability") %>%
      dplyr::select(estimate) %>%
    dplyr::pull() - c(rep(1, 7), rep(0.667, 3), rep(0.333, 21), 0) < c(0.01)))
  expect_true(all(surv %>%
                    dplyr::filter(variable_type == "estimate")  %>%
                    dplyr::filter(estimate_type == "Cumulative failure probability") %>%
                    dplyr::select(estimate) %>% dplyr::pull() - c(rep(0, 6), rep(0.333, 3), rep(0.667, 22), 1) < c(0.01)))
  expect_true(all(surv %>% dplyr::select(analysis_type) %>% dplyr::pull() == "Single event"))

  expect_true(all(surv %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::select(variable_level) %>% dplyr::pull() %in% c("timeGap 1", "timeGap 7", "timeGap 30", "timeGap 365")))
  expect_true(all(surv %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::group_by(variable_level) %>% dplyr::summarise(n = sum(estimate, na.rm = T)) %>% dplyr::select(n) %>%
                    dplyr::pull() == c(3, 3, 3, 3)))

  CDMConnector::cdmDisconnect(cdm)

  # Compting events
  outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1),
    subject_id = c(2, 3, 3),
    cohort_start_date = c(
      as.Date("2020-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    ),
    cohort_end_date = c(
      as.Date("2020-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    )
  )
  competing_risk_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2),
    subject_id = c(2, 3, 1),
    cohort_start_date = c(
      as.Date("2020-02-07"),
      as.Date("2021-02-02"),
      as.Date("2020-01-03")
    ),
    cohort_end_date = c(
      as.Date("2020-02-07"),
      as.Date("2021-02-02"),
      as.Date("2020-01-03")
    )
  )
  cdm <- PatientProfiles::mockPatientProfiles(
    exposure_cohort = exposure_cohort,
    cohort1 = outcome_cohort,
    cohort2 = competing_risk_cohort,
    observation_period = observation_period
  )
  surv2 <- estimateSurvival(cdm,
    targetCohortTable = "exposure_cohort",
    outcomeCohortTable = "cohort1",
    competingOutcomeCohortTable = "cohort2",
    competingOutcomeCohortId = 1,
    minCellCount = 1
  )

  expect_true(all(compareNA(surv2 %>% dplyr::select(time) %>% dplyr::pull() %>% unique(), c(0:981, NA))))
  expect_true(all(surv2 %>%
                    dplyr::filter(outcome == "outcome") %>%
                    dplyr::filter(estimate_type == "Cumulative failure probability") %>%
                    dplyr::filter(variable_type == "n_risk") %>%
                    dplyr::select(estimate) %>%
                    dplyr::pull() ==
                    c(rep(3, 5), rep(2, 27), rep(1, 950))))
  expect_true(nrow(surv2 %>%
                          dplyr::filter(estimate_type == "Survival probability") %>%
                          dplyr::select(estimate)) == 0)
  expect_true(all(surv2 %>% dplyr::filter(variable_type == "n_risk") %>% dplyr::pull() ==
                    c(rep(3, 5), rep(2, 27), rep(1, 950))))

  expect_true(all(surv2 %>% dplyr::select(analysis_type) %>% dplyr::pull() == "Competing risk"))
  expect_true(all(surv2 %>% dplyr::select(outcome) %>% dplyr::pull() %in% c("cohort_1", "cohort_1_competing_outcome", "outcome", "competing outcome", "none")))
  expect_true(all(surv2 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::filter(!is.na(estimate) & variable_level == "timeGap 1" & outcome == "cohort_1") %>% dplyr::select(time) == c(0:981)))
  expect_true(all(surv2 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::filter(!is.na(estimate) & variable_level == "timeGap 1" & outcome == "cohort_1_competing_outcome") %>% dplyr::select(time) == c(0:981)))
  expect_true(all(surv2 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::filter(!is.na(estimate) & variable_level == "timeGap 7" & outcome == "cohort_1") %>% dplyr::select(time) == c(seq(0,981, by = 7), 981)))
  expect_true(all(surv2 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::filter(!is.na(estimate) & variable_level == "timeGap 7" & outcome == "cohort_1_competing_outcome") %>% dplyr::select(time) == c(seq(0,981, by = 7), 981)))
  expect_true(all(surv2 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::filter(!is.na(estimate) & variable_level == "timeGap 30" & outcome == "cohort_1") %>% dplyr::select(time) == c(seq(0,981, by = 30), 981)))
  expect_true(all(surv2 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::filter(!is.na(estimate) & variable_level == "timeGap 30" & outcome == "cohort_1_competing_outcome") %>% dplyr::select(time) == c(seq(0,981, by = 30), 981)))
  expect_true(all(surv2 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::filter(!is.na(estimate) & variable_level == "timeGap 365" & outcome == "cohort_1") %>% dplyr::select(time) == c(seq(0,981, by = 365), 981)))
  expect_true(all(surv2 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::filter(!is.na(estimate) & variable_level == "timeGap 365" & outcome == "cohort_1_competing_outcome") %>% dplyr::select(time) == c(seq(0,981, by = 365), 981)))
  expect_true(all(surv2 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::select(variable_level) %>% dplyr::pull() %in% c("timeGap 1", "timeGap 7", "timeGap 30", "timeGap 365")))
  expect_true(all(surv2 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::group_by(variable_level) %>% dplyr::summarise(n = sum(estimate, na.rm = T)) %>% dplyr::select(n) %>% dplyr::pull() == c(2, 2, 2, 2)))

  # change variable_level not name cohort
  # maybe not names outcome or copeting, or say must be different names (or put name_cr, in outcome!)
  # then check

  CDMConnector::cdmDisconnect(cdm)

  # Censor at cohort end
  outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 1, 2, 3, 3),
    cohort_start_date = c(
      as.Date("2020-02-10"),
      as.Date("2020-02-02"),
      as.Date("2020-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    ),
    cohort_end_date = c(
      as.Date("2020-02-10"),
      as.Date("2020-02-02"),
      as.Date("2020-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    )
  )

  cdm <- PatientProfiles::mockPatientProfiles(
    exposure_cohort = exposure_cohort,
    cohort1 = outcome_cohort,
    observation_period = observation_period
  )

  surv3 <- estimateSurvival(cdm, "exposure_cohort",
    outcomeCohortTable = "cohort1",
    censorOnCohortExit = TRUE,
    minCellCount = 1
  )

  expect_true(all(compareNA(surv3 %>% dplyr::select(time) %>% dplyr::pull() %>% unique(), c(0:31,NA))))
   expect_true(all(surv3 %>% dplyr::filter(variable_type == "n_risk") %>% dplyr::select(estimate) %>% dplyr::pull() == c(rep(3, 7), rep(2, 24), 1)))
   expect_true(all(surv3  %>%
                     dplyr::filter(estimate_type == "Cumulative failure probability") %>%
                     dplyr::filter(variable_type == "estimate") %>%
                     dplyr::select(estimate) %>% dplyr::pull() - c(rep(0, 6), rep(0.333, 25), 1) < c(0.01)))
   expect_true(all(surv3 %>% dplyr::select(analysis_type) %>% dplyr::pull() == "Single event"))

   expect_true(all(surv3 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::filter(!is.na(estimate) & variable_level == "timeGap 1") %>% dplyr::select(time) == c(0: 31)))
   expect_true(all(surv3 %>% dplyr::filter(estimate_type == "Survival events")  %>% dplyr::select(variable_level) %>% dplyr::pull() %in% c("timeGap 1", "timeGap 7", "timeGap 30", "timeGap 365")))
   expect_true(all(surv3 %>% dplyr::filter(estimate_type == "Survival events")  %>% dplyr::group_by(variable_level) %>% dplyr::summarise(n = sum(estimate, na.rm = T)) %>%
                     dplyr::select(n) %>% dplyr::pull() == c(2, 2, 2, 2)))

  CDMConnector::cdmDisconnect(cdm)

  cdm <- PatientProfiles::mockPatientProfiles(
    exposure_cohort = exposure_cohort,
    cohort1 = outcome_cohort,
    observation_period = observation_period
  )

  # Censor by follow up days
  surv4 <- estimateSurvival(cdm, "exposure_cohort",
    outcomeCohortTable = "cohort1",
    followUpDays = 10,
    minCellCount = 1
  )

   expect_true(all(compareNA(surv4 %>% dplyr::select(time) %>% dplyr::pull() %>% unique(), c(0:10,NA))))
   expect_true(all(surv4 %>% dplyr::filter(variable_type == "n_risk") %>% dplyr::select(estimate) %>% dplyr::pull() == c(rep(3, 7), rep(2, 4))))
   expect_true(all(surv4 %>%
                     dplyr::filter(estimate_type == "Cumulative failure probability") %>%
                     dplyr::filter(variable_type == "estimate") %>%
                     dplyr::select(estimate) %>% dplyr::pull() - c(rep(0, 6), rep(0.333, 5)) < c(0.01)))
   expect_true(all(surv4 %>% dplyr::select(analysis_type) %>% dplyr::pull() == "Single event"))

   expect_true(all(surv4 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::filter(!is.na(estimate) & variable_level == "timeGap 1") %>% dplyr::select(time) == c(0:10)))
   expect_true(all(surv4 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::select(variable_level) %>% dplyr::pull() %in% c("timeGap 1", "timeGap 7", "timeGap 30", "timeGap 365")))
   expect_true(all(surv4 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::group_by(variable_level) %>% dplyr::summarise(n = sum(estimate, na.rm = T)) %>% dplyr::select(n) %>% dplyr::pull() == c(1, 1, 1, 1)))

  CDMConnector::cdmDisconnect(cdm)

  outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 1, 2, 3, 3),
    cohort_start_date = c(
      as.Date("2020-01-10"),
      as.Date("2020-02-02"),
      as.Date("2020-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    ),
    cohort_end_date = c(
      as.Date("2020-01-10"),
      as.Date("2020-02-02"),
      as.Date("2020-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    )
  )

  cdm <- PatientProfiles::mockPatientProfiles(
    exposure_cohort = exposure_cohort,
    cohort1 = outcome_cohort,
    observation_period = observation_period
  )

  # Strata
  surv5 <- estimateSurvival(cdm, "exposure_cohort",
    outcomeCohortTable = "cohort1",
    strata = list(
      "Age group" = c("age_group"),
      "Sex" = c("sex"),
      "Age group and sex" = c("age_group", "sex"),
      "Blood type" = c("blood_type")
    ),
    minCellCount = 1
  )

  expect_true(all(compareNA(surv5 %>% dplyr::filter(strata_name == "Overall") %>% dplyr::select(time) %>% dplyr::pull() %>% unique(), c(0:31,NA))))
   expect_true(all(surv5 %>% dplyr::filter(strata_name == "Overall") %>% dplyr::filter(estimate == "n_risk") %>% dplyr::select(estimate) %>% dplyr::pull() == c(rep(3, 7), rep(2, 3), rep(1, 22))))
   expect_true(all(surv5 %>% dplyr::filter(strata_name == "Overall") %>%
                     dplyr::filter(estimate_type == "Cumulative failure probability") %>%
                     dplyr::filter(variable_type == "estimate") %>%
                     dplyr::select(estimate) %>% dplyr::pull() - c(rep(0, 6), rep(0.333, 3), rep(0.667, 22), 1) < c(0.01)))
   expect_true(all(surv5 %>% dplyr::filter(strata_name == "Overall") %>% dplyr::select(analysis_type) %>% dplyr::pull() == "Single event"))

   expect_true(all(surv5%>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::filter(strata_name == "Overall") %>% dplyr::filter(!is.na(estimate) & variable_level == "timeGap 1") %>% dplyr::select(time) == c(0:31)))

   expect_true(all(surv5 %>% dplyr::filter(strata_name == "age_group; sex" & strata_level == "20;29; Female") %>% dplyr::select(time) %>% dplyr::pull() == c(0:31)))
   expect_true(all(compareNA(surv5 %>% dplyr::filter(strata_name == "age_group; sex" & strata_level == "20;29; Female") %>% dplyr::filter(estimate == "n_risk") %>% dplyr::select(estimate) %>% dplyr::pull(), c(rep(c(rep(1, 10), rep(NA, 22)),2)))))
   expect_true(all(surv5 %>% dplyr::filter(strata_name == "age_group; sex" & strata_level == "20;29; Female") %>%
                     dplyr::filter(estimate_type == "Cumulative failure probability") %>%
                     dplyr::filter(variable_type == "estimate") %>%
                     dplyr::select(estimate) %>% dplyr::pull() - c(rep(0, 9), rep(1, 23)) < c(0.01)))
   expect_true(all(surv5 %>% dplyr::filter(strata_name == "age_group; sex" & strata_level == "20;29; Female") %>% dplyr::select(analysis_type) %>% dplyr::pull() == "Single event"))

   expect_true(all(surv5%>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::filter(strata_name == "age_group and sex" & strata_level == "20;29 and Female") %>% dplyr::filter(!is.na(estimate) & variable_level == "timeGap 1") %>% dplyr::select(time) == c(0:31)))

   expect_true(all(surv5 %>% dplyr::filter(estimate_type == "Cumulative failure probability" & variable_type == "estimate" & strata_name == "blood_type" & strata_level == "B") %>% dplyr::select(time) %>% dplyr::pull() == c(0:31)))
   expect_true(all(surv5 %>% dplyr::filter(estimate_type == "Cumulative failure probability" & variable_type == "n_risk" & strata_name == "blood_type" & strata_level == "B") %>% dplyr::select(estimate) %>% dplyr::pull() == c(rep(2, 7), rep(1, 25))))
   expect_true(all(surv5 %>% dplyr::filter(estimate_type == "Cumulative failure probability" & variable_type == "estimate" & strata_name == "blood_type" & strata_level == "B") %>%
                     dplyr::select(estimate) %>% dplyr::pull() - c(rep(0, 6), rep(0.5, 25), 1) < c(0.01)))
   expect_true(all(surv5 %>% dplyr::filter(strata_name == "blood_type" & strata_level == "B") %>% dplyr::select(analysis_type) %>% dplyr::pull() == "Single event"))

  CDMConnector::cdmDisconnect(cdm)

  outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 2, 3, 3),
    cohort_start_date = c(
      as.Date("2019-01-10"),
      as.Date("2020-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    ),
    cohort_end_date = c(
      as.Date("2019-01-10"),
      as.Date("2020-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    )
  )

  cdm <- PatientProfiles::mockPatientProfiles(
    exposure_cohort = exposure_cohort,
    cohort1 = outcome_cohort,
    observation_period = observation_period
  )

  # Washout for outcome
  surv6 <- estimateSurvival(cdm,
    targetCohortTable = "exposure_cohort",
    outcomeCohortTable = "cohort1",
    minCellCount = 1
  )

   expect_true(all(compareNA(surv6 %>% dplyr::select(time) %>% dplyr::pull() %>% unique(), c(0:31,NA))))
   expect_true(all(surv6 %>% dplyr::filter(estimate == "n_risk") %>% dplyr::select(estimate) %>% dplyr::pull() == c(rep(2, 7), rep(1, 25))))
   expect_true(all(surv6 %>%
                     dplyr::filter(estimate_type == "Cumulative failure probability") %>%
                     dplyr::filter(variable_type == "estimate") %>%
                     dplyr::select(estimate) %>% dplyr::pull() - c(rep(0, 6), rep(0.5, 25), 1) < c(0.01)))
   expect_true(all(surv6 %>% dplyr::select(analysis_type) %>% dplyr::pull() == "Single event"))

   expect_true(all(surv6 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::filter(!is.na(estimate) & variable_level == "timeGap 1") %>% dplyr::select(time) == c(0:31)))
   expect_true(all(surv6 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::group_by(variable_level) %>% dplyr::summarise(n = sum(estimate, na.rm = T)) %>% dplyr::select(n) %>% dplyr::pull() == c(2, 2, 2, 2)))

  CDMConnector::cdmDisconnect(cdm)

  outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 2, 3, 3),
    cohort_start_date = c(
      as.Date("2020-01-10"),
      as.Date("2020-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    ),
    cohort_end_date = c(
      as.Date("2020-01-10"),
      as.Date("2020-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    )
  )

  cdm <- PatientProfiles::mockPatientProfiles(
    exposure_cohort = exposure_cohort,
    cohort1 = outcome_cohort,
    observation_period = observation_period
  )

  # Censor on date
  surv7 <- estimateSurvival(cdm,
    targetCohortTable = "exposure_cohort",
    outcomeCohortTable = "cohort1",
    minCellCount = 1,
    censorOnDate = as.Date("2020-02-01")
  )

   expect_true(all(compareNA(surv7 %>% dplyr::select(time) %>% dplyr::pull() %>% unique(), c(0:9,NA))))
   expect_true(all(surv7 %>% dplyr::filter(estimate == "n_risk") %>% dplyr::select(estimate) %>% dplyr::pull() == c(rep(1, 10))))
   expect_true(all(surv7 %>%
                     dplyr::filter(estimate_type == "Cumulative failure probability") %>%
                     dplyr::filter(variable_type == "estimate") %>%
                     dplyr::select(estimate)%>% dplyr::pull() - c(rep(0, 9), 1) < c(0.01)))
   expect_true(all(surv7 %>% dplyr::select(analysis_type) %>% dplyr::pull() == "Single event"))

   expect_true(all(surv7%>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::filter(!is.na(estimate) & variable_level == "timeGap 1") %>% dplyr::select(time) == c(0:9)))
   expect_true(all(surv7%>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::group_by(variable_level) %>% dplyr::summarise(n = sum(estimate, na.rm = T)) %>% dplyr::select(n) %>% dplyr::pull() == c(1, 1, 1, 1)))

  CDMConnector::cdmDisconnect(cdm)

  # try with selected time points
  cdm <- PatientProfiles::mockPatientProfiles(
    exposure_cohort = exposure_cohort,
    cohort1 = outcome_cohort,
    observation_period = observation_period
  )
  surv8 <- estimateSurvival(cdm,
                           targetCohortTable = "exposure_cohort",
                           outcomeCohortTable = "cohort1",
                           minCellCount = 1,
                           times = seq(1,1000, by = 100)
  )

   expect_true(all(compareNA(surv8 %>% dplyr::select(time) %>% dplyr::pull() %>% unique(), c(seq(1,901,by = 100),NA))))
   expect_true(all(compareNA(surv8 %>% dplyr::filter(estimate == "n_risk") %>% dplyr::select(estimate) %>% dplyr::pull(), rep(c(3, rep(NA, 9)),2))))
   expect_true(all(surv8 %>%
                     dplyr::filter(estimate_type == "Cumulative failure probability") %>%
                     dplyr::filter(variable_type == "estimate") %>%
                     dplyr::select(estimate) %>%
                     dplyr::pull() == c(0,1,1,1,1,1,1,1,1,1)))
   expect_true(all(surv8 %>% dplyr::select(analysis_type) %>% dplyr::pull() == "Single event"))

   expect_true(all(surv8 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::filter(!is.na(estimate) & variable_level == "timeGap 1") %>% dplyr::select(time) == seq(1,901,100)))
   expect_true(all(surv8 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::group_by(variable_level) %>% dplyr::summarise(n = sum(estimate, na.rm = T)) %>% dplyr::select(n) %>%
                     dplyr::pull() == c(3, 3, 3, 3)))

  CDMConnector::cdmDisconnect(cdm)

})

test_that("different exposure cohort ids", {
  cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2),
    subject_id = c(1, 2, 3),
    cohort_start_date = c(
      as.Date("2020-01-01"),
      as.Date("2020-01-02"),
      as.Date("2020-01-01")
    ),
    cohort_end_date = c(
      as.Date("2020-01-11"),
      as.Date("2020-01-12"),
      as.Date("2020-01-11")
    )
  )
  outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1),
    subject_id = c(1, 2, 3),
    cohort_start_date = c(
      as.Date("2020-01-10"),
      as.Date("2020-01-03"),
      as.Date("2020-01-09")
    ),
    cohort_end_date = c(
      as.Date("2020-01-10"),
      as.Date("2020-01-03"),
      as.Date("2020-01-09")
    )
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 1, 1),
    person_id = c(1, 2, 3),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2000-01-02"),
      as.Date("2000-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2023-04-01"),
      as.Date("2023-05-02"),
      as.Date("2023-03-01")
    )
  )
  cdm <- PatientProfiles::mockPatientProfiles(
    cohort1 = cohort,
    cohort2 = outcome_cohort,
    observation_period = observation_period
  )
  surv8 <-
    estimateSurvival(
      cdm = cdm,
      targetCohortTable = "cohort1",
      targetCohortId = 1,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 1,
      minCellCount = 1
    )
   expect_true(all(compareNA(surv8 %>% dplyr::select(time) %>% dplyr::pull() %>% unique(), c(0:9,NA))))
   expect_true(all(surv8 %>% dplyr::filter(estimate == "n_risk") %>% dplyr::select(estimate) %>% dplyr::pull() == c(rep(2, 2), rep(1, 8))))
   expect_true(all(surv8 %>%
                     dplyr::filter(estimate_type == "Cumulative failure probability") %>%
                     dplyr::filter(variable_type == "estimate") %>%
                     dplyr::select(estimate) %>% dplyr::pull() - c(0, rep(0.5, 8), 1) < c(0.01)))
   expect_true(all(surv8 %>% dplyr::select(analysis_type) %>% dplyr::pull() == "Single event"))

   expect_true(all(surv8 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::filter(!is.na(estimate) & variable_level == "timeGap 1") %>% dplyr::select(time) == c(0:9)))
   expect_true(all(surv8 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::group_by(variable_level) %>% dplyr::summarise(n = sum(estimate, na.rm = T)) %>% dplyr::select(n) %>% dplyr::pull() == c(2, 2, 2, 2)))

  surv9 <-
    estimateSurvival(
      cdm = cdm,
      targetCohortTable = "cohort1",
      targetCohortId = 2,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 1,
      minCellCount = 1
    )
   expect_true(all(compareNA(surv9 %>% dplyr::select(time) %>% dplyr::pull() %>% unique(), c(0:8,NA))))
   expect_true(all(surv9 %>% dplyr::filter(estimate == "n_risk") %>% dplyr::select(estimate) %>% dplyr::pull() == c(rep(1, 9))))
   expect_true(all(surv9 %>%
                     dplyr::filter(estimate_type == "Cumulative failure probability") %>%
                     dplyr::filter(variable_type == "estimate") %>%
                     dplyr::select(estimate) %>% dplyr::pull() - c(rep(0, 8), 1) < c(0.01)))
   expect_true(all(surv9 %>% dplyr::select(analysis_type) %>% dplyr::pull() == "Single event"))

   expect_true(all(surv9 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::filter(!is.na(estimate) & variable_level == "timeGap 1") %>% dplyr::select(time) == c(0:8)))
   expect_true(all(surv9 %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::group_by(variable_level) %>% dplyr::summarise(n = sum(estimate, na.rm = T)) %>% dplyr::select(n) %>% dplyr::pull() == c(1, 1, 1, 1)))

  CDMConnector::cdm_disconnect(cdm)
  })

test_that("expected errors", {
  cdm <- mockMGUS2cdm()

  expect_error(estimateSurvival("cdm", targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression"))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosiss", outcomeCohortTable = "progression"))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "outcome"))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = c(1, 3)))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, timeGap = -3))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, timeGap = "time"))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, timeGap = NULL))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, strata = "age"))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, strata = list("name" = "noname")))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, censorOnDate = "2020-09-02"))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("return participants", {
  cdm <- mockMGUS2cdm()
  surv1 <- estimateSurvival(cdm,
    targetCohortTable = "mgus_diagnosis",
    outcomeCohortTable = "death_cohort",
    returnParticipants = FALSE
  )
  expect_true(is.null(attr(surv1, "participants")))
  surv2 <- estimateSurvival(cdm,
    targetCohortTable = "mgus_diagnosis",
    targetCohortId = 1,
    outcomeCohortTable = "death_cohort",
    outcomeCohortId = 1,
    timeGap = 7, returnParticipants = TRUE
  )
  expect_false(is.null(attr(surv2, "participants")))

  expect_equal(
    colnames(survivalParticipants(surv2) %>%
      head(1) %>%
      dplyr::collect()),
    c(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    )
  )

  CDMConnector::cdmDisconnect(cdm)
})

test_that("within cohort survival", {
  cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1),
    subject_id = c(1, 2, 3),
    cohort_start_date = c(
      as.Date("2020-01-01"),
      as.Date("2020-01-02"),
      as.Date("2020-01-01")
    ),
    cohort_end_date = c(
      as.Date("2020-04-01"),
      as.Date("2020-08-02"),
      as.Date("2021-03-01")
    )
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 1, 1),
    person_id = c(1, 2, 3),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2000-01-02"),
      as.Date("2000-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2023-04-01"),
      as.Date("2023-05-02"),
      as.Date("2023-03-01")
    )
  )
  cdm <- PatientProfiles::mockPatientProfiles(
    cohort1 = cohort,
    observation_period = observation_period
  )

  surv <- estimateSurvival(cdm,
    targetCohortTable = "cohort1",
    targetCohortId = 1,
    outcomeCohortTable = "cohort1",
    outcomeCohortId = 1,
    outcomeDateVariable = "cohort_end_date",
    timeGap = 7,
    minCellCount = 0
  )
  expect_true(sum(surv %>% dplyr::filter(estimate_type == "Survival events") %>% dplyr::select(estimate) %>% sum()) == 3)

  CDMConnector::cdmDisconnect(cdm)
})
