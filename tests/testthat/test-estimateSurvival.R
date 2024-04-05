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
                                      eventGap = 7
  ) %>% asSurvivalResult()
  expect_true(tibble::is_tibble(surv))
  expect_true(all(c(
    "cdm_name","result_type",
    "cohort",
    "strata_name", "strata_level",
    "variable_name","variable_level",
    "estimate_name",
    "estimate_value",
    "time",
    "outcome",
    "analysis_type") %in%
      colnames(surv)))

  expect_true(surv %>% dplyr::select(time) %>% dplyr::distinct() %>% dplyr::tally() == 425)

  expect_true(attr(surv, "events") %>%
                dplyr::select(variable_level) %>% dplyr::pull() %>% unique() == "death_cohort")
  expect_true(all(attr(surv, "events") %>%
                    dplyr::pull("time") %>% unique() %in% c(seq(0, 424, by = 7), 424)))

  expect_true(tibble::is_tibble(attr(surv, "summary")))

  expect_true(all(surv$variable_level == "death_cohort"))
  expect_true(all(attr(surv, "event")$variable_level == "death_cohort"))
  expect_true(all(attr(surv, "summary")$variable_level == "death_cohort"))

  # mgus example: Competing risk
  survCR <- estimateCompetingRiskSurvival(cdm,
                             targetCohortTable = "mgus_diagnosis",
                             targetCohortId = 1,
                             outcomeCohortTable = "progression",
                             outcomeCohortId = 1,
                             competingOutcomeCohortTable = "death_cohort",
                             competingOutcomeCohortId = 1
  ) %>% asSurvivalResult()

  expect_true(all(colnames(surv) %in% c(colnames(survCR), "competing_outcome")))
  expect_true(tibble::is_tibble(survCR))
  expect_true(all(survCR %>%
                    dplyr::select(variable_level) %>%
                    dplyr::pull() %>%
                    unique() %in%
                    c("death_cohort", "progression")))

  expect_true(all(compareNA(survCR %>%
                              dplyr::pull("time") %>%
                              unique(), c(0:424))))

  expect_true(all(attr(survCR, "events") %>%
                    dplyr::select(variable_level) %>%
                    dplyr::pull() %>%
                    unique() %in% c("progression", "death_cohort")))

  expect_true(all(attr(survCR, "events") %>%
                    dplyr::pull("time") %>% unique() %in% c(0:424)))

  expect_true(nrow(survCR %>%
                     dplyr::filter(.data$variable_level == "death_cohort") %>%
                     dplyr::collect())>=1)
  expect_true(nrow(survCR %>%
                     dplyr::filter(.data$variable_level == "progression") %>%
                     dplyr::collect())>=1)

  expect_true(all(c("death_cohort", "progression") %in%
                    (survCR %>%
                       dplyr::pull("variable_level") %>%
                       unique())))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("mgus example: no Competing risk, strata", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  cdm[["mgus_diagnosis"]] <- cdm[["mgus_diagnosis"]] %>%
    dplyr::mutate(mspike_r = round(mspike, digits = 0))
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      targetCohortId = 1,
                                      outcomeCohortTable = "death_cohort",
                                      outcomeCohortId = 1,
                                      eventGap = c(1, 10, 100),
                                      strata = list(
                                        "age_gr" = c("age"),
                                        "sex" = c("sex"),
                                        "age and sex" = c("age", "sex"),
                                        "mspike rounded" = c("mspike_r")
                                      )
  ) %>% asSurvivalResult()
  expect_true(tibble::is_tibble(surv))

  expect_true(all(surv %>% dplyr::select(variable_level) %>% dplyr::pull() %>% unique() %in% c("death_cohort")))
  expect_true(all(surv %>% dplyr::pull("time") %>% unique() %in% c(0:424)))
  expect_true(all(surv %>% dplyr::select(strata_name) %>% dplyr::pull() %>% unique() %in%
                    c("overall", "sex", "age", "mspike_r", "age and sex")))
  expect_true(all(surv %>% dplyr::select(strata_level) %>% dplyr::pull() %>% unique() %in% c(
    "M", "F", 0, 1, 2, 3, c(24:96), "overall",
    paste(expand.grid(c(24:96), c("M", "F"))$Var1, expand.grid(c(24:96), c("M", "F"))$Var2, sep = " and ")
  )))
  expect_true(all(attr(surv, "events") %>% dplyr::select(strata_name) %>% dplyr::pull() %>% unique() %in%
                    c("overall", "sex", "age", "mspike_r", "age and sex")))
  expect_true(all(attr(surv, "events") %>% dplyr::select(strata_level) %>% dplyr::pull() %>% unique() %in% c(
    "M", "F", 0, 1, 2, 3, c(24:96), "overall",
    paste(expand.grid(c(24:96), c("M", "F"))$Var1, expand.grid(c(24:96), c("M", "F"))$Var2, sep = " and ")
  )))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("mgus example: Competing risk, strata", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()
  cdm[["mgus_diagnosis"]] <- cdm[["mgus_diagnosis"]] %>%
    dplyr::mutate(mspike_r = round(mspike, digits = 0))
  survCR <- estimateCompetingRiskSurvival(cdm,
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
  ) %>% asSurvivalResult()

  expect_true(tibble::is_tibble(survCR))
  expect_true(all(survCR %>% dplyr::select(variable_level) %>% dplyr::pull() %>% unique() %in%
                    c("death_cohort", "progression")))
  expect_true(all(survCR %>% dplyr::pull("time") %>% unique() %in% c(0:424)))
  expect_true(all(survCR %>% dplyr::select(strata_name) %>% dplyr::pull() %>% unique() %in%
                    c("overall", "sex", "age", "mspike_r", "age and sex")))
  expect_true(all(survCR %>% dplyr::select(strata_level) %>% dplyr::pull() %>% unique() %in% c(
    "M", "F", 0, 1, 2, 3, c(24:96), "overall",
    paste(expand.grid(c(24:96), c("M", "F"))$Var1, expand.grid(c(24:96), c("M", "F"))$Var2, sep = " and ")
  )))
  expect_true(all(attr(survCR, "events") %>% dplyr::select(strata_name) %>% dplyr::pull() %>% unique() %in%
                    c("overall", "sex", "age", "mspike_r", "age and sex")))
  expect_true(all(attr(survCR, "events") %>% dplyr::select(strata_level) %>% dplyr::pull() %>% unique() %in% c(
    "M", "F", 0, 1, 2, 3, c(24:96), "overall",
    paste(expand.grid(c(24:96), c("M", "F"))$Var1, expand.grid(c(24:96), c("M", "F"))$Var2, sep = " and ")
  )))
  # strata with only one value
  cdm$mgus_diagnosis <- cdm$mgus_diagnosis %>% dplyr::mutate(a = "X")
  survCR <- estimateCompetingRiskSurvival(cdm,
                             targetCohortTable = "mgus_diagnosis",
                             targetCohortId = 1,
                             outcomeCohortTable = "progression",
                             outcomeCohortId = 1,
                             competingOutcomeCohortTable = "death_cohort",
                             competingOutcomeCohortId = 1,
                             strata = list("a" = "a")) %>% asSurvivalResult()

  CDMConnector::cdmDisconnect(cdm)
})

test_that("multiple exposures, multiple outcomes: single event", {
  skip_on_cran()

  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3, 4, 5,6),
    person_id = c(1, 2, 3, 4, 5,6),
    observation_period_start_date = c(
      rep(as.Date("1980-07-20"),6)
    ),
    observation_period_end_date = c(
      rep(as.Date("2023-05-20"),6)
    ),
    period_type_concept_id = c(rep(0,6))
  )

  exposure_cohort <- dplyr::tibble(
    subject_id = c(1, 2, 3, 3, 4, 5),
    cohort_definition_id = c(1, 1, 1, 2, 2, 2),
    cohort_start_date = c(
      as.Date("2020-01-01"),
      as.Date("2020-02-03"),
      as.Date("2020-05-01"),
      as.Date("2020-05-01"),
      as.Date("2020-08-01"),
      as.Date("2020-09-01")
    ),
    cohort_end_date = c(
      as.Date("2020-01-31"),
      as.Date("2022-02-03"),
      as.Date("2021-06-28"),
      as.Date("2021-06-01"),
      as.Date("2021-08-01"),
      as.Date("2021-09-01")
    )
  )


  outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(2, 3, 3),
    subject_id = c(2, 3, 4),
    cohort_start_date = c(
      as.Date("2021-01-01"),
      as.Date("2021-01-01"),
      as.Date("2021-01-01")
    ),
    cohort_end_date = c(
      as.Date("2021-01-01"),
      as.Date("2021-01-01"),
      as.Date("2021-01-01")
    )
  )

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      exposure_cohort = exposure_cohort,
      cohort1 = outcome_cohort
    ),
    cdmName = "mock_es"
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)
  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  # one target, one outcome
  expect_no_error(surv <- estimateSingleEventSurvival(cdm2,
                                                      targetCohortTable = "exposure_cohort",
                                                      targetCohortId = 1,
                                                      outcomeCohortTable = "cohort1",
                                                      outcomeCohortId = 2
  ) %>% asSurvivalResult())
  expect_equal(unique(surv$cohort),
               omopgenerics::settings(cdm$exposure_cohort) %>%
                 dplyr::filter(cohort_definition_id == 1) %>%
                 dplyr::pull("cohort_name"))
  expect_equal(unique(surv$variable_level),
               omopgenerics::settings(cdm$cohort1) %>%
                 dplyr::filter(cohort_definition_id == 2) %>%
                 dplyr::pull("cohort_name"))

  # two target, one outcome
  surv <- estimateSingleEventSurvival(cdm2,
                                      targetCohortTable = "exposure_cohort",
                                      targetCohortId = c(1,2),
                                      outcomeCohortTable = "cohort1",
                                      outcomeCohortId = 2
  ) %>% asSurvivalResult()
  expect_equal(sort(unique(surv$cohort)),
               sort(omopgenerics::settings(cdm$exposure_cohort) %>%
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) %>%
                      dplyr::pull("cohort_name")))
  expect_equal(unique(surv$variable_level),
               omopgenerics::settings(cdm$cohort1) %>%
                 dplyr::filter(cohort_definition_id == 2) %>%
                 dplyr::pull("cohort_name"))

  # two target, two outcome
  surv <- estimateSingleEventSurvival(cdm2,
                                      targetCohortTable = "exposure_cohort",
                                      targetCohortId = c(1,2),
                                      outcomeCohortTable = "cohort1",
                                      outcomeCohortId = c(2,3),
                                      minCellCount = 0
  ) %>% asSurvivalResult()
  expect_equal(sort(unique(surv$cohort)),
               sort(omopgenerics::settings(cdm$exposure_cohort) %>%
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) %>%
                      dplyr::pull("cohort_name")))
  expect_equal(unique(surv$variable_level),
               omopgenerics::settings(cdm$cohort1) %>%
                 dplyr::filter(cohort_definition_id %in%  c(2,3)) %>%
                 dplyr::pull("cohort_name"))

  expect_equal(sort(unique(attr(surv, "event")$cohort)),
               sort(omopgenerics::settings(cdm$exposure_cohort) %>%
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) %>%
                      dplyr::pull("cohort_name")))
  expect_equal(unique(attr(surv, "event")$variable_level),
               omopgenerics::settings(cdm$cohort1) %>%
                 dplyr::filter(cohort_definition_id %in%  c(2,3)) %>%
                 dplyr::pull("cohort_name"))

  expect_equal(sort(unique(attr(surv, "summary")$cohort)),
               sort(omopgenerics::settings(cdm$exposure_cohort) %>%
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) %>%
                      dplyr::pull("cohort_name")))
  expect_equal(unique(attr(surv, "summary")$variable_level),
               omopgenerics::settings(cdm$cohort1) %>%
                 dplyr::filter(cohort_definition_id %in%  c(2,3)) %>%
                 dplyr::pull("cohort_name"))

  # two target, two outcome - without specifying
  surv <- estimateSingleEventSurvival(cdm2,
                                      targetCohortTable = "exposure_cohort",
                                      outcomeCohortTable = "cohort1"
  ) %>% asSurvivalResult()
  expect_equal(sort(unique(surv$cohort)),
               sort(omopgenerics::settings(cdm$exposure_cohort) %>%
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) %>%
                      dplyr::pull("cohort_name")))
  expect_equal(unique(surv$variable_level),
               omopgenerics::settings(cdm$cohort1) %>%
                 dplyr::filter(cohort_definition_id %in%  c(2,3)) %>%
                 dplyr::pull("cohort_name"))

  CDMConnector::cdmDisconnect(cdm2)

})

test_that("multiple exposures, multiple outcomes: competing risk", {
  skip_on_cran()

  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3, 4, 5,6),
    person_id = c(1, 2, 3, 4, 5,6),
    observation_period_start_date = c(
      rep(as.Date("1980-07-20"),6)
    ),
    observation_period_end_date = c(
      rep(as.Date("2023-05-20"),6)
    ),
    period_type_concept_id = c(rep(0,6))
  )

  exposure_cohort <- dplyr::tibble(
    subject_id = c(1, 2, 3, 3, 4, 5,6,6),
    cohort_definition_id = c(1, 1, 1, 2, 2, 2,1,2),
    cohort_start_date = c(
      as.Date("2020-01-01"),
      as.Date("2020-02-03"),
      as.Date("2020-05-01"),
      as.Date("2020-05-01"),
      as.Date("2020-08-01"),
      as.Date("2020-09-01"),
      as.Date("2020-09-01"),
      as.Date("2020-09-01")
    ),
    cohort_end_date = c(
      as.Date("2020-01-31"),
      as.Date("2022-02-03"),
      as.Date("2021-06-28"),
      as.Date("2021-06-01"),
      as.Date("2021-08-01"),
      as.Date("2021-09-01"),
      as.Date("2021-09-01"),
      as.Date("2021-09-01")
    )
  )

  outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(2,2, 3, 3),
    subject_id = c(2, 3, 3, 4),
    cohort_start_date = c(
      as.Date("2021-01-01"),
      as.Date("2021-01-01"),
      as.Date("2021-01-01"),
      as.Date("2021-01-01")
    ),
    cohort_end_date = c(
      as.Date("2021-01-01"),
      as.Date("2021-01-01"),
      as.Date("2021-01-01"),
      as.Date("2021-01-01")
    )
  )

  competing_cohort <- dplyr::tibble(
    cohort_definition_id = c(4,5, 4, 5),
    subject_id = c(1,1, 5, 5),
    cohort_start_date = c(
      as.Date("2020-11-01"),
      as.Date("2020-11-01"),
      as.Date("2020-11-01"),
      as.Date("2020-11-01")
    ),
    cohort_end_date = c(
      as.Date("2020-11-01"),
      as.Date("2020-11-01"),
      as.Date("2020-11-01"),
      as.Date("2020-11-01")
    )
  )

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      exposure_cohort = exposure_cohort,
      cohort1 = outcome_cohort,
      cohort2 = competing_cohort
    ),
    cdmName = "mock_es"
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  # one target, one outcome
  expect_no_error(surv <- estimateCompetingRiskSurvival(cdm2,
                                                        targetCohortTable = "exposure_cohort",
                                                        targetCohortId = 1,
                                                        outcomeCohortTable = "cohort1",
                                                        outcomeCohortId = 2,
                                                        competingOutcomeCohortTable  = "cohort2",
                                                        competingOutcomeCohortId = 4
  ) %>% asSurvivalResult())
  expect_equal(unique(surv$cohort),
               omopgenerics::settings(cdm2$exposure_cohort) %>%
                 dplyr::filter(cohort_definition_id == 1) %>%
                 dplyr::pull("cohort_name"))
  expect_equal(sort(unique(surv$variable_level)),
               sort(c(omopgenerics::settings(cdm2$cohort1) %>%
                        dplyr::filter(cohort_definition_id == 2) %>%
                        dplyr::pull("cohort_name"),
                      omopgenerics::settings(cdm2$cohort2) %>%
                        dplyr::filter(cohort_definition_id == 4) %>%
                        dplyr::pull("cohort_name"))))

  # two target, one outcome, one competing risk
  expect_no_error(surv <- estimateCompetingRiskSurvival(cdm2,
                                                        targetCohortTable = "exposure_cohort",
                                                        targetCohortId = c(1,2),
                                                        outcomeCohortTable = "cohort1",
                                                        outcomeCohortId = 2,
                                                        competingOutcomeCohortTable  = "cohort2",
                                                        competingOutcomeCohortId = 4
  ) %>% asSurvivalResult())
  expect_equal(sort(unique(surv$cohort)),
               sort(omopgenerics::settings(cdm2$exposure_cohort) %>%
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) %>%
                      dplyr::pull("cohort_name")))
  expect_equal(sort(unique(surv$variable_level)),
               sort(c(omopgenerics::settings(cdm2$cohort1) %>%
                        dplyr::filter(cohort_definition_id == 2) %>%
                        dplyr::pull("cohort_name"),
                      omopgenerics::settings(cdm2$cohort2) %>%
                        dplyr::filter(cohort_definition_id == 4) %>%
                        dplyr::pull("cohort_name"))))

  # two target, two outcome, one competing risk
  surv <- estimateCompetingRiskSurvival(cdm2,
                                        targetCohortTable = "exposure_cohort",
                                        targetCohortId = c(1,2),
                                        outcomeCohortTable = "cohort1",
                                        outcomeCohortId = c(2,3),
                                        competingOutcomeCohortTable  = "cohort2",
                                        competingOutcomeCohortId = 4
  ) %>% asSurvivalResult()
  expect_equal(sort(unique(surv$cohort)),
               sort(omopgenerics::settings(cdm2$exposure_cohort) %>%
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) %>%
                      dplyr::pull("cohort_name")))
  expect_equal(sort(unique(surv$variable_level)),
               sort(c(omopgenerics::settings(cdm2$cohort1) %>%
                        dplyr::filter(cohort_definition_id %in%  c(2,3)) %>%
                        dplyr::pull("cohort_name"),
                      omopgenerics::settings(cdm2$cohort2) %>%
                        dplyr::filter(cohort_definition_id == 4) %>%
                        dplyr::pull("cohort_name"))))

  # two target, two outcome, two competing risk
  surv <- estimateCompetingRiskSurvival(cdm2,
                                        targetCohortTable = "exposure_cohort",
                                        targetCohortId = c(1,2),
                                        outcomeCohortTable = "cohort1",
                                        outcomeCohortId = c(2,3),
                                        competingOutcomeCohortTable  = "cohort2",
                                        competingOutcomeCohortId = c(4,5),
                                        minCellCount = 1
  ) %>% asSurvivalResult()
  expect_equal(sort(unique(surv$cohort)),
               sort(omopgenerics::settings(cdm2$exposure_cohort) %>%
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) %>%
                      dplyr::pull("cohort_name")))
  expect_equal(sort(unique(surv$variable_level)),
               sort(c(omopgenerics::settings(cdm2$cohort1) %>%
                        dplyr::filter(cohort_definition_id %in%  c(2,3)) %>%
                        dplyr::pull("cohort_name"),
                      omopgenerics::settings(cdm2$cohort2) %>%
                        dplyr::filter(cohort_definition_id %in% c(4,5)) %>%
                        dplyr::pull("cohort_name"))))

  #  two target, two outcome, two competing risk - without specifying
  surv <- estimateCompetingRiskSurvival(cdm2,
                                        targetCohortTable = "exposure_cohort",
                                        outcomeCohortTable = "cohort1",
                                        competingOutcomeCohortTable  = "cohort2"
  ) %>% asSurvivalResult()
  expect_equal(sort(unique(surv$cohort)),
               sort(omopgenerics::settings(cdm2$exposure_cohort) %>%
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) %>%
                      dplyr::pull("cohort_name")))
  expect_equal(sort(unique(surv$variable_level)),
               sort(c(omopgenerics::settings(cdm2$cohort1) %>%
                        dplyr::filter(cohort_definition_id %in%  c(2,3)) %>%
                        dplyr::pull("cohort_name"),
                      omopgenerics::settings(cdm2$cohort2) %>%
                        dplyr::filter(cohort_definition_id %in% c(4,5)) %>%
                        dplyr::pull("cohort_name"))))

  CDMConnector::cdmDisconnect(cdm2)

})

test_that("required estimateGap", {
  skip_on_cran()

  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3, 4, 5),
    person_id = c(1, 2, 3, 4, 5),
    observation_period_start_date = c(
      rep(as.Date("1980-07-20"),5)
    ),
    observation_period_end_date = c(
      rep(as.Date("2023-05-20"),5)
    ),
    period_type_concept_id = c(rep(0,5))
  )

  exposure_cohort <- dplyr::tibble(
    subject_id = c(1, 2, 3, 3, 4, 5),
    cohort_definition_id = c(1, 1, 1, 2, 2, 2),
    cohort_start_date = c(
      as.Date("2020-01-01"),
      as.Date("2020-02-03"),
      as.Date("2020-05-01"),
      as.Date("2020-05-01"),
      as.Date("2020-08-01"),
      as.Date("2020-09-01")
    ),
    cohort_end_date = c(
      as.Date("2020-01-31"),
      as.Date("2022-02-03"),
      as.Date("2021-06-28"),
      as.Date("2021-06-01"),
      as.Date("2021-08-01"),
      as.Date("2021-09-01")
    )
  )


  outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(2, 3, 3),
    subject_id = c(2, 3, 4),
    cohort_start_date = c(
      as.Date("2021-01-01"),
      as.Date("2021-01-01"),
      as.Date("2021-01-01")
    ),
    cohort_end_date = c(
      as.Date("2021-01-01"),
      as.Date("2021-01-01"),
      as.Date("2021-01-01")
    )
  )

  competing_cohort <- dplyr::tibble(
    cohort_definition_id = c(4,5, 4, 5),
    subject_id = c(1,1, 5, 5),
    cohort_start_date = c(
      as.Date("2020-11-01"),
      as.Date("2020-11-01"),
      as.Date("2020-11-01"),
      as.Date("2020-11-01")
    ),
    cohort_end_date = c(
      as.Date("2020-11-01"),
      as.Date("2020-11-01"),
      as.Date("2020-11-01"),
      as.Date("2020-11-01")
    )
  )

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      exposure_cohort = exposure_cohort,
      cohort1 = outcome_cohort,
      cohort2 = competing_cohort
    ),
    cdmName = "mock_es"
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  # one target, one outcome
  surv <- estimateSingleEventSurvival(cdm2,
                                      targetCohortTable = "exposure_cohort",
                                      targetCohortId = 1,
                                      outcomeCohortTable = "cohort1",
                                      outcomeCohortId = 2
  ) %>% asSurvivalResult()

  surv_pair <- estimateSingleEventSurvival(cdm2,
                                           targetCohortTable = "exposure_cohort",
                                           targetCohortId = 1,
                                           outcomeCohortTable = "cohort1",
                                           outcomeCohortId = 2,
                                           estimateGap = 2
  ) %>% asSurvivalResult()

  surv2 <- surv %>%
    dplyr::filter(time %in% seq(0, 1235, by = 2))

  expect_true(all.equal(surv_pair, surv2))

  # two targets, two outcomes, competing risk event
  survCR <- estimateCompetingRiskSurvival(cdm2,
                                          targetCohortTable = "exposure_cohort",
                                          targetCohortId = c(1,2),
                                          outcomeCohortTable = "cohort1",
                                          outcomeCohortId = c(2,3),
                                          competingOutcomeCohortTable  = "cohort2",
                                          competingOutcomeCohortId = 4
  ) %>% asSurvivalResult()

  survCR_time <- estimateCompetingRiskSurvival(cdm2,
                                               targetCohortTable = "exposure_cohort",
                                               targetCohortId = c(1,2),
                                               outcomeCohortTable = "cohort1",
                                               outcomeCohortId = c(2,3),
                                               competingOutcomeCohortTable  = "cohort2",
                                               competingOutcomeCohortId = 4,
                                               estimateGap = 2
  ) %>% asSurvivalResult()

  survCR <- survCR %>%
    dplyr::filter(time %in% seq(0,1235,by = 2))

  expect_true(all.equal(survCR, survCR_time))

  CDMConnector::cdmDisconnect(cdm2)

})

test_that("funcionality with created dataset", {
  skip_on_cran()
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
    ),
    period_type_concept_id = c(rep(0,3))
  )

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      exposure_cohort = exposure_cohort,
      cohort1 = outcome_cohort
    ),
    cdmName = "mock_es"
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  # No competing events
  surv <- estimateSingleEventSurvival(cdm2,
                                      targetCohortTable = "exposure_cohort",
                                      outcomeCohortTable = "cohort1",
                                      minCellCount = 1
  ) %>% asSurvivalResult()

  expect_true(all(surv %>%
                    dplyr::pull("time") %>%
                    unique() %in% c(0:31)))
  expect_true(all(attr(surv, "events") %>%
                    dplyr::filter(estimate_name == "n_risk") %>%
                    dplyr::select(estimate_value) %>%
                    dplyr::pull() ==
                    c(3, 1, 1)))
  expect_true(all(surv %>%
                    dplyr::filter(estimate_name == "estimate")  %>%
                    dplyr::select(estimate_value) %>%
                    dplyr::pull() - c(rep(1, 7), rep(0.667, 3), rep(0.333, 21), 0) < c(0.01)))
#  expect_true(all(surv %>%
#                    dplyr::filter(estimate_name == "estimate")  %>%
#                    dplyr::filter(estimate_type == "Cumulative failure probability") %>%
#                    dplyr::select(estimate_value) %>% dplyr::pull() - c(rep(0, 6), rep(0.333, 3), rep(0.667, 22), 1) < c(0.01)))

  expect_true(all(attr(surv, "events") %>% dplyr::select(variable_level) %>% dplyr::pull() %in% c("cohort_1")))
  expect_true(all(attr(surv, "events") %>%
                    dplyr::filter(time == 0,
                                  estimate_name == "n_risk") %>%
                    dplyr::pull("estimate_value") %in%  c(3)))

  CDMConnector::cdmDisconnect(cdm2)

  # Competing events
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
  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      exposure_cohort = exposure_cohort,
      cohort1 = outcome_cohort,
      cohort2 = competing_risk_cohort
    ),
    cdmName = "mock_es"
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  surv2 <- estimateCompetingRiskSurvival(cdm2,
                                         targetCohortTable = "exposure_cohort",
                                         outcomeCohortTable = "cohort1",
                                         competingOutcomeCohortTable = "cohort2",
                                         competingOutcomeCohortId = 1,
                                         minCellCount = 1
  ) %>% asSurvivalResult()

  expect_true(all(surv2 %>%
                    dplyr::pull("time") %>%
                    unique() %in% c(0:981)))
  expect_true(all(attr(surv2, "events") %>%
                    dplyr::filter(variable_level == "cohort_1") %>%
                    dplyr::filter(estimate_name == "n_risk") %>%
                    dplyr::select(estimate_value) %>%
                    dplyr::pull() ==
                    c(3, 2, rep(1, 32))))

  expect_true(all(surv2 %>% dplyr::select(variable_level) %>% dplyr::pull() %in% c("cohort_1", "cohort_1_competing_outcome")))
  expect_true(all(attr(surv2, "events") %>%
                    dplyr::pull("time") %in%  c(seq(0,981, by = 30), 981)))

  CDMConnector::cdmDisconnect(cdm2)

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

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      exposure_cohort = exposure_cohort,
      cohort1 = outcome_cohort
    ),
    cdmName = "mock_es"
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  surv3 <- estimateSingleEventSurvival(cdm2, "exposure_cohort",
                                       outcomeCohortTable = "cohort1",
                                       censorOnCohortExit = TRUE,
                                       minCellCount = 1
  ) %>% asSurvivalResult()

  expect_true(all(surv3 %>%
                    dplyr::pull("time") %>%
                    unique() %in% c(0:31)))
  expect_true(all(attr(surv3, "events") %>% dplyr::filter(estimate_name == "n_risk") %>%
                    dplyr::select(estimate_value) %>% dplyr::pull() == c(3, 2, 1)))
  expect_true(all(surv3  %>%
                    dplyr::filter(estimate_name == "estimate") %>%
                    dplyr::select(estimate_value) %>% dplyr::pull() - c(rep(1, 6), rep(0.667, 25), 0) < c(0.01)))

  CDMConnector::cdmDisconnect(cdm2)

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      exposure_cohort = exposure_cohort,
      cohort1 = outcome_cohort
    ),
    cdmName = "mock_es"
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  # Censor by follow up days
  surv4 <- estimateSingleEventSurvival(cdm2, "exposure_cohort",
                                       outcomeCohortTable = "cohort1",
                                       followUpDays = 10,
                                       minCellCount = 1
  ) %>% asSurvivalResult()

  expect_true(all(surv4 %>%
                    dplyr::pull("time") %>%
                    unique() %in% c(0:10)))
  expect_true(all(attr(surv4, "events") %>% dplyr::filter(estimate_name == "n_risk") %>% dplyr::select(estimate_value) %>% dplyr::pull() == c(3, 2)))
  expect_true(all(surv4 %>%
                    dplyr::filter(estimate_name == "estimate") %>%
                    dplyr::select(estimate_value) %>% dplyr::pull() - c(rep(1, 6), rep(0.667, 5)) < c(0.01)))
  expect_true(all(attr(surv4, "events") %>%
                    dplyr::pull("time") %in%  c(0:10)))

  # if followUpDays larger than last of the times, follow until then
  surv4b <- estimateSingleEventSurvival(cdm2, "exposure_cohort",
                                        outcomeCohortTable = "cohort1",
                                        followUpDays = 40,
                                        minCellCount = 1
  ) %>% asSurvivalResult()

  expect_true(all(attr(surv4, "events") %>%
                    dplyr::pull("time") %in%  c(0:40)))

  CDMConnector::cdmDisconnect(cdm2)

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

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      exposure_cohort = exposure_cohort,
      cohort1 = outcome_cohort
    ),
    cdmName = "mock_es"
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  # Strata
  surv5 <- estimateSingleEventSurvival(cdm2, "exposure_cohort",
                                       outcomeCohortTable = "cohort1",
                                       strata = list(
                                         "Age group" = c("age_group"),
                                         "Sex" = c("sex"),
                                         "Age group and sex" = c("age_group", "sex"),
                                         "Blood type" = c("blood_type")
                                       ),
                                       minCellCount = 1
  ) %>% asSurvivalResult()

  expect_true(all(surv5 %>%
                    dplyr::filter(strata_name == "overall") %>%
                    dplyr::pull("time") %>%
                    unique() %in% c(0:31)))
  expect_true(all(attr(surv5, "events") %>% dplyr::filter(strata_name == "overall") %>% dplyr::filter(estimate_name == "n_risk") %>% dplyr::select(estimate_value) %>% dplyr::pull() == c(3, 1, 1)))
  expect_true(all(surv5 %>% dplyr::filter(strata_name == "overall") %>%
                    dplyr::filter(estimate_name == "estimate") %>%
                    dplyr::select(estimate_value) %>% dplyr::pull() - c(rep(1, 6), rep(0.667, 3), rep(0.333, 22), 0) < c(0.01)))

  expect_true(all(surv5 %>% dplyr::filter(strata_name == "age_group; sex" &
                                            strata_level == "20;29; Female") %>%
                    dplyr::select("time") %>% dplyr::pull() %in% c(0:31)))
  expect_true(all(compareNA(attr(surv5, "events") %>% dplyr::filter(strata_name == "age_group; sex" &
                                                                      strata_level == "20;29; Female") %>%
                              dplyr::filter(estimate_name == "n_risk") %>% dplyr::select(estimate_value) %>%
                              dplyr::pull(), c(1,1))))
  expect_true(all(surv5 %>% dplyr::filter(strata_name == "age_group; sex" &
                                            strata_level == "20;29; Female") %>%
                    dplyr::filter(estimate_name == "estimate") %>%
                    dplyr::select(estimate_value) %>% dplyr::pull() - c(rep(1, 9), 0) < c(0.01)))

  expect_true(all(attr(surv5, "events") %>%
                    dplyr::filter(strata_name == "age_group; sex" &
                                    strata_level == "20;29; Female") %>%
                    dplyr::pull("time") %in%  c(0,9)))

  expect_true(all(surv5 %>% dplyr::filter(estimate_name == "estimate" & strata_name == "blood_type" & strata_level == "B") %>%
                    dplyr::select(time) %>% dplyr::pull() == c(0:31)))
  expect_true(all(surv5 %>% dplyr::filter(estimate_name == "n_risk" & strata_name == "blood_type" & strata_level == "B") %>% dplyr::select(estimate_value) %>% dplyr::pull() == c(rep(2, 7), rep(1, 25))))
  expect_true(all(surv5 %>% dplyr::filter(estimate_name == "estimate" & strata_name == "blood_type" & strata_level == "B") %>%
                    dplyr::select(estimate_value) %>% dplyr::pull() - c(rep(1, 6), rep(0.5, 25), 0) < c(0.01)))

  CDMConnector::cdmDisconnect(cdm2)

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

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      exposure_cohort = exposure_cohort,
      cohort1 = outcome_cohort
    ),
    cdmName = "mock_es"
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  # Washout for outcome
  surv6 <- estimateSingleEventSurvival(cdm2,
                                       targetCohortTable = "exposure_cohort",
                                       outcomeCohortTable = "cohort1",
                                       minCellCount = 1
  ) %>% asSurvivalResult()

  expect_true(all(surv6 %>%
                    dplyr::pull("time") %>%
                    unique() %in% c(0:31)))
  expect_true(all(attr(surv6, "events") %>% dplyr::filter(estimate_name == "n_risk") %>% dplyr::select(estimate_value) %>% dplyr::pull() == c(2, 1, 1)))
  expect_true(all(surv6 %>%
                    dplyr::filter(estimate_name == "estimate") %>%
                    dplyr::select(estimate_value) %>% dplyr::pull() - c(rep(1, 6), rep(0.5, 25), 0) < c(0.01)))

  CDMConnector::cdmDisconnect(cdm2)

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

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      exposure_cohort = exposure_cohort,
      cohort1 = outcome_cohort
    ),
    cdmName = "mock_es"
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  # Censor on date
  surv7 <- estimateSingleEventSurvival(cdm2,
                                       targetCohortTable = "exposure_cohort",
                                       outcomeCohortTable = "cohort1",
                                       minCellCount = 1,
                                       censorOnDate = as.Date("2020-05-04")
  ) %>% asSurvivalResult()

  expect_true(all(surv7 %>%
                    dplyr::pull("time") %>%
                    unique() %in% c(0:9)))
  expect_true(all(attr(surv7, "events") %>% dplyr::filter(estimate_name == "n_risk") %>% dplyr::select(estimate_value) %>% dplyr::pull() == c(3, 1)))
  expect_true(all(surv7 %>%
                    dplyr::filter(estimate_name == "estimate") %>%
                    dplyr::select(estimate_value)%>% dplyr::pull() - c(rep(1, 6), 0.5, 0.5, 0.5, 0) < c(0.01)))

  CDMConnector::cdmDisconnect(cdm2)
})

test_that("different exposure cohort ids", {
  skip_on_cran()
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
    ),
    period_type_concept_id = c(rep(0,3))
  )
  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      cohort1 = cohort,
      cohort2 = outcome_cohort
    ),
    cdmName = "mock_es"
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  surv8 <-
    estimateSingleEventSurvival(
      cdm = cdm2,
      targetCohortTable = "cohort1",
      targetCohortId = 1,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 1,
      minCellCount = 0
    ) %>% asSurvivalResult()
  expect_true(all(surv8 %>%
                    dplyr::pull("time") %>% unique() %in% c(0:9)))
  expect_true(all(attr(surv8, "events") %>% dplyr::filter(estimate_name == "n_risk") %>% dplyr::select(estimate_value) %>% dplyr::pull() == c(2, 1)))
  expect_true(all(surv8 %>%
                    dplyr::filter(estimate_name == "estimate") %>%
                    dplyr::select(estimate_value) %>% dplyr::pull() - c(1, rep(0.5, 8), 0) < c(0.01)))

  expect_true(all(attr(surv8, "events") %>% dplyr::filter(!is.na(estimate_value) & estimate_name =="n_risk") %>%
                    dplyr::select(time) == c(0,9)))
  expect_true(all(attr(surv8, "events") %>%
                    dplyr::filter(estimate_name =="n_risk") %>%
                    dplyr::filter(time == 0) %>% dplyr::select(estimate_value) %>%
                    dplyr::pull() == c(2, 2, 2, 2)))

  surv9 <-
    estimateSingleEventSurvival(
      cdm = cdm2,
      targetCohortTable = "cohort1",
      targetCohortId = 2,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 1,
      minCellCount = 1
    ) %>% asSurvivalResult()

  expect_true(all(surv9 %>% dplyr::pull("time") %>% unique() %in% c(0:8)))
  expect_true(all(surv9 %>% dplyr::filter(estimate_name == "n_risk") %>% dplyr::select(estimate_value) %>% dplyr::pull() == c(rep(1, 9))))
  expect_true(all(surv9 %>%
                    dplyr::filter(estimate_name == "estimate") %>%
                    dplyr::select(estimate_value) %>% dplyr::pull() - c(rep(1, 8), 0) < c(0.01)))

  expect_true(all(attr(surv9, "events") %>% dplyr::filter(!is.na(estimate_value)) %>%
                    dplyr::pull("time") %in%  c(0:8)))
  expect_true(all(attr(surv9, "events") %>%
                    dplyr::filter(estimate_name =="n_risk") %>%
                    dplyr::filter(time == 0) %>% dplyr::select(estimate_value) %>%
                    dplyr::pull() %in%  c(1)))

  CDMConnector::cdm_disconnect(cdm2)
})

test_that("expected errors", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()

  expect_error(estimateSurvival("cdm", targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression"))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosiss", outcomeCohortTable = "progression"))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "outcome"))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = c(1, 3)))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, eventGap = -3))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, eventGap = "time"))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, eventGap = NULL))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, strata = "age"))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, strata = list("name" = "noname")))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, censorOnDate = "2020-09-02"))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, minimumSurvivalDays = -3))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, minimumSurvivalDays = c(0,3)))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, outcomeWashout = 0))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, competingOutcomeWashout = "1"))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("return participants", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()
  surv1 <- estimateSingleEventSurvival(cdm,
                                       targetCohortTable = "mgus_diagnosis",
                                       outcomeCohortTable = "death_cohort",
                                       returnParticipants = FALSE
  ) %>% asSurvivalResult()
  expect_true(is.null(attr(surv1, "participants")))
  surv2 <- estimateSingleEventSurvival(cdm,
                                       targetCohortTable = "mgus_diagnosis",
                                       targetCohortId = 1,
                                       outcomeCohortTable = "death_cohort",
                                       outcomeCohortId = 1,
                                       eventGap = 7, returnParticipants = TRUE
  ) %>% asSurvivalResult()
  expect_false(is.null(attr(surv2, "participants")))

  expect_equal(
    colnames(survivalParticipants(surv2) %>%
               head(1) %>%
               dplyr::collect()),
    c(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date",
      "exposure_id",
      "outcome_id"
    )
  )

  CDMConnector::cdmDisconnect(cdm)
})

test_that("within cohort survival", {
  skip_on_cran()
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
    ),
    period_type_concept_id = c(rep(0,3))
  )

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      cohort1 = cohort
    ),
    cdmName = "mock_es"
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  surv <- estimateSingleEventSurvival(cdm2,
                                      targetCohortTable = "cohort1",
                                      targetCohortId = 1,
                                      outcomeCohortTable = "cohort1",
                                      outcomeCohortId = 1,
                                      outcomeDateVariable = "cohort_end_date",
                                      eventGap = 7,
                                      minCellCount = 0
  ) %>% asSurvivalResult()
  expect_true(max(attr(surv, "events") %>% dplyr::select(estimate_value) %>% dplyr::pull(), na.rm = TRUE) == 3)

  CDMConnector::cdmDisconnect(cdm2)
})

test_that("strata specific survival", {
  skip_on_cran()

  exposure_cohort <- dplyr::tibble(
    subject_id = c(1, 2, 3, 4, 5),
    cohort_definition_id = c(1, 1, 1,1,1),
    cohort_start_date = c(
      as.Date("2008-01-01"),
      as.Date("2010-01-01"),
      as.Date("2000-01-01"),
      as.Date("2010-01-01"),
      as.Date("2010-01-01")
    ),
    cohort_end_date = c(
      as.Date("2020-01-31"),
      as.Date("2012-01-01"),
      as.Date("2021-06-28"),
      as.Date("2012-01-01"),
      as.Date("2012-01-01")
    ),
    sex = c("Female", "Male", "Female", "Male", "Male")
  )
  outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 1, 2, 3, 3),
    cohort_start_date = c(
      as.Date("2020-01-10"),
      as.Date("2020-02-02"),
      as.Date("2011-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    ),
    cohort_end_date = c(
      as.Date("2020-01-10"),
      as.Date("2020-02-02"),
      as.Date("2011-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    )
  )
  other_outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(1),
    subject_id = c(4),
    cohort_start_date = c(
      as.Date("2011-02-09")
    ),
    cohort_end_date = c(
      as.Date("2011-02-09")
    )
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3,4,5),
    person_id = c(1, 2, 3,4,5),
    observation_period_start_date = c(
      as.Date("2007-03-21"),
      as.Date("2009-09-09"),
      as.Date("1980-07-20"),
      as.Date("2009-09-09"),
      as.Date("2009-09-09")
    ),
    observation_period_end_date = c(
      as.Date("2022-09-08"),
      as.Date("2022-01-03"),
      as.Date("2023-05-20"),
      as.Date("2015-01-03"),
      as.Date("2015-01-05")
    ),
    period_type_concept_id = c(rep(0,5))
  )

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      exposure_cohort = exposure_cohort,
      cohort1 = outcome_cohort,
      cohort2 = other_outcome_cohort
    ),
    cdmName = "mock_es"
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  surv <- estimateSingleEventSurvival(cdm2,
                                      targetCohortTable = "exposure_cohort",
                                      outcomeCohortTable = "cohort1",
                                      strata = list("sex"),
                                      minCellCount = 1
  ) %>% asSurvivalResult()
  surv_cr <- estimateCompetingRiskSurvival(cdm2,
                                           targetCohortTable = "exposure_cohort",
                                           outcomeCohortTable = "cohort1",
                                           competingOutcomeCohortTable = "cohort2",
                                           strata = list("sex"),
                                           minCellCount = 1
  ) %>% asSurvivalResult()

  # only males
  cdm2[["exposure_cohort_m"]] <- cdm2$exposure_cohort %>%
    dplyr::filter(sex =="Male") %>%
    dplyr::compute()
  surv_m <- estimateSingleEventSurvival(cdm2,
                                        targetCohortTable = "exposure_cohort_m",
                                        outcomeCohortTable = "cohort1",
                                        minCellCount = 1
  ) %>% asSurvivalResult()
  surv_cr_m <- estimateCompetingRiskSurvival(cdm2,
                                             targetCohortTable = "exposure_cohort_m",
                                             outcomeCohortTable = "cohort1",
                                             competingOutcomeCohortTable = "cohort2",
                                             minCellCount = 1
  ) %>% asSurvivalResult()

  # overall result should now be the same as the strata of males before filtering
  expect_equal(surv %>%
                 dplyr::filter(!is.na(time)) %>%
                 dplyr::filter(strata_name == "sex" & strata_level =="Male") %>%
                 dplyr::pull(),
               surv_m  %>%
                 dplyr::filter(!is.na(time)) %>%
                 dplyr::pull()
  )
  expect_equal(surv %>%
                 dplyr::filter(is.na(time)) %>%
                 dplyr::filter(strata_name == "sex" & strata_level =="Male") %>%
                 dplyr::pull(),
               surv_m  %>%
                 dplyr::filter(is.na(time)) %>%
                 dplyr::pull()
  )
  expect_equal(
    surv %>%
      dplyr::filter(strata_name == "sex" & strata_level =="Male") %>%
      dplyr::select("estimate_value") %>%
      dplyr::pull(),
    surv_m %>%
      dplyr::select("estimate_value") %>%
      dplyr::pull()
  )


  expect_equal(surv_cr %>%
                 dplyr::filter(!is.na(time)) %>%
                 dplyr::filter(strata_name == "sex" & strata_level =="Male") %>%
                 dplyr::pull(),
               surv_cr_m  %>%
                 dplyr::filter(!is.na(time)) %>%
                 dplyr::pull()
  )

  expect_equal(surv_cr %>%
                 dplyr::filter(is.na(time)) %>%
                 dplyr::filter(strata_name == "sex" & strata_level =="Male") %>%
                 dplyr::pull(),
               surv_cr_m  %>%
                 dplyr::filter(is.na(time)) %>%
                 dplyr::pull()
  )


  # strata with only one value
  cdm2$exposure_cohort <- cdm2$exposure_cohort %>% dplyr::mutate(a = "X")
  surv <- estimateSingleEventSurvival(cdm2,
                                      targetCohortTable = "exposure_cohort",
                                      outcomeCohortTable = "cohort1",
                                      strata = list("sex", "a"),
                                      minCellCount = 1
  ) %>% asSurvivalResult()


  CDMConnector::cdmDisconnect(cdm2)
})

test_that("multiple rows per person - same observation period", {
  skip_on_cran()

  exposure_cohort <- dplyr::tibble(
    subject_id = c(1, 1, 2, 2, 3,4),
    cohort_definition_id = rep(1,6),
    cohort_start_date = c(
      as.Date("2010-01-01"),
      as.Date("2015-01-01"),
      as.Date("2010-01-01"),
      as.Date("2016-01-01"),
      as.Date("2010-01-01"),
      as.Date("2010-01-01")
    ),
    cohort_end_date = c(
      as.Date("2010-01-01"),
      as.Date("2015-01-01"),
      as.Date("2010-01-01"),
      as.Date("2016-01-01"),
      as.Date("2010-01-01"),
      as.Date("2010-01-01")
    )
  )
  # outcome during first cohort entry for id 1
  # outcome during second cohort entry for id 2
  outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1),
    subject_id = c(1, 2),
    cohort_start_date = c(
      as.Date("2012-01-10"),
      as.Date("2017-01-10")
    ),
    cohort_end_date = c(
      as.Date("2012-01-10"),
      as.Date("2017-01-10")
    ))
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2,3,4),
    person_id = c(1, 2, 3, 4),
    observation_period_start_date = c(
      as.Date("2007-03-21"),
      as.Date("2006-09-09"),
      as.Date("1980-07-20"),
      as.Date("1980-07-20")
    ),
    observation_period_end_date = c(
      as.Date("2022-09-08"),
      as.Date("2023-01-03"),
      as.Date("2023-05-20"),
      as.Date("2023-05-20")
    ),
    period_type_concept_id = c(rep(0,4))
  )
  competing_cohort <- dplyr::tibble(
    cohort_definition_id = c(1),
    subject_id = c(4),
    cohort_start_date = c(
      as.Date("2012-01-10")
    ),
    cohort_end_date = c(
      as.Date("2012-01-10")
    )
  )

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      exposure_cohort = exposure_cohort,
      cohort1 = outcome_cohort,
      cohort2 = competing_cohort
    ),
    cdmName = "mock_es"
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  expect_no_error(surv <- estimateSingleEventSurvival(cdm2,
                                                      targetCohortTable = "exposure_cohort",
                                                      outcomeCohortTable = "cohort1",
                                                      returnParticipants = TRUE
  ) %>% asSurvivalResult())

  # we have three events because subject 2 has two events
  expect_true(max(attr(surv, "summary") %>%
                    dplyr::filter(estimate_name == "events") %>%
                    dplyr::pull("estimate_value")) == 3)

  # we have five for n_start because subject 2 appears twice
  expect_true(max(attr(surv, "summary") %>%
                    dplyr::filter(estimate_name == "number_records") %>%
                    dplyr::pull("estimate_value")) == 5)

  ## competing risk
  expect_no_error(surv <- estimateCompetingRiskSurvival(cdm2,
                                                        targetCohortTable = "exposure_cohort",
                                                        outcomeCohortTable = "cohort1",
                                                        competingOutcomeCohortTable = "cohort2",
                                                        returnParticipants = TRUE
  ) %>% asSurvivalResult())


  CDMConnector::cdm_disconnect(cdm2)
})

test_that("multiple outcomes competing risk", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()

  result <- estimateCompetingRiskSurvival(
    cdm = cdm,
    targetCohortTable = "mgus_diagnosis",
    outcomeCohortTable = "progression_type",
    competingOutcomeCohortTable = "death_cohort"
  ) %>% asSurvivalResult()

  x <- result %>%
    dplyr::group_by(dplyr::across(!"estimate_value")) %>%
    dplyr::summarise(
      number_rows = dplyr::n(),
      distinct_values = dplyr::n_distinct(estimate_value),
      .groups = "drop"
    ) %>%
    dplyr::summarise(
      number_groups = dplyr::n(),
      multiple_rows = sum(number_rows>1),
      multiple_values = sum(distinct_values > 1)
    )

  expect_true(x$multiple_rows == 0)
  expect_true(x$multiple_values == 0)

  CDMConnector::cdm_disconnect(cdm)
})

test_that("empty cohort table", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()

  cdm$progression_type <- cdm$progression_type %>%
    dplyr::filter(.data$cohort_definition_id != 1) %>%
    CDMConnector::recordCohortAttrition("filter")

  expect_no_error(result <- estimateCompetingRiskSurvival(
    cdm = cdm,
    targetCohortTable = "mgus_diagnosis",
    outcomeCohortTable = "progression_type",
    competingOutcomeCohortTable = "death_cohort"
  ) %>% asSurvivalResult())

  CDMConnector::cdm_disconnect(cdm)

})

test_that("min cell count", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      outcomeCohortTable = "death_cohort",
                                      strata = list(
                                        "age" = c("age")
                                      ),
                                      eventGap = 7,
                                      minCellCount = 35) %>% asSurvivalResult()

  expect_true(nrow(attr(surv, "events") %>%
                     dplyr::filter(estimate_name == "n_risk") %>%
                     dplyr::filter(estimate_value < 35)) == 0)
  expect_true(nrow(attr(surv, "summary") %>%
                     dplyr::filter(estimate_name == "n_records") %>%
                     dplyr::filter(estimate_value < 35)) == 0)

  result <- estimateCompetingRiskSurvival(
    cdm = cdm,
    targetCohortTable = "mgus_diagnosis",
    outcomeCohortTable = "progression_type",
    competingOutcomeCohortTable = "death_cohort",
    minCellCount = 35
  ) %>% asSurvivalResult()
  expect_true(nrow(attr(result, "events") %>%
                     dplyr::filter(estimate_name == "n_risk") %>%
                     dplyr::filter(estimate_value < 35)) == 0)

  CDMConnector::cdm_disconnect(cdm)

})

test_that("minimum survival days", {
  skip_on_cran()

  exposure_cohort <- dplyr::tibble(
    subject_id = c(1, 2, 3, 4, 5),
    cohort_definition_id = c(1, 1, 1,1,1),
    cohort_start_date = c(
      as.Date("2008-01-01"),
      as.Date("2010-01-01"),
      as.Date("2000-01-01"),
      as.Date("2010-01-01"),
      as.Date("2010-01-01")
    ),
    cohort_end_date = c(
      as.Date("2020-01-31"),
      as.Date("2012-01-01"),
      as.Date("2021-06-28"),
      as.Date("2012-01-01"),
      as.Date("2012-01-01")
    ),
    sex = c("Female", "Male", "Female", "Male", "Male")
  )
  outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 2, 2, 3, 3),
    cohort_start_date = c(
      as.Date("2020-01-10"),
      as.Date("2010-01-01"),
      as.Date("2011-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    ),
    cohort_end_date = c(
      as.Date("2020-01-10"),
      as.Date("2010-02-02"),
      as.Date("2011-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    )
  )
  other_outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(1,1),
    subject_id = c(4,3),
    cohort_start_date = c(
      as.Date("2011-02-09"),
      as.Date("2000-01-01")
    ),
    cohort_end_date = c(
      as.Date("2011-02-09"),
      as.Date("2000-01-01")
    )
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3,4,5),
    person_id = c(1, 2, 3,4,5),
    observation_period_start_date = c(
      as.Date("2007-03-21"),
      as.Date("2009-09-09"),
      as.Date("1980-07-20"),
      as.Date("2009-09-09"),
      as.Date("2009-09-09")
    ),
    observation_period_end_date = c(
      as.Date("2022-09-08"),
      as.Date("2022-01-03"),
      as.Date("2023-05-20"),
      as.Date("2015-01-03"),
      as.Date("2015-01-05")
    ),
    period_type_concept_id = c(rep(0,5))
  )

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      exposure_cohort = exposure_cohort,
      cohort1 = outcome_cohort,
      cohort2 = other_outcome_cohort
    ),
    cdmName = "mock_es"
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  surv <- estimateSingleEventSurvival(cdm2,
                                      targetCohortTable = "exposure_cohort",
                                      outcomeCohortTable = "cohort1",
                                      strata = list("sex"),
                                      minCellCount = 1
  ) %>% asSurvivalResult()

  expect_true(attr(surv, "attrition") %>%
                dplyr::filter(reason == "Survival days for outcome less than 1") %>%
                dplyr::select(excluded_records) == 1)

  surv_cr <- estimateCompetingRiskSurvival(cdm2,
                                           targetCohortTable = "exposure_cohort",
                                           outcomeCohortTable = "cohort1",
                                           competingOutcomeCohortTable = "cohort2",
                                           strata = list("sex"),
                                           minCellCount = 1
  ) %>% asSurvivalResult()

  expect_true(attr(surv_cr, "attrition") %>%
                dplyr::filter(reason == "Survival days for competing outcome less than 1") %>%
                dplyr::select(excluded_records) == 1)

  CDMConnector::cdm_disconnect(cdm2)

})

test_that("outcomeWashout", {
  skip_on_cran()

  exposure_cohort <- dplyr::tibble(
    subject_id = c(1, 2, 3, 4, 5),
    cohort_definition_id = c(1, 1, 1,1,1),
    cohort_start_date = c(
      as.Date("2008-01-01"),
      as.Date("2010-01-01"),
      as.Date("2000-01-01"),
      as.Date("2010-01-01"),
      as.Date("2010-01-01")
    ),
    cohort_end_date = c(
      as.Date("2020-01-31"),
      as.Date("2012-01-01"),
      as.Date("2021-06-28"),
      as.Date("2012-01-01"),
      as.Date("2012-01-01")
    ),
    sex = c("Female", "Male", "Female", "Male", "Male")
  )
  outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 2, 2, 3, 3),
    cohort_start_date = c(
      as.Date("2020-01-10"),
      as.Date("2009-01-01"),
      as.Date("2011-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    ),
    cohort_end_date = c(
      as.Date("2020-01-10"),
      as.Date("2009-02-02"),
      as.Date("2011-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    )
  )
  other_outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(1,1),
    subject_id = c(4,3),
    cohort_start_date = c(
      as.Date("2011-02-09"),
      as.Date("2000-01-01")
    ),
    cohort_end_date = c(
      as.Date("2011-02-09"),
      as.Date("2000-01-01")
    )
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3,4,5),
    person_id = c(1, 2, 3,4,5),
    observation_period_start_date = c(
      as.Date("2007-03-21"),
      as.Date("2008-09-09"),
      as.Date("1980-07-20"),
      as.Date("2009-09-09"),
      as.Date("2009-09-09")
    ),
    observation_period_end_date = c(
      as.Date("2022-09-08"),
      as.Date("2022-01-03"),
      as.Date("2023-05-20"),
      as.Date("2015-01-03"),
      as.Date("2015-01-05")
    ),
    period_type_concept_id = c(rep(0,5))
  )

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      exposure_cohort = exposure_cohort,
      cohort1 = outcome_cohort,
      cohort2 = other_outcome_cohort
    ),
    cdmName = "mock_es"
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  surv <- estimateSingleEventSurvival(cdm2,
                                      targetCohortTable = "exposure_cohort",
                                      outcomeCohortTable = "cohort1",
                                      minCellCount = 1
  ) %>% asSurvivalResult()

  surv_w0 <- estimateSingleEventSurvival(cdm2,
                                      targetCohortTable = "exposure_cohort",
                                      outcomeCohortTable = "cohort1",
                                      outcomeWashout = 1,
                                      minCellCount = 1
  ) %>% asSurvivalResult()

  # We should have one extra event when washout is 0 because we don't censor person 2
  expect_true(attr(surv_w0, "summary") %>%
                dplyr::filter(estimate_name == "events") %>%
                dplyr::pull(estimate_value) ==
                attr(surv, "summary") %>%
                dplyr::filter(estimate_name == "events") %>%
                dplyr::pull(estimate_value) + 1)

  CDMConnector::cdm_disconnect(cdm2)
})
