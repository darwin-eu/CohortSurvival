test_that("mgus example: no Competing risk", {
  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      targetCohortId = 1,
                                      outcomeCohortTable = "death_cohort",
                                      outcomeCohortId = 1,
                                      eventGap = 7
  ) |> asSurvivalResult()
  expect_true(tibble::is_tibble(surv))
  expect_true(all(c(
    "cdm_name", "result_type",
    "target_cohort",
    "outcome", "competing_outcome",
    "variable","estimate",
    "estimate_95CI_lower",
    "estimate_95CI_upper",
    "time") %in%
      colnames(surv)))

  expect_true(surv |> dplyr::select(time) |> dplyr::distinct() |> dplyr::tally() == 425)

  expect_true(attr(surv, "events") |>
                dplyr::select(outcome) |> dplyr::pull() |> unique() == "death_cohort")
  expect_true(all(attr(surv, "events") |>
                    dplyr::pull("time") |> unique() %in% c(seq(0, 424, by = 7), 424)))

  expect_true(tibble::is_tibble(attr(surv, "summary")))

  expect_true(all(surv$outcome == "death_cohort"))
  expect_true(all(attr(surv, "event")$outcome == "death_cohort"))
  expect_true(all(attr(surv, "summary")$outcome == "death_cohort"))

  # mgus example: Competing risk
  survCR <- estimateCompetingRiskSurvival(cdm,
                             targetCohortTable = "mgus_diagnosis",
                             targetCohortId = 1,
                             outcomeCohortTable = "progression",
                             outcomeCohortId = 1,
                             competingOutcomeCohortTable = "death_cohort",
                             competingOutcomeCohortId = 1
  ) |> asSurvivalResult()

  expect_true(all(colnames(surv) %in% c(colnames(survCR), "competing_outcome")))
  expect_true(tibble::is_tibble(survCR))
  expect_true(all(survCR |>
                    dplyr::select(outcome) |>
                    dplyr::pull() |>
                    unique() %in%
                    c("death_cohort", "progression")))

  expect_true(all(compareNA(survCR |>
                              dplyr::pull("time") |>
                              unique(), c(0:424))))

  expect_true(all(attr(survCR, "events") |>
                    dplyr::select(outcome) |>
                    dplyr::pull() |>
                    unique() %in% c("progression", "death_cohort")))

  expect_true(all(attr(survCR, "events") |>
                    dplyr::pull("time") |> unique() %in% c(0:424)))

  expect_true(nrow(survCR |>
                     dplyr::filter(.data$variable == "death_cohort") |>
                     dplyr::collect())>=1)
  expect_true(nrow(survCR |>
                     dplyr::filter(.data$variable == "progression") |>
                     dplyr::collect())>=1)

  expect_true(all(c("death_cohort", "progression") %in%
                    (survCR |>
                       dplyr::pull("variable") |>
                       unique())))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("mgus example: no Competing risk, strata", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  cdm[["mgus_diagnosis"]] <- cdm[["mgus_diagnosis"]] |>
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
  ) |> asSurvivalResult()
  expect_true(tibble::is_tibble(surv))

  expect_true(all(surv |> dplyr::select(variable) |> dplyr::pull() |> unique() %in% c("death_cohort")))
  expect_true(all(surv |> dplyr::pull("time") |> unique() %in% c(0:424)))
  expect_true(all(c("age", "sex", "mspike_r") %in% colnames(surv)))
  expect_true(
    dplyr::anti_join(
      surv |> dplyr::select(c("age", "sex", "mspike_r")) |> dplyr::distinct(),
      dplyr::tibble(expand.grid(c("overall",24:96), c("overall","M", "F"), c("overall",0,1,2,3))) |>
         dplyr::rename("age" = "Var1", "sex" = "Var2", "mspike_r" = "Var3") |>
         dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.character(.x))) |>
         dplyr::filter(age == "overall" | sex == "overall" | mspike_r == "overall"),
      by = c("age", "sex", "mspike_r")
    ) |>
      nrow() == 0
  )
  expect_true(all(c("sex", "age", "mspike_r") %in% colnames(attr(surv, "events"))))
  expect_true(
    dplyr::anti_join(
      attr(surv, "events") |> dplyr::select(c("age", "sex", "mspike_r")) |> dplyr::distinct(),
      dplyr::tibble(expand.grid(c("overall",24:96), c("overall","M", "F"), c("overall",0,1,2,3))) |>
        dplyr::rename("age" = "Var1", "sex" = "Var2", "mspike_r" = "Var3") |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.character(.x))) |>
        dplyr::filter(age == "overall" | sex == "overall" | mspike_r == "overall"),
      by = c("age", "sex", "mspike_r")
    ) |>
      nrow() == 0
  )

  CDMConnector::cdmDisconnect(cdm)
})

test_that("mgus example: Competing risk, strata", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()
  cdm[["mgus_diagnosis"]] <- cdm[["mgus_diagnosis"]] |>
    dplyr::mutate(mspike_r = round(mspike, digits = 0))
  survCR <- estimateCompetingRiskSurvival(cdm,
                             targetCohortTable = "mgus_diagnosis",
                             targetCohortId = 1,
                             outcomeCohortTable = "progression",
                             outcomeCohortId = 1,
                             competingOutcomeCohortTable = "death_cohort",
                             competingOutcomeCohortId = 1,
                             strata = list(
                               c("age"),
                               c("sex"),
                               c("age", "sex"),
                               c("mspike_r")
                             )
  ) |> asSurvivalResult()

  expect_true(tibble::is_tibble(survCR))
  expect_true(all(survCR |> dplyr::select(variable) |> dplyr::pull() |> unique() %in%
                    c("death_cohort", "progression")))
  expect_true(all(survCR |> dplyr::pull("time") |> unique() %in% c(0:424)))
  expect_true(all(c("age", "sex", "mspike_r") %in% colnames(survCR)))
  expect_true(
    dplyr::anti_join(
      survCR |> dplyr::select(c("age", "sex", "mspike_r")) |> dplyr::distinct(),
      dplyr::tibble(expand.grid(c("overall",24:96), c("overall","M", "F"), c("overall",0,1,2,3))) |>
        dplyr::rename("age" = "Var1", "sex" = "Var2", "mspike_r" = "Var3") |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.character(.x))) |>
        dplyr::filter(age == "overall" | sex == "overall" | mspike_r == "overall"),
      by = c("age", "sex", "mspike_r")
    ) |>
      nrow() == 0
  )
  expect_true(all(c("age", "sex", "mspike_r") %in% colnames(attr(survCR, "events"))))
  expect_true(
    dplyr::anti_join(
      attr(survCR, "events") |> dplyr::select(c("age", "sex", "mspike_r")) |> dplyr::distinct(),
      dplyr::tibble(expand.grid(c("overall",24:96), c("overall","M", "F"), c("overall",0,1,2,3))) |>
        dplyr::rename("age" = "Var1", "sex" = "Var2", "mspike_r" = "Var3") |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.character(.x))) |>
        dplyr::filter(age == "overall" | sex == "overall" | mspike_r == "overall"),
      by = c("age", "sex", "mspike_r")
    ) |>
      nrow() == 0
  )
  # strata with only one value
  cdm$mgus_diagnosis <- cdm$mgus_diagnosis |> dplyr::mutate(a = "X")
  survCR <- estimateCompetingRiskSurvival(cdm,
                             targetCohortTable = "mgus_diagnosis",
                             targetCohortId = 1,
                             outcomeCohortTable = "progression",
                             outcomeCohortId = 1,
                             competingOutcomeCohortTable = "death_cohort",
                             competingOutcomeCohortId = 1,
                             strata = list("a")) |> asSurvivalResult()

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
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(02, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- mockCohortSurvival(
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

  # one target, one outcome
  expect_no_error(surv <- estimateSingleEventSurvival(cdm,
                                                      targetCohortTable = "exposure_cohort",
                                                      targetCohortId = 1,
                                                      outcomeCohortTable = "cohort1",
                                                      outcomeCohortId = 2
  ) |> asSurvivalResult())
  expect_equal(unique(surv$target_cohort),
               omopgenerics::settings(cdm$exposure_cohort) |>
                 dplyr::filter(cohort_definition_id == 1) |>
                 dplyr::pull("cohort_name"))
  expect_equal(unique(surv$variable),
               omopgenerics::settings(cdm$cohort1) |>
                 dplyr::filter(cohort_definition_id == 2) |>
                 dplyr::pull("cohort_name"))

  # two target, one outcome
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "exposure_cohort",
                                      targetCohortId = c(1,2),
                                      outcomeCohortTable = "cohort1",
                                      outcomeCohortId = 2
  ) |> asSurvivalResult()
  expect_equal(sort(unique(surv$target_cohort)),
               sort(omopgenerics::settings(cdm$exposure_cohort) |>
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) |>
                      dplyr::pull("cohort_name")))
  expect_equal(unique(surv$variable),
               omopgenerics::settings(cdm$cohort1) |>
                 dplyr::filter(cohort_definition_id == 2) |>
                 dplyr::pull("cohort_name"))

  # two target, two outcome
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "exposure_cohort",
                                      targetCohortId = c(1,2),
                                      outcomeCohortTable = "cohort1",
                                      outcomeCohortId = c(2,3)
  ) |> asSurvivalResult()
  expect_equal(sort(unique(surv$target_cohort)),
               sort(omopgenerics::settings(cdm$exposure_cohort) |>
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) |>
                      dplyr::pull("cohort_name")))
  expect_equal(unique(surv$variable),
               omopgenerics::settings(cdm$cohort1) |>
                 dplyr::filter(cohort_definition_id %in%  c(2,3)) |>
                 dplyr::pull("cohort_name"))

  expect_equal(sort(unique(attr(surv, "event")$target_cohort)),
               sort(omopgenerics::settings(cdm$exposure_cohort) |>
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) |>
                      dplyr::pull("cohort_name")))
  expect_equal(unique(attr(surv, "event")$variable),
               omopgenerics::settings(cdm$cohort1) |>
                 dplyr::filter(cohort_definition_id %in%  c(2,3)) |>
                 dplyr::pull("cohort_name"))

  expect_equal(sort(unique(attr(surv, "summary")$target_cohort)),
               sort(omopgenerics::settings(cdm$exposure_cohort) |>
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) |>
                      dplyr::pull("cohort_name")))
  expect_equal(unique(attr(surv, "summary")$variable),
               omopgenerics::settings(cdm$cohort1) |>
                 dplyr::filter(cohort_definition_id %in%  c(2,3)) |>
                 dplyr::pull("cohort_name"))

  # two target, two outcome - without specifying
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "exposure_cohort",
                                      outcomeCohortTable = "cohort1"
  ) |> asSurvivalResult()
  expect_equal(sort(unique(surv$target_cohort)),
               sort(omopgenerics::settings(cdm$exposure_cohort) |>
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) |>
                      dplyr::pull("cohort_name")))
  expect_equal(unique(surv$variable),
               omopgenerics::settings(cdm$cohort1) |>
                 dplyr::filter(cohort_definition_id %in%  c(2,3)) |>
                 dplyr::pull("cohort_name"))

  CDMConnector::cdmDisconnect(cdm)

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
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(02, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- mockCohortSurvival(
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

  # one target, one outcome
  expect_no_error(surv <- estimateCompetingRiskSurvival(cdm,
                                                        targetCohortTable = "exposure_cohort",
                                                        targetCohortId = 1,
                                                        outcomeCohortTable = "cohort1",
                                                        outcomeCohortId = 2,
                                                        competingOutcomeCohortTable  = "cohort2",
                                                        competingOutcomeCohortId = 4
  ) |> asSurvivalResult())
  expect_equal(unique(surv$target_cohort),
               omopgenerics::settings(cdm$exposure_cohort) |>
                 dplyr::filter(cohort_definition_id == 1) |>
                 dplyr::pull("cohort_name"))
  expect_equal(sort(unique(surv$variable)),
               sort(c(omopgenerics::settings(cdm$cohort1) |>
                        dplyr::filter(cohort_definition_id == 2) |>
                        dplyr::pull("cohort_name"),
                      omopgenerics::settings(cdm$cohort2) |>
                        dplyr::filter(cohort_definition_id == 4) |>
                        dplyr::pull("cohort_name"))))

  # two target, one outcome, one competing risk
  expect_no_error(surv <- estimateCompetingRiskSurvival(cdm,
                                                        targetCohortTable = "exposure_cohort",
                                                        targetCohortId = c(1,2),
                                                        outcomeCohortTable = "cohort1",
                                                        outcomeCohortId = 2,
                                                        competingOutcomeCohortTable  = "cohort2",
                                                        competingOutcomeCohortId = 4
  ) |> asSurvivalResult())
  expect_equal(sort(unique(surv$target_cohort)),
               sort(omopgenerics::settings(cdm$exposure_cohort) |>
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) |>
                      dplyr::pull("cohort_name")))
  expect_equal(sort(unique(surv$variable)),
               sort(c(omopgenerics::settings(cdm$cohort1) |>
                        dplyr::filter(cohort_definition_id == 2) |>
                        dplyr::pull("cohort_name"),
                      omopgenerics::settings(cdm$cohort2) |>
                        dplyr::filter(cohort_definition_id == 4) |>
                        dplyr::pull("cohort_name"))))

  # two target, two outcome, one competing risk
  surv <- estimateCompetingRiskSurvival(cdm,
                                        targetCohortTable = "exposure_cohort",
                                        targetCohortId = c(1,2),
                                        outcomeCohortTable = "cohort1",
                                        outcomeCohortId = c(2,3),
                                        competingOutcomeCohortTable  = "cohort2",
                                        competingOutcomeCohortId = 4
  ) |> asSurvivalResult()
  expect_equal(sort(unique(surv$target_cohort)),
               sort(omopgenerics::settings(cdm$exposure_cohort) |>
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) |>
                      dplyr::pull("cohort_name")))
  expect_equal(sort(unique(surv$variable)),
               sort(c(omopgenerics::settings(cdm$cohort1) |>
                        dplyr::filter(cohort_definition_id %in%  c(2,3)) |>
                        dplyr::pull("cohort_name"),
                      omopgenerics::settings(cdm$cohort2) |>
                        dplyr::filter(cohort_definition_id == 4) |>
                        dplyr::pull("cohort_name"))))

  # two target, two outcome, two competing risk
  surv <- estimateCompetingRiskSurvival(cdm,
                                        targetCohortTable = "exposure_cohort",
                                        targetCohortId = c(1,2),
                                        outcomeCohortTable = "cohort1",
                                        outcomeCohortId = c(2,3),
                                        competingOutcomeCohortTable  = "cohort2",
                                        competingOutcomeCohortId = c(4,5)
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()
  expect_equal(sort(unique(surv$target_cohort)),
               sort(omopgenerics::settings(cdm$exposure_cohort) |>
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) |>
                      dplyr::pull("cohort_name")))
  expect_equal(sort(unique(surv$variable)),
               sort(c(omopgenerics::settings(cdm$cohort1) |>
                        dplyr::filter(cohort_definition_id %in%  c(2,3)) |>
                        dplyr::pull("cohort_name"),
                      omopgenerics::settings(cdm$cohort2) |>
                        dplyr::filter(cohort_definition_id %in% c(4,5)) |>
                        dplyr::pull("cohort_name"))))

  #  two target, two outcome, two competing risk - without specifying
  surv <- estimateCompetingRiskSurvival(cdm,
                                        targetCohortTable = "exposure_cohort",
                                        outcomeCohortTable = "cohort1",
                                        competingOutcomeCohortTable  = "cohort2"
  ) |> asSurvivalResult()
  expect_equal(sort(unique(surv$target_cohort)),
               sort(omopgenerics::settings(cdm$exposure_cohort) |>
                      dplyr::filter(cohort_definition_id %in%  c(1,2)) |>
                      dplyr::pull("cohort_name")))
  expect_equal(sort(unique(surv$variable)),
               sort(c(omopgenerics::settings(cdm$cohort1) |>
                        dplyr::filter(cohort_definition_id %in%  c(2,3)) |>
                        dplyr::pull("cohort_name"),
                      omopgenerics::settings(cdm$cohort2) |>
                        dplyr::filter(cohort_definition_id %in% c(4,5)) |>
                        dplyr::pull("cohort_name"))))

  CDMConnector::cdmDisconnect(cdm)

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
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(02, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- mockCohortSurvival(
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

  # one target, one outcome
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "exposure_cohort",
                                      targetCohortId = 1,
                                      outcomeCohortTable = "cohort1",
                                      outcomeCohortId = 2
  ) |> asSurvivalResult()

  surv_pair <- estimateSingleEventSurvival(cdm,
                                           targetCohortTable = "exposure_cohort",
                                           targetCohortId = 1,
                                           outcomeCohortTable = "cohort1",
                                           outcomeCohortId = 2,
                                           estimateGap = 2
  ) |> asSurvivalResult()

  surv2 <- surv |>
    dplyr::filter(time %in% seq(0, 1235, by = 2))

  expect_true(all.equal(surv_pair, surv2, check.attributes = FALSE))

  # two targets, two outcomes, competing risk event
  survCR <- estimateCompetingRiskSurvival(cdm,
                                          targetCohortTable = "exposure_cohort",
                                          targetCohortId = c(1,2),
                                          outcomeCohortTable = "cohort1",
                                          outcomeCohortId = c(2,3),
                                          competingOutcomeCohortTable  = "cohort2",
                                          competingOutcomeCohortId = 4
  ) |> asSurvivalResult()

  survCR_time <- estimateCompetingRiskSurvival(cdm,
                                               targetCohortTable = "exposure_cohort",
                                               targetCohortId = c(1,2),
                                               outcomeCohortTable = "cohort1",
                                               outcomeCohortId = c(2,3),
                                               competingOutcomeCohortTable  = "cohort2",
                                               competingOutcomeCohortId = 4,
                                               estimateGap = 2
  ) |> asSurvivalResult()

  survCR <- survCR |>
    dplyr::filter(time %in% seq(0,1235,by = 2))

  expect_true(all.equal(survCR, survCR_time, check.attributes = FALSE))

  CDMConnector::cdmDisconnect(cdm)

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
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(02, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- mockCohortSurvival(
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

  # No competing events
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "exposure_cohort",
                                      outcomeCohortTable = "cohort1"
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  expect_true(all(surv |>
                    dplyr::pull("time") |>
                    unique() %in% c(0:31)))
  expect_true(all(attr(surv, "events") |>
                    dplyr::select(n_risk) |>
                    dplyr::pull() ==
                    c(3, 1, 1)))
  expect_true(all(surv |>
                    dplyr::select(estimate) |>
                    dplyr::pull() - c(rep(1, 7), rep(0.667, 3), rep(0.333, 21), 0) < c(0.01)))
  expect_true(all(surv |>
                    dplyr::filter(result_type == "Cumulative failure probability") |>
                    dplyr::select(estimate) |> dplyr::pull() - c(rep(0, 6), rep(0.333, 3), rep(0.667, 22), 1) < c(0.01)))

  expect_true(all(attr(surv, "events") |> dplyr::select(variable) |> dplyr::pull() %in% c("cohort_1")))
  expect_true(all(attr(surv, "events") |>
                    dplyr::filter(time == 0) |>
                    dplyr::pull("n_risk") %in%  c(3)))

  CDMConnector::cdmDisconnect(cdm)

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
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(02, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- mockCohortSurvival(
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

  surv2 <- estimateCompetingRiskSurvival(cdm,
                                         targetCohortTable = "exposure_cohort",
                                         outcomeCohortTable = "cohort1",
                                         competingOutcomeCohortTable = "cohort2",
                                         competingOutcomeCohortId = 1
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  expect_true(all(surv2 |>
                    dplyr::pull("time") |>
                    unique() %in% c(0:981)))
  expect_true(all(attr(surv2, "events") |>
                    dplyr::filter(variable == "cohort_1") |>
                    dplyr::select(n_risk) |>
                    dplyr::pull() ==
                    c(3, 2, rep(1, 32))))

  expect_true(all(surv2 |> dplyr::select(variable) |> dplyr::pull() %in% c("cohort_1", "cohort_1_competing_outcome")))
  expect_true(all(attr(surv2, "events") |>
                    dplyr::pull("time") %in%  c(seq(0,981, by = 30), 981)))

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

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(02, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- mockCohortSurvival(
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

  surv3 <- estimateSingleEventSurvival(cdm, "exposure_cohort",
                                       outcomeCohortTable = "cohort1",
                                       censorOnCohortExit = TRUE
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  expect_true(all(surv3 |>
                    dplyr::pull("time") |>
                    unique() %in% c(0:31)))
  expect_true(all(attr(surv3, "events") |>
                    dplyr::select(n_risk) |> dplyr::pull() == c(3, 2, 1)))
  expect_true(all(surv3  |>
                    dplyr::select(estimate) |> dplyr::pull() - c(rep(1, 6), rep(0.667, 25), 0) < c(0.01)))

  CDMConnector::cdmDisconnect(cdm)

  cdm <- mockCohortSurvival(
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

  # Censor by follow up days
  surv4 <- estimateSingleEventSurvival(cdm, "exposure_cohort",
                                       outcomeCohortTable = "cohort1",
                                       followUpDays = 10
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  expect_true(all(surv4 |>
                    dplyr::pull("time") |>
                    unique() %in% c(0:10)))
  expect_true(all(attr(surv4, "events") |> dplyr::select(n_risk) |> dplyr::pull() == c(3, 2)))
  expect_true(all(surv4 |>
                    dplyr::select(estimate) |> dplyr::pull() - c(rep(1, 6), rep(0.667, 5)) < c(0.01)))
  expect_true(all(attr(surv4, "events") |>
                    dplyr::pull("time") %in%  c(0:10)))

  # if followUpDays larger than last of the times, follow until then
  surv4b <- estimateSingleEventSurvival(cdm, "exposure_cohort",
                                        outcomeCohortTable = "cohort1",
                                        followUpDays = 40
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  expect_true(all(attr(surv4, "events") |>
                    dplyr::pull("time") %in%  c(0:40)))

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

  cdm <- mockCohortSurvival(
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

  # Strata
  surv5 <- estimateSingleEventSurvival(cdm, "exposure_cohort",
                                       outcomeCohortTable = "cohort1",
                                       strata = list(
                                         c("age_group"),
                                         c("sex"),
                                         c("age_group", "sex"),
                                         c("blood_type")
                                       )
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  expect_true(all(surv5 |>
                    dplyr::filter(age_group == "overall",
                                  sex == "overall",
                                  blood_type == "overall") |>
                    dplyr::pull("time") |>
                    unique() %in% c(0:31)))
  expect_true(all(attr(surv5, "events") |> dplyr::filter(age_group == "overall",
                                                          sex == "overall",
                                                          blood_type == "overall") |>
                    dplyr::select(n_risk) |> dplyr::pull() == c(3, 1, 1)))
  expect_true(all(surv5 |> dplyr::filter(age_group == "overall",
                                          sex == "overall",
                                          blood_type == "overall") |>
                    dplyr::select(estimate) |> dplyr::pull() - c(rep(1, 6), rep(0.667, 3), rep(0.333, 22), 0) < c(0.01)))

  expect_true(all(surv5 |> dplyr::filter(age_group == "20;29",
                                          sex == "Female",
                                          blood_type == "overall") |>
                    dplyr::select("time") |> dplyr::pull() %in% c(0:31)))
  expect_true(all(compareNA(attr(surv5, "events") |> dplyr::filter(age_group == "20;29",
                                                                    sex == "Female",
                                                                    blood_type == "overall") |>
                              dplyr::select(n_risk) |>
                              dplyr::pull(), c(1,1))))
  expect_true(all(surv5 |> dplyr::filter(age_group == "20;29",
                                          sex == "Female",
                                          blood_type == "overall") |>
                    dplyr::select(estimate) |> dplyr::pull() - c(rep(1, 9), 0) < c(0.01)))

  expect_true(all(attr(surv5, "events") |>
                    dplyr::filter(age_group == "20;29",
                                  sex == "Female",
                                  blood_type == "overall") |>
                    dplyr::pull("time") %in%  c(0,9)))

  expect_true(all(surv5 |> dplyr::filter(age_group == "overall",
                                          sex == "overall",
                                          blood_type == "B") |>
                    dplyr::select(time) |> dplyr::pull() == c(0:31)))
  expect_true(all(attr(surv5,"events") |> dplyr::filter(age_group == "overall",
                                          sex == "overall",
                                          blood_type == "B") |> dplyr::select(n_risk) |>
                    dplyr::pull() == c(2,1,1)))
  expect_true(all(surv5 |> dplyr::filter(age_group == "overall",
                                          sex == "overall",
                                          blood_type == "B") |>
                    dplyr::select(estimate) |> dplyr::pull() - c(rep(1, 6), rep(0.5, 25), 0) < c(0.01)))

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

  cdm <- mockCohortSurvival(
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

  # Washout for outcome
  surv6 <- estimateSingleEventSurvival(cdm, "exposure_cohort",
                                       outcomeCohortTable = "cohort1"
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  expect_true(all(surv6 |>
                    dplyr::pull("time") |>
                    unique() %in% c(0:31)))
  expect_true(all(attr(surv6, "events") |> dplyr::select(n_risk) |> dplyr::pull() == c(2, 1, 1)))
  expect_true(all(surv6 |>
                    dplyr::select(estimate) |> dplyr::pull() - c(rep(1, 6), rep(0.5, 25), 0) < c(0.01)))

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

  cdm <- mockCohortSurvival(
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

  # Censor on date
  surv7 <- estimateSingleEventSurvival(cdm,
                                       targetCohortTable = "exposure_cohort",
                                       outcomeCohortTable = "cohort1",
                                       censorOnDate = as.Date("2020-05-04")
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  expect_true(all(surv7 |>
                    dplyr::pull("time") |>
                    unique() %in% c(0:9)))
  expect_true(all(attr(surv7, "events") |> dplyr::select(n_risk) |> dplyr::pull() == c(3, 1)))
  expect_true(all(surv7 |>
                    dplyr::select(estimate)|> dplyr::pull() - c(rep(1, 6), 0.5, 0.5, 0.5, 0) < c(0.01)))

  CDMConnector::cdmDisconnect(cdm)
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
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(02, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- mockCohortSurvival(
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

  surv8 <-
    estimateSingleEventSurvival(
      cdm = cdm,
      targetCohortTable = "cohort1",
      targetCohortId = 1,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 1
    ) |> asSurvivalResult()
  expect_true(all(surv8 |>
                    dplyr::pull("time") |> unique() %in% c(0:9)))
  expect_true(all(attr(surv8, "events") |> dplyr::select(n_risk) |> dplyr::pull() == c(2, 1)))
  expect_true(all(surv8 |>
                    dplyr::select(estimate) |> dplyr::pull() - c(1, rep(0.5, 8), 0) < c(0.01)))

  expect_true(all(attr(surv8, "events") |> dplyr::filter(!is.na(n_risk)) |>
                    dplyr::select(time) == c(0,9)))
  expect_true(all(attr(surv8, "events") |>
                    dplyr::filter(time == 0) |> dplyr::select(n_risk) |>
                    dplyr::pull() == c(2, 2, 2, 2)))

  surv9 <-
    estimateSingleEventSurvival(
      cdm = cdm,
      targetCohortTable = "cohort1",
      targetCohortId = 2,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 1
    ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  expect_true(all(surv9 |> dplyr::pull("time") |> unique() %in% c(0:8)))
  expect_true(all(surv9 |>
                    dplyr::select(estimate) |> dplyr::pull() - c(rep(1, 8), 0) < c(0.01)))

  expect_true(all(attr(surv9, "events") |>
                    dplyr::pull("time") %in%  c(0:8)))
  expect_true(all(attr(surv9, "events") |>
                    dplyr::filter(time == 0) |> dplyr::select(n_risk) |>
                    dplyr::pull() %in%  c(1)))

  CDMConnector::cdmDisconnect(cdm)
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
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, outcomeWashout = -1))
  expect_error(estimateSurvival(cdm, targetCohortTable = "mgus_diagnosis", outcomeCohortTable = "progression", outcomeCohortId = 1, competingOutcomeWashout = "1"))

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
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(02, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- mockCohortSurvival(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      cohort1 = cohort
    ),
    cdmName = "mock_es"
  )


  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "cohort1",
                                      targetCohortId = 1,
                                      outcomeCohortTable = "cohort1",
                                      outcomeCohortId = 1,
                                      outcomeDateVariable = "cohort_end_date",
                                      eventGap = 7
  ) |> asSurvivalResult()
  expect_true(max(attr(surv, "events") |> dplyr::select(n_risk) |> dplyr::pull(), na.rm = TRUE) == 3)

  CDMConnector::cdmDisconnect(cdm)
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
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(02, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- mockCohortSurvival(
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

  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "exposure_cohort",
                                      outcomeCohortTable = "cohort1",
                                      strata = list("sex")
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()
  surv_cr <- estimateCompetingRiskSurvival(cdm,
                                           targetCohortTable = "exposure_cohort",
                                           outcomeCohortTable = "cohort1",
                                           competingOutcomeCohortTable = "cohort2",
                                           strata = list("sex")
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  # only males
  cdm[["exposure_cohort_m"]] <- cdm$exposure_cohort |>
    dplyr::filter(sex =="Male") |>
    dplyr::compute(temporary = FALSE, name = "exposure_cohort_m")
  surv_m <- estimateSingleEventSurvival(cdm,
                                        targetCohortTable = "exposure_cohort_m",
                                        outcomeCohortTable = "cohort1"
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()
  surv_cr_m <- estimateCompetingRiskSurvival(cdm,
                                             targetCohortTable = "exposure_cohort_m",
                                             outcomeCohortTable = "cohort1",
                                             competingOutcomeCohortTable = "cohort2"
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  # overall result should now be the same as the strata of males before filtering
  expect_equal(surv |>
                 dplyr::filter(!is.na(time)) |>
                 dplyr::filter(sex =="Male") |>
                 dplyr::pull(),
               surv_m  |>
                 dplyr::filter(!is.na(time)) |>
                 dplyr::pull()
  )
  expect_equal(surv |>
                 dplyr::filter(is.na(time)) |>
                 dplyr::filter(sex =="Male") |>
                 dplyr::pull(),
               surv_m  |>
                 dplyr::filter(is.na(time)) |>
                 dplyr::pull()
  )
  expect_equal(
    surv |>
      dplyr::filter(sex =="Male") |>
      dplyr::select("estimate") |>
      dplyr::pull(),
    surv_m |>
      dplyr::select("estimate") |>
      dplyr::pull()
  )

  expect_equal(surv_cr |>
                 dplyr::filter(!is.na(time)) |>
                 dplyr::filter(sex =="Male") |>
                 dplyr::pull(),
               surv_cr_m  |>
                 dplyr::filter(!is.na(time)) |>
                 dplyr::pull()
  )

  expect_equal(surv_cr |>
                 dplyr::filter(is.na(time)) |>
                 dplyr::filter(sex =="Male") |>
                 dplyr::pull(),
               surv_cr_m  |>
                 dplyr::filter(is.na(time)) |>
                 dplyr::pull()
  )

  # strata with only one value
  cdm$exposure_cohort <- cdm$exposure_cohort |> dplyr::mutate(a = "X")
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "exposure_cohort",
                                      outcomeCohortTable = "cohort1",
                                      strata = list("sex", "a")
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  CDMConnector::cdmDisconnect(cdm)
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
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(02, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- mockCohortSurvival(
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

  expect_no_error(surv <- estimateSingleEventSurvival(cdm,
                                                      targetCohortTable = "exposure_cohort",
                                                      outcomeCohortTable = "cohort1"
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult())

  # we have three events because subject 2 has two events
  expect_true(max(attr(surv, "summary") |>
                    dplyr::pull("n_events")) == 3)

  # we have five for n_start because subject 2 appears twice
  expect_true(max(attr(surv, "summary") |>
                    dplyr::pull("number_records")) == 5)

  ## competing risk
  expect_no_error(surv <- estimateCompetingRiskSurvival(cdm,
                                                        targetCohortTable = "exposure_cohort",
                                                        outcomeCohortTable = "cohort1",
                                                        competingOutcomeCohortTable = "cohort2"
  ) |> asSurvivalResult())


  CDMConnector::cdmDisconnect(cdm)
})

test_that("multiple outcomes competing risk", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()

  result <- estimateCompetingRiskSurvival(
    cdm = cdm,
    targetCohortTable = "mgus_diagnosis",
    outcomeCohortTable = "progression_type",
    competingOutcomeCohortTable = "death_cohort"
  ) |> asSurvivalResult()

  x <- result |>
    dplyr::group_by(dplyr::across(!"estimate")) |>
    dplyr::summarise(
      number_rows = dplyr::n(),
      distinct_values = dplyr::n_distinct(estimate),
      .groups = "drop"
    ) |>
    dplyr::summarise(
      number_groups = dplyr::n(),
      multiple_rows = sum(number_rows>1),
      multiple_values = sum(distinct_values > 1)
    )

  expect_true(x$multiple_rows == 0)
  expect_true(x$multiple_values == 0)

  CDMConnector::cdmDisconnect(cdm)
})

test_that("empty cohort table", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()

  cdm$progression_type <- cdm$progression_type |>
    dplyr::filter(.data$cohort_definition_id != 1) |>
    CDMConnector::recordCohortAttrition("filter")

  expect_warning(result <- estimateCompetingRiskSurvival(
    cdm = cdm,
    targetCohortTable = "mgus_diagnosis",
    outcomeCohortTable = "progression_type",
    competingOutcomeCohortTable = "death_cohort"
  ) |> asSurvivalResult())

  CDMConnector::cdmDisconnect(cdm)

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
                                      eventGap = 7)  |>
    omopgenerics::suppress(minCellCount = 35) |>
    asSurvivalResult()

  expect_true(nrow(attr(surv, "events") |>
                     dplyr::filter(n_risk < 35)) == 0)
  expect_true(nrow(attr(surv, "summary") |>
                     dplyr::filter(number_records < 35)) == 0)

  result <- estimateCompetingRiskSurvival(
    cdm = cdm,
    targetCohortTable = "mgus_diagnosis",
    outcomeCohortTable = "progression_type",
    competingOutcomeCohortTable = "death_cohort"
  )  |>
    omopgenerics::suppress(minCellCount = 35) |>
    asSurvivalResult()
  expect_true(nrow(attr(result, "events") |>
                     dplyr::filter(n_risk < 35)) == 0)

  CDMConnector::cdmDisconnect(cdm)

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

  competing_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1),
    subject_id = c(4, 3),
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
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(02, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- mockCohortSurvival(
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

  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "exposure_cohort",
                                      outcomeCohortTable = "cohort1",
                                      strata = list("sex")
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  expect_true(attr(surv, "attrition") |>
                dplyr::filter(reason == "Survival days for outcome less than 1",
                              variable_name == "excluded_records") |>
                dplyr::pull(count) == 1)

  surv_cr <- estimateCompetingRiskSurvival(cdm,
                                           targetCohortTable = "exposure_cohort",
                                           outcomeCohortTable = "cohort1",
                                           competingOutcomeCohortTable = "cohort2",
                                           strata = list("sex")
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  expect_true(attr(surv_cr, "attrition") |>
                dplyr::filter(reason == "Survival days for outcome less than 1",
                              variable_name == "excluded_records") |>
                dplyr::pull(count) == 1)

  CDMConnector::cdmDisconnect(cdm)

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
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(02, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- mockCohortSurvival(
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

  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "exposure_cohort",
                                      outcomeCohortTable = "cohort1"
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  surv_w0 <- estimateSingleEventSurvival(cdm,
                                         targetCohortTable = "exposure_cohort",
                                         outcomeCohortTable = "cohort1",
                                         outcomeWashout = 1
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  # We should have one extra event when washout is 0 because we don't censor person 2
  expect_true(attr(surv_w0, "summary") |>
                dplyr::pull(n_events) ==
                attr(surv, "summary") |>
                dplyr::pull(n_events) + 1)

  # Now let's make the previous outcome at exactly index date, with washout 0
  # Person 2 should be treated as having an outcome on day 0
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

  cdm <- mockCohortSurvival(
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

  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "exposure_cohort",
                                      outcomeCohortTable = "cohort1",
                                      outcomeWashout = 0,
                                      minimumSurvivalDays = 0
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  expect_true(attr(surv, "summary") |>
                dplyr::pull("n_events") == 3)
  expect_true(attr(surv, "summary") |>
                dplyr::pull("number_records") == 5)

  # This is not true if the event happens a day before
  outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 2, 2, 3, 3),
    cohort_start_date = c(
      as.Date("2020-01-10"),
      as.Date("2009-12-31"),
      as.Date("2009-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    ),
    cohort_end_date = c(
      as.Date("2020-01-10"),
      as.Date("2010-02-02"),
      as.Date("2009-02-09"),
      as.Date("2020-06-01"),
      as.Date("2020-06-03")
    )
  )

  cdm <- mockCohortSurvival(
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

  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "exposure_cohort",
                                      outcomeCohortTable = "cohort1",
                                      outcomeWashout = 0,
                                      minimumSurvivalDays = 0
  ) |>
    omopgenerics::suppress(minCellCount = 1) |>
    asSurvivalResult()

  expect_true(attr(surv, "summary") |>
                dplyr::pull("n_events") == 2)
  expect_true(attr(surv, "summary") |>
                dplyr::pull("number_records") == 5)

  CDMConnector::cdmDisconnect(cdm)
})

test_that("restrictedMeanFollowUp", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()
  cdm[["mgus_diagnosis"]] <- cdm[["mgus_diagnosis"]] |>
    dplyr::mutate(mspike_r = round(mspike, digits = 0))
  survCR <- estimateCompetingRiskSurvival(cdm,
                                          targetCohortTable = "mgus_diagnosis",
                                          targetCohortId = 1,
                                          outcomeCohortTable = "progression",
                                          outcomeCohortId = 1,
                                          competingOutcomeCohortTable = "death_cohort",
                                          competingOutcomeCohortId = 1
  )
  survCR_rmean <- estimateCompetingRiskSurvival(cdm,
                                                targetCohortTable = "mgus_diagnosis",
                                                targetCohortId = 1,
                                                outcomeCohortTable = "progression",
                                                outcomeCohortId = 1,
                                                competingOutcomeCohortTable = "death_cohort",
                                                competingOutcomeCohortId = 1,
                                                restrictedMeanFollowUp = 100
  )
  tsurv <- tableSurvival(survCR, type = "tibble", .options = list(includeHeaderKey = FALSE))
  tsurvrmean <- tableSurvival(survCR_rmean, type = "tibble", .options = list(includeHeaderKey = FALSE))

  expect_true(all.equal(tsurv |> dplyr::select(- dplyr::contains("Restricted mean survival (95% CI)")),
                        tsurvrmean |> dplyr::select(- dplyr::contains("Restricted mean survival (95% CI)"))))
  expect_true(all(tsurv |> dplyr::pull(dplyr::contains("Restricted mean survival (95% CI)")) == c("35.00 (28.00, 42.00)", "260.00 (250.00, 269.00)")))
  expect_true(all(tsurvrmean |> dplyr::pull(dplyr::contains("Restricted mean survival (95% CI)")) == c("3.00 (2.00, 3.00)", "28.00 (26.00, 29.00)")))

  # too big a number produces NA
  survCR_rmean_big <- estimateCompetingRiskSurvival(cdm,
                                                targetCohortTable = "mgus_diagnosis",
                                                targetCohortId = 1,
                                                outcomeCohortTable = "progression",
                                                outcomeCohortId = 1,
                                                competingOutcomeCohortTable = "death_cohort",
                                                competingOutcomeCohortId = 1,
                                                restrictedMeanFollowUp = 500
  )

  tsurvrmeanbig <- tableSurvival(survCR_rmean_big, type = "tibble", .options = list(includeHeaderKey = FALSE))
  expect_true(all(compareNA(tsurvrmeanbig |> dplyr::pull(dplyr::contains("Restricted mean survival")), c("\U2013","\U2013"))))

  # different follow ups for different strata
  survCR_rmean_big_strata <- estimateCompetingRiskSurvival(cdm,
                                                    targetCohortTable = "mgus_diagnosis",
                                                    targetCohortId = 1,
                                                    outcomeCohortTable = "progression",
                                                    outcomeCohortId = 1,
                                                    competingOutcomeCohortTable = "death_cohort",
                                                    competingOutcomeCohortId = 1,
                                                    restrictedMeanFollowUp = 500
  )

  tsurvrmeanbig <- tableSurvival(survCR_rmean_big, type = "tibble", .options = list(includeHeaderKey = FALSE))
  expect_true(all(compareNA(tsurvrmeanbig |> dplyr::pull(dplyr::contains("Restricted mean survival")), c("\U2013", "\U2013"))))

  # different follow ups for different strata
  survCR_rmean_big_strata <- estimateCompetingRiskSurvival(cdm,
                                                           targetCohortTable = "mgus_diagnosis",
                                                           targetCohortId = 1,
                                                           outcomeCohortTable = "progression",
                                                           outcomeCohortId = 1,
                                                           competingOutcomeCohortTable = "death_cohort",
                                                           competingOutcomeCohortId = 1,
                                                           strata = list("sex"),
                                                           restrictedMeanFollowUp = 400
  )

  tsurvrmeanbigs <- tableSurvival(survCR_rmean_big_strata, type = "tibble", .options = list(includeHeaderKey = FALSE)) |>
    dplyr::arrange("Sex", "Outcome name")
  expect_true(all(compareNA(tsurvrmeanbigs |> dplyr::pull(dplyr::contains("Restricted mean survival (95% CI)")), c("31.00 (26.00, 37.00)","241.00 (232.00, 250.00)","\U2013","27.00 (20.00, 34.00)","\U2013","252.00 (241.00, 264.00)"))))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("mgus example: empty outcome tables or cohorts", {
    cdm <- mockMGUS2cdm()
    cdm$death_c <- cdm$death_cohort |>
      dplyr::filter(cohort_definition_id == 2) |>
      dplyr::compute(name = "death_c")
    attr(cdm$death_c, "cohort_set") <- dplyr::tibble(
      cohort_definition_id = 1,
      cohort_name = "death_c"
    )
    attr(cdm$death_c, "cohort_attrition") <- omopgenerics:::defaultCohortAttrition(cdm$death_c, attr(cdm$death_c, "cohort_set"))
    attr(cdm$death_c, "tbl_name") <- "death_c"

    # Whole empty table throws warning for outcome
    expect_warning(estimateSingleEventSurvival(cdm, targetCohortTable = "mgus_diagnosis",
                                             outcomeCohortTable = "death_c"))

    # and warning for target
    expect_warning(estimateSingleEventSurvival(cdm, targetCohortTable = "death_c",
                                             outcomeCohortTable = "mgus_diagnosis"))

    # Some empty cohortIds are just not calculated, for both primary and competing outcomes
    attr(cdm$death_cohort, "cohort_set") <- dplyr::tibble(
      cohort_definition_id = c(1,3),
      cohort_name = c("death_cohort", "death_test_empty")
    )
    attr(cdm$death_cohort, "cohort_attrition") <- omopgenerics:::defaultCohortAttrition(cdm$death_cohort, attr(cdm$death_cohort, "cohort_set"))
    attr(cdm$progression, "cohort_set") <- dplyr::tibble(
      cohort_definition_id = c(1,2),
      cohort_name = c("progression", "progression_fake_empty")
    )
    attr(cdm$progression, "cohort_attrition") <- omopgenerics:::defaultCohortAttrition(cdm$progression, attr(cdm$progression, "cohort_set"))

    expect_warning(emptyResultBis <- estimateSingleEventSurvival(cdm, targetCohortTable = "mgus_diagnosis",
                                                  outcomeCohortTable = "death_cohort",
                                                  outcomeCohortId = c(1,3)))
    expect_true(all(emptyResultBis |>
                  dplyr::filter(variable_level == "death_test_empty") |>
                    omopgenerics::filterSettings(result_type == "survival_estimates") |>
                  dplyr::pull("estimate_value") == c(1)))

    suppressWarnings(expect_warning(emptyResultBisBis <- estimateCompetingRiskSurvival(cdm, targetCohortTable = "mgus_diagnosis",
                                                       outcomeCohortTable = "progression",
                                                       outcomeCohortId = c(1,2),
                                                       competingOutcomeCohortTable = "death_cohort",
                                                       competingOutcomeCohortId = c(1,3))))

    expect_true(emptyResultBisBis |>
                  dplyr::select(variable_level) |>
                  dplyr::distinct() |>
                  dplyr::tally() |>
                  dplyr::pull() == 2)

    PatientProfiles::mockDisconnect(cdm)
  })

test_that("n_censor", {
    skip_on_cran()
    cdm <- mockMGUS2cdm()

    surv <- estimateSingleEventSurvival(cdm,
                                            targetCohortTable = "mgus_diagnosis",
                                            targetCohortId = 1,
                                            outcomeCohortTable = "death_cohort",
                                            outcomeCohortId = 1
    )

    eventstable <- attr(surv |> asSurvivalResult(), "events")

    expect_true(all(eventstable |>
                  dplyr::arrange(time) |>
                  dplyr::pull(n_censor) == c(0,3,27,79,74,54,54,58,33,18,10,6,3,1,1,0)))

    CDMConnector::cdmDisconnect(cdm)
  })

test_that("no outcomes among cohort", {
    cdm <- mockMGUS2cdm()
    cdm$death_cohort <- cdm$death_cohort |>
      dplyr::filter(subject_id == 1)
    cdm$mgus_diagnosis <- cdm$mgus_diagnosis |>
      dplyr::filter(subject_id != 1)

    expect_no_error(surv <- estimateSingleEventSurvival(cdm,
                                                        targetCohortTable = "mgus_diagnosis",
                                                        outcomeCohortTable = "death_cohort"
    ))

    expect_true(surv |>
                  tableSurvival(type = "tibble") |>
                  dplyr::pull(`[header_name]Estimate name\n[header_level]Number events`) == 0)

    # empty death table
    cdm$death_cohort <- cdm$death_cohort |>
      dplyr::filter(subject_id == 2)

    expect_warning(surv <- estimateSingleEventSurvival(cdm,
                                        targetCohortTable = "mgus_diagnosis",
                                        outcomeCohortTable = "death_cohort"
    ))

    expect_true(surv |>
                  tableSurvival(type = "tibble") |>
                  dplyr::pull(`[header_name]Estimate name\n[header_level]Number events`) == 0)

    cdm <- bind(cdm$progression, cdm$death_cohort, name = "outcome_cohorts")

    expect_warning(surv <- estimateSingleEventSurvival(cdm,
                                        targetCohortTable = "mgus_diagnosis",
                                        outcomeCohortTable = "outcome_cohorts"
    ))

    expect_true(all(surv |>
                  tableSurvival(type = "tibble") |>
                  dplyr::arrange("Outcome name") |>
                  dplyr::pull(`[header_name]Estimate name\n[header_level]Number events`) == c(115,0)))

    CDMConnector::cdmDisconnect(cdm)
  })

test_that("tables from cdm do not change after estimation", {
  cdm <- mockMGUS2cdm()
  old_cdm <- cdm

  surv <- estimateSingleEventSurvival(cdm, "mgus_diagnosis", "death_cohort")
  expect_true(all.equal(cdm$mgus_diagnosis, old_cdm$mgus_diagnosis))
  expect_true(all.equal(cdm$death_cohort, old_cdm$death_cohort))
  expect_true(all.equal(attributes(cdm$mgus_diagnosis), attributes(old_cdm$mgus_diagnosis)))
  expect_true(all.equal(attributes(cdm$death_cohort), attributes(old_cdm$death_cohort)))

  survcr <- estimateCompetingRiskSurvival(cdm, "mgus_diagnosis", "progression", "death_cohort")
  expect_true(all.equal(cdm$mgus_diagnosis, old_cdm$mgus_diagnosis))
  expect_true(all.equal(cdm$death_cohort, old_cdm$death_cohort))
  expect_true(all.equal(cdm$progression, old_cdm$progression))
  expect_true(all.equal(attributes(cdm$mgus_diagnosis), attributes(old_cdm$mgus_diagnosis)))
  expect_true(all.equal(attributes(cdm$death_cohort), attributes(old_cdm$death_cohort)))
  expect_true(all.equal(attributes(cdm$progression), attributes(old_cdm$progression)))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("median survival is NA when it cannot be estimated completely", {
  cdm <- mockMGUS2cdm()
  rows_allowed <- c(seq(1,1400,2))
  cdm$death_cohort <- cdm$death_cohort |>
    dplyr::filter(subject_id %in% rows_allowed) # less than 50% of people in target_cohort

  surv_summary <- estimateSingleEventSurvival(cdm, "mgus_diagnosis", "death_cohort",
                                              strata = list("age")) |>
    dplyr::filter(result_id == 3)

  # this should have either all NAs (x3) or all numbers (x3)
  expect_true(
    all(
    surv_summary |>
    dplyr::filter(grepl("median_survival",estimate_name), !is.na(estimate_value)) |>
      dplyr::arrange(strata_name, strata_level) |>
      dplyr::pull(estimate_value) ==
    c("135","124","191","189","173","228")
  ))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("empty input cohort after input filtering", {
  cdm <- mockMGUS2cdm()
  expect_warning(surv <- estimateSingleEventSurvival(cdm, "mgus_diagnosis", "death_cohort",
                                              censorOnCohortExit = TRUE))

  # this should give a warning and return a result with only attrition
  expect_true(omopgenerics::settings(surv) |>
                dplyr::pull(result_type) == "survival_attrition")

  expect_warning(survcr <- estimateCompetingRiskSurvival(cdm, "mgus_diagnosis", "progression",
                                                         "death_cohort",
                                                         censorOnCohortExit = TRUE))

  # this should give a warning and return a result with only attrition
  expect_true(omopgenerics::settings(survcr) |>
                dplyr::pull(result_type) == "survival_attrition")

  # add a second target cohort with cohort_end_date plus a year which should yield results
  cdm$mgus_diagnosis <- cdm$mgus_diagnosis |>
    dplyr::union_all(
      cdm$mgus_diagnosis |>
        dplyr::mutate(cohort_definition_id = 2,
                      cohort_end_date = as.Date("2020-01-01"))
    )
  attr(cdm$mgus_diagnosis, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1,2),
    cohort_name = c("mgus_diagnosis", "mgus_diagnosis_2020")
  )
  attr(cdm$mgus_diagnosis, "cohort_attrition") <- attr(cdm$mgus_diagnosis, "cohort_attrition") |>
    dplyr::union_all(
      attr(cdm$mgus_diagnosis, "cohort_attrition") |>
        dplyr::mutate(cohort_definition_id = 2)
    )

  expect_warning(surv <- estimateSingleEventSurvival(cdm, "mgus_diagnosis", "death_cohort",
                                                     censorOnCohortExit = TRUE))

  expect_warning(survcr <- estimateCompetingRiskSurvival(cdm, "mgus_diagnosis", "progression",
                                                         "death_cohort",
                                                         censorOnCohortExit = TRUE))

  expect_true(all(omopgenerics::settings(surv) |>
                dplyr::pull(result_type) == c("survival_estimates", "survival_events",
                "survival_summary", "survival_attrition")))

  expect_true(all(omopgenerics::settings(survcr) |>
                    dplyr::pull(result_type) == c("survival_estimates", "survival_events",
                                                  "survival_summary", "survival_attrition")))

  expect_true(all(omopgenerics::filterSettings(surv, result_id == 4) |>
                    dplyr::pull(group_level) |> unique() == c("mgus_diagnosis_1", "mgus_diagnosis_2020_2")))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("empty input cohort before input filtering", {
  cdm <- mockMGUS2cdm()
  cdm$mgus_diagnosis <- cdm$mgus_diagnosis |>
    dplyr::filter(cohort_definition_id == 10)
  expect_warning(surv <- estimateSingleEventSurvival(cdm, "mgus_diagnosis", "death_cohort"))
expect_true(surv |>
              dplyr::tally() |>
              dplyr::pull() == 0)
  CDMConnector::cdmDisconnect(cdm)
})

test_that("multiple strata names", {
  cdm <- mockMGUS2cdm()
  cdm$mgus_diagnosis <- cdm$mgus_diagnosis |>
    dplyr::mutate(
      sex = dplyr::if_else(
        sex == "M", "Male", "Female"
      )
    )
  surv <- estimateSingleEventSurvival(cdm, "mgus_diagnosis", "death_cohort",
                                      strata = list("age_group", "sex", c("age_group", "sex")))

  expect_true(
    surv |> asSurvivalResult() |> dplyr::select( sex) |> dplyr::distinct() |> dplyr::tally() == 3
  )

  CDMConnector::cdmDisconnect(cdm)
})

test_that("cohort id or cohort name as inputs", {
  cdm <- mockMGUS2cdm()
  cdm <- bind(cdm$progression, cdm$death_cohort, name = "outcome_cohorts")

  surv_id <- estimateSingleEventSurvival(cdm, "mgus_diagnosis", "outcome_cohorts", outcomeCohortId = 2)
  surv_name <- estimateSingleEventSurvival(cdm, "mgus_diagnosis", "outcome_cohorts", outcomeCohortId = "death_cohort")

  expect_true(
    all.equal(surv_id, surv_name, check.attributes = FALSE)
  )

  surv_cr_id <- estimateCompetingRiskSurvival(cdm, "mgus_diagnosis", "progression", "outcome_cohorts", competingOutcomeCohortId = 2)
  surv_cr_name <- estimateCompetingRiskSurvival(cdm, "mgus_diagnosis", "progression", "outcome_cohorts", competingOutcomeCohortId = "death_cohort")

  expect_true(
    all.equal(surv_cr_id, surv_cr_name, check.attributes = FALSE)
  )

  CDMConnector::cdmDisconnect(cdm)
})

test_that("estimateSingleEventSurvival includes new quantiles q05 and q95", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      outcomeCohortId = 1,
                                      outcomeCohortTable = "death_cohort")

  # Check that the new quantiles are present in the summary
  summary_data <- surv |>
    omopgenerics::filterSettings(result_type == "survival_summary")

  estimate_names <- unique(summary_data$estimate_name)

  # Check that q05 and q95 quantiles are included
  expect_true("q05_survival" %in% estimate_names)
  expect_true("q05_survival_95CI_lower" %in% estimate_names)
  expect_true("q05_survival_95CI_higher" %in% estimate_names)
  expect_true("q95_survival" %in% estimate_names)
  expect_true("q95_survival_95CI_lower" %in% estimate_names)
  expect_true("q95_survival_95CI_higher" %in% estimate_names)

  # Check that existing quantiles are still present
  expect_true("q0_survival" %in% estimate_names)
  expect_true("q25_survival" %in% estimate_names)
  expect_true("q75_survival" %in% estimate_names)
  expect_true("q100_survival" %in% estimate_names)

  CDMConnector::cdmDisconnect(cdm)
})

test_that("tableSurvival estimates parameter works correctly", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      outcomeCohortTable = "death_cohort")

  # Test default estimates
  table_default <- tableSurvival(surv, type = "tibble")
  expect_true(any(grepl("Median survival", colnames(table_default))))
  expect_true(any(grepl("Restricted mean survival", colnames(table_default))))

  # Test custom estimates with quantiles
  table_quantiles <- tableSurvival(surv,
                                   estimates = c("q05_survival", "q25_survival", "q75_survival", "q95_survival"),
                                   type = "tibble")
  expect_true(any(grepl("5% quantile", colnames(table_quantiles))))
  expect_true(any(grepl("25% quantile", colnames(table_quantiles))))
  expect_true(any(grepl("75% quantile", colnames(table_quantiles))))
  expect_true(any(grepl("95% quantile", colnames(table_quantiles))))

  # Test single estimate
  table_median_only <- tableSurvival(surv,
                                     estimates = "median_survival",
                                     type = "tibble")
  expect_true(any(grepl("Median survival", colnames(table_median_only))))
  expect_false(any(grepl("Restricted mean survival", colnames(table_median_only))))

  # Test invalid estimate should error
  expect_error(tableSurvival(surv, estimates = "invalid_estimate"))

  CDMConnector::cdmDisconnect(cdm)
})
