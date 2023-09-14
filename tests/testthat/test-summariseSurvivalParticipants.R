# test_that("basic table", {
#
#   cdm <- mockMGUS2cdm()
#   surv <- estimateSurvival(cdm,
#                            targetCohortTable = "mgus_diagnosis",
#                            outcomeCohortTable = "death_cohort",
#                            returnParticipants = TRUE
#   )
#    tableOne <- summariseCharacteristics(cohort = survivalParticipants(surv) %>%
#                                           CDMConnector::computeQuery() ,
#                                              cdm = cdm)
#
#   expect_false(is.null(tableOne))
#   CDMConnector::cdm_disconnect(cdm)
#   })
