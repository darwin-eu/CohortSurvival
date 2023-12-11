test_that("mock mgus2 as a cdm reference", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()

  expect_true(cdm$person %>%
    dplyr::tally() %>%
    dplyr::pull("n") ==
    survival::mgus2 %>%
      dplyr::tally() %>%
      dplyr::pull("n"))

  CDMConnector::cdmDisconnect(cdm)
})
