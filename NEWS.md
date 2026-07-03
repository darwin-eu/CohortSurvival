# CohortSurvival 1.1.2

* Expand survival documentation and vignettes by @KimLopezGuell #415, #416
* Move `DBI` to `Suggests` and check optional mock-CDM dependencies only when
  the mock helpers are used by @KimLopezGuell #413
* Fallback to single event survival output when a competing risk analysis has
  no observed competing events by @KimLopezGuell #412
* Improve handling of empty target and outcome cohorts by @KimLopezGuell #411
* Improve survival table output, especially for competing risk results by @KimLopezGuell #409, #411


# CohortSurvival 1.1.1

* Remove duplicate outcome cohort validation by @KimLopezGuell #395
* Add weighted Kaplan-Meier support to survival estimation by @KimLopezGuell #393
* Allow `censorOnDate` to be supplied as a cohort column in `addCohortSurvival()` by @KimLopezGuell #392
* Move `CDMConnector` to suggests by @KimLopezGuell #391
* Allow `type = NULL` in `plotSurvival()` by @KimLopezGuell #390
* Add BugReports by @catalamarti #383


# CohortSurvival 1.1.0

* Use "first" option in addCohortSurvival to remove warning by @KimLopezGuell #375
* Allow tableSurvival to display single event and competing risk together by @KimLopezGuell #374
* Allow empty results tables instead of giving errors by @KimLopezGuell #372
* Add tableSurvivalAttrition by @catalamarti #371
* Change default style argument to NULL by @catalamarti #368
* Use native pipe and remove magrittr dependency by @catalamarti #366
* Report quantiles .05 and .95 in estimateSurvival by @KimLopezGuell #365
* Change result_type of competing risk for coherence by @KimLopezGuell #364
* Allow cohort names in addCohortSurvival by @KimLopezGuell #363
* Add style and hide argument to plot and table functions by @KimLopezGuell #350


# CohortSurvival 1.0.3

* Fix bug on character days_to_event by @KimLopezGuell #348
* Add logPrefix in compute() by @KimLopezGuell #347
* Report survival output for empty target and outcome tables by @KimLopezGuell #346
* Add availableSurvivalGrouping() function for clarity on plot variables by @KimLopezGuell #344
* Allow cohortId inputs to be cohort names by @KimLopezGuell #343


# CohortSurvival 1.0.2

* Survival result keeps distinctive columns by @KimLopezGuell #335
* Remove spaces in strata level by @KimLopezGuell #333
* Added time scaling in survival plot #331
* Problem when timeScale and times are incompatible by @KimLopezGuell #330
* Loglog plot by @KimLopezGuell #329
* Create NEWS.md by @catalamarti #327
* Attrition duplicates by @KimLopezGuell #326
* Omopgenerics tidy by @KimLopezGuell #322

# CohortSurvival 1.0.1

* Change RMST (SE) to RMST (95% CI) by @KimLopezGuell #316
* Added change in time axis in plots by @KimLopezGuell #315 
* Tablesurvival months by @KimLopezGuell #312
* Lifecycle Badge changes to Stable by @KimLopezGuell #311
* Attrition problem in CR analyses by @KimLopezGuell #308 

# CohortSurvival 1.0.0

* Stable release of the package
* Added a `NEWS.md` file to track changes to the package.
