# Changelog

## CohortSurvival 1.1.2

CRAN release: 2026-07-03

- Expand survival documentation and vignettes by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#415](https://github.com/darwin-eu/CohortSurvival/issues/415),
  [\#416](https://github.com/darwin-eu/CohortSurvival/issues/416)
- Move `DBI` to `Suggests` and check optional mock-CDM dependencies only
  when the mock helpers are used by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#413](https://github.com/darwin-eu/CohortSurvival/issues/413)
- Fallback to single event survival output when a competing risk
  analysis has no observed competing events by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#412](https://github.com/darwin-eu/CohortSurvival/issues/412)
- Improve handling of empty target and outcome cohorts by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#411](https://github.com/darwin-eu/CohortSurvival/issues/411)
- Improve survival table output, especially for competing risk results
  by [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#409](https://github.com/darwin-eu/CohortSurvival/issues/409),
  [\#411](https://github.com/darwin-eu/CohortSurvival/issues/411)

## CohortSurvival 1.1.1

CRAN release: 2026-05-13

- Remove duplicate outcome cohort validation by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#395](https://github.com/darwin-eu/CohortSurvival/issues/395)
- Add weighted Kaplan-Meier support to survival estimation by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#393](https://github.com/darwin-eu/CohortSurvival/issues/393)
- Allow `censorOnDate` to be supplied as a cohort column in
  [`addCohortSurvival()`](https://darwin-eu.github.io/CohortSurvival/reference/addCohortSurvival.md)
  by [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#392](https://github.com/darwin-eu/CohortSurvival/issues/392)
- Move `CDMConnector` to suggests by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#391](https://github.com/darwin-eu/CohortSurvival/issues/391)
- Allow `type = NULL` in
  [`plotSurvival()`](https://darwin-eu.github.io/CohortSurvival/reference/plotSurvival.md)
  by [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#390](https://github.com/darwin-eu/CohortSurvival/issues/390)
- Add BugReports by [@catalamarti](https://github.com/catalamarti)
  [\#383](https://github.com/darwin-eu/CohortSurvival/issues/383)

## CohortSurvival 1.1.0

CRAN release: 2025-12-04

- Use “first” option in addCohortSurvival to remove warning by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#375](https://github.com/darwin-eu/CohortSurvival/issues/375)
- Allow tableSurvival to display single event and competing risk
  together by [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#374](https://github.com/darwin-eu/CohortSurvival/issues/374)
- Allow empty results tables instead of giving errors by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#372](https://github.com/darwin-eu/CohortSurvival/issues/372)
- Add tableSurvivalAttrition by
  [@catalamarti](https://github.com/catalamarti)
  [\#371](https://github.com/darwin-eu/CohortSurvival/issues/371)
- Change default style argument to NULL by
  [@catalamarti](https://github.com/catalamarti)
  [\#368](https://github.com/darwin-eu/CohortSurvival/issues/368)
- Use native pipe and remove magrittr dependency by
  [@catalamarti](https://github.com/catalamarti)
  [\#366](https://github.com/darwin-eu/CohortSurvival/issues/366)
- Report quantiles .05 and .95 in estimateSurvival by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#365](https://github.com/darwin-eu/CohortSurvival/issues/365)
- Change result_type of competing risk for coherence by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#364](https://github.com/darwin-eu/CohortSurvival/issues/364)
- Allow cohort names in addCohortSurvival by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#363](https://github.com/darwin-eu/CohortSurvival/issues/363)
- Add style and hide argument to plot and table functions by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#350](https://github.com/darwin-eu/CohortSurvival/issues/350)

## CohortSurvival 1.0.3

CRAN release: 2025-08-18

- Fix bug on character days_to_event by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#348](https://github.com/darwin-eu/CohortSurvival/issues/348)
- Add logPrefix in compute() by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#347](https://github.com/darwin-eu/CohortSurvival/issues/347)
- Report survival output for empty target and outcome tables by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#346](https://github.com/darwin-eu/CohortSurvival/issues/346)
- Add availableSurvivalGrouping() function for clarity on plot variables
  by [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#344](https://github.com/darwin-eu/CohortSurvival/issues/344)
- Allow cohortId inputs to be cohort names by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#343](https://github.com/darwin-eu/CohortSurvival/issues/343)

## CohortSurvival 1.0.2

CRAN release: 2025-06-16

- Survival result keeps distinctive columns by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#335](https://github.com/darwin-eu/CohortSurvival/issues/335)
- Remove spaces in strata level by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#333](https://github.com/darwin-eu/CohortSurvival/issues/333)
- Added time scaling in survival plot
  [\#331](https://github.com/darwin-eu/CohortSurvival/issues/331)
- Problem when timeScale and times are incompatible by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#330](https://github.com/darwin-eu/CohortSurvival/issues/330)
- Loglog plot by [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#329](https://github.com/darwin-eu/CohortSurvival/issues/329)
- Create NEWS.md by [@catalamarti](https://github.com/catalamarti)
  [\#327](https://github.com/darwin-eu/CohortSurvival/issues/327)
- Attrition duplicates by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#326](https://github.com/darwin-eu/CohortSurvival/issues/326)
- Omopgenerics tidy by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#322](https://github.com/darwin-eu/CohortSurvival/issues/322)

## CohortSurvival 1.0.1

CRAN release: 2025-04-02

- Change RMST (SE) to RMST (95% CI) by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#316](https://github.com/darwin-eu/CohortSurvival/issues/316)
- Added change in time axis in plots by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#315](https://github.com/darwin-eu/CohortSurvival/issues/315)
- Tablesurvival months by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#312](https://github.com/darwin-eu/CohortSurvival/issues/312)
- Lifecycle Badge changes to Stable by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#311](https://github.com/darwin-eu/CohortSurvival/issues/311)
- Attrition problem in CR analyses by
  [@KimLopezGuell](https://github.com/KimLopezGuell)
  [\#308](https://github.com/darwin-eu/CohortSurvival/issues/308)

## CohortSurvival 1.0.0

CRAN release: 2025-03-19

- Stable release of the package
- Added a `NEWS.md` file to track changes to the package.
