# Copyright 2025 DARWIN EU®
#
# This file is part of CohortSurvival
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

mockCohortSurvival <- function(tables, cohortTables, cdmName = "mock") {
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  cdm <- omopgenerics::cdmFromTables(
    tables = tables,
    cohortTables = cohortTables,
    cdmName = cdmName
  )

  cdm <- CDMConnector::copyCdmTo(db, cdm, schema = "main", overwrite = TRUE)

  attr(cdm, "cdm_schema") <- "main"
  attr(cdm, "write_schema") <- "main"

  return(cdm)
}
