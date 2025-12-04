# Helper for consistent documentation of `tables`.

Helper for consistent documentation of `tables`.

## Arguments

- result:

  A summarised_result object obtained either from
  [`estimateSingleEventSurvival()`](https://darwin-eu.github.io/CohortSurvival/reference/estimateSingleEventSurvival.md)
  or
  [`estimateCompetingRiskSurvival()`](https://darwin-eu.github.io/CohortSurvival/reference/estimateCompetingRiskSurvival.md).

- header:

  A vector specifying the elements to include in the header. The order
  of elements matters, with the first being the topmost header. Elements
  in header can be:

  - Any of the columns returned by `tableColumns(result)` to create a
    header for these columns.

  - Any other input to create an overall header.

- hide:

  Columns to drop from the output table. By default, `result_id` and
  `estimate_type` are always dropped.

- groupColumn:

  Columns to use as group labels, to see options use
  `tableColumns(result)`. By default, the name of the new group will be
  the tidy\* column names separated by ";". To specify a custom group
  name, use a named list such as: list("newGroupName" =
  c("variable_name", "variable_level")).

  \*tidy: The tidy format applied to column names replaces "\_" with a
  space and converts to sentence case. Use `rename` to customise
  specific column names.

- type:

  Character string specifying the desired output table format. See
  `tableType()` for supported table types. If `type = NULL`, global
  options (set via `setGlobalTableOptions()`) will be used if available;
  otherwise, a default `'gt'` table is created.

- columnOrder:

  Character vector establishing the position of the columns in the
  formatted table. Columns in either header, groupColumn, or hide will
  be ignored.

- style:

  Defines the visual formatting of the table. This argument can be
  provided in one of the following ways:

  1.  **Pre-defined style:** Use the name of a built-in style (e.g.,
      `"darwin"`). See `tableStyle()` for available options.

  2.  **YAML file path:** Provide the path to an existing `.yml` file
      defining a new style.

  3.  **List of custome R code:** Supply a block of custom R code or a
      named list describing styles for each table section. This code
      must be specific to the selected table type. If `style = NULL`,
      the function will use global options (see
      `setGlobalTableOptions()`) or an existing `_brand.yml` file (if
      found); otherwise, the default style is applied. For more details,
      see the *Styles* vignette on the package website.

- .options:

  A named list with additional formatting options.
  [`visOmopResults::tableOptions()`](https://darwin-eu.github.io/visOmopResults/reference/tableOptions.html)
  shows allowed arguments and their default values.
