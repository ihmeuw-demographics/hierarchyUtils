
# Scale categorical variable with varying interval id cols ----------------

# Inputs:
# - males: 5-calendar-year interval and 5-year age groups up to 95+
# - females: 1-calendar-year interval and 1-year age groups up to 95+
# - all sexes combined: 5-calendar year interval and 0-15, 15-60 and 60+ age
#     groups. Values are double the sum of male and female values.
# Output:
# - males: row values doubled
# - females: row values doubled
# - all sexes combined: unchanged

sex_mapping <- data.table(parent = "all", child = c("female", "male"))
id_cols <- c("year_start", "year_end", "sex", "age_start", "age_end")
value_cols <- c("value")

# set up test input data.table
input_dt_male <- CJ(
  year_start = 2005, year_end = 2010,
  sex = "male",
  age_start = seq(0, 95, 5),
  value = 25
)
input_dt_male[age_start == 95, value := 5]

input_dt_female <- CJ(
  year_start = 2005:2009,
  sex = "female",
  age_start = seq(0, 95, 1),
  value = 1
)
gen_end(
  input_dt_female,
  id_cols = setdiff(id_cols, c("year_end", "age_end")),
  col_stem = "year",
  right_most_endpoint = 2010
)

input_dt_both <- CJ(
  year_start = 2005, year_end = 2010,
  sex = "all",
  age_start = c(0, 15, 60)
)
input_dt_both[age_start == 0, value := 300]
input_dt_both[age_start == 15, value := 900]
input_dt_both[age_start == 60, value := 720]

input_dt <- rbind(input_dt_male, input_dt_female, input_dt_both)
gen_end(input_dt, id_cols = setdiff(id_cols, "age_end"), col_stem = "age")
setkeyv(input_dt, id_cols)

# set up expected output table
expected_dt <- copy(input_dt)
expected_dt[sex != "all", value := value * 2]
setkeyv(expected_dt, id_cols)

description <- "scaling a categorical variable with varying interval id cols
works"
test_that(description, {

  # since interval id columns are not collapsed to most common intervals
  expect_error(
    scale(
      dt = input_dt,
      id_cols = id_cols,
      value_cols = value_cols,
      col_stem = "sex",
      col_type = "categorical",
      mapping = sex_mapping
    ),
    regexp = "expected input data is missing"
  )

  output_dt <- scale(
    dt = input_dt,
    id_cols = id_cols,
    value_cols = value_cols,
    col_stem = "sex",
    col_type = "categorical",
    mapping = sex_mapping,
    collapse_interval_cols = T
  )
  expect_identical(output_dt, expected_dt)
})

description <- "error is thrown when scaling a categorical variable and
levels are missing"
test_that(description, {

  new_input_dt <- input_dt[!(sex == "female" & year_start == 2008)]
  expect_error(
    scale(
      dt = new_input_dt,
      id_cols = id_cols,
      value_cols = value_cols,
      col_stem = "sex",
      col_type = "categorical",
      mapping = sex_mapping,
      collapse_interval_cols = T
    ),
    regexp = "intervals in `dt` are missing making it impossible to collapse"
  )

  new_input_dt <- input_dt[sex != "all"]
  expect_error(
    scale(
      dt = new_input_dt,
      id_cols = id_cols,
      value_cols = value_cols,
      col_stem = "sex",
      col_type = "categorical",
      mapping = sex_mapping,
      collapse_interval_cols = T
    ),
    regexp = "expected input data is missing"
  )
})

description <- "error is thrown when scaling a categorical variable with missing
id interval columns or is silent and scales possible intervals if requested to
not error out"
test_that(description, {

  new_input_dt <- input_dt[!(age_start == 24 & year_start == 2008) &
                             !(age_start == 46 & year_start == 2006)]
  new_expected_dt <- copy(expected_dt)
  new_expected_dt <- new_expected_dt[!(age_start == 24 & year_start == 2008) &
                                       !(age_start == 46 & year_start == 2006)]
  new_expected_dt[between(age_start, 15, 59) & sex != "all", value := value / 2]

  expect_error(
    scale(
      dt = new_input_dt,
      id_cols = id_cols,
      value_cols = value_cols,
      col_stem = "sex",
      col_type = "categorical",
      mapping = sex_mapping,
      collapse_interval_cols = T
    ),
    regexp = "intervals in `dt` are missing making it impossible to collapse"
  )

  output_dt <- scale(
    dt = new_input_dt,
    id_cols = id_cols,
    value_cols = value_cols,
    col_stem = "sex",
    col_type = "categorical",
    mapping = sex_mapping,
    missing_dt_severity = "none",
    collapse_interval_cols = T
  )
  expect_identical(output_dt, new_expected_dt)
})

# Scale interval variable with varying interval id cols -------------------

# Inputs:
# - 1st level
#   - 5-calendar-year interval and all-ages (value = 4 * 480 = 1920)
# - 2nd level (males only)
#   - 5-calendar-year interval and 5-year age groups up to 90-95 (value = 2 * 25 = 50)
#   - 5-calendar year interval and terminal age group 95+ (value = 2 * 5 = 10)
# - 2nd level (females only)
#   - 1-calendar year interval and 5-year age groups up to 95+ (value = 2 * 5 = 10)
#   - 1-calendar year interval and terminal age group 95+ (value = 2 * 1 = 2)
# - 3rd level (females only)
#   - 1-calendar year interval and 1-year age groups up to 95+ (value = 1 * 1 = 1)
# Output:
# - 1st level
#   - 5-calendar-year interval and all-ages (value = 4 * 480 = 1920)
# - 2nd level (males only)
#   - 5-calendar-year interval and 5-year age groups up to 90-95 (value = 2 * original = 100)
#   - 5-calendar year interval and terminal age group 95+ (value = 2 * original = 20)
# - 2nd level (females only)
#   - 1-calendar year interval and 5-year age groups up to 95+ (value = 2 * original = 20)
#   - 1-calendar year interval and terminal age group 95+ (value = 2 * original = 4)
# - 3rd level (females only)
#   - 1-calendar year interval and 1-year age groups up to 95+ (value = 4 * original = 4)

id_cols <- c("year_start", "year_end", "sex", "age_start", "age_end")
value_cols <- c("value")

# total number of most detailed age and year groupings
total_age_years <- length(seq(2005, 2009, 1)) * length(seq(0, 95, 1))

# create 1st level for males
input_dt_male1 <- data.table::CJ(
  year_start = 2005,
  year_end = 2010,
  sex = "male",
  age_start = 0,
  age_end = Inf,
  value = total_age_years
)

# create 2nd level for males
input_dt_male2 <- data.table::CJ(
  year_start = 2005,
  year_end = 2010,
  sex = "male",
  age_start = seq(0, 95, 5)
)
gen_end(input_dt_male2, setdiff(id_cols, "age_end"), col_stem = "age")
gen_length(input_dt_male2, col_stem = "age")
input_dt_male2[is.infinite(age_length), age_length := 1]
gen_length(input_dt_male2, col_stem = "year")
input_dt_male2[, value := age_length * year_length]
input_dt_male2[, c("age_length", "year_length") := NULL]

# create 1st level for females
input_dt_female1 <- data.table::CJ(
  year_start = 2005,
  year_end = 2010,
  sex = "female",
  age_start = 0,
  age_end = Inf,
  value = total_age_years
)

# create 2nd level for females
input_dt_female2 <- data.table::CJ(
  year_start = 2005:2009,
  sex = "female",
  age_start = seq(0, 95, 5)
)
gen_end(
  input_dt_female2, setdiff(id_cols, c("age_end", "year_end")),
  col_stem = "year", right_most_endpoint = 2010
)
gen_end(input_dt_female2, setdiff(id_cols, "age_end"), col_stem = "age")
gen_length(input_dt_female2, col_stem = "age")
input_dt_female2[is.infinite(age_length), age_length := 1]
gen_length(input_dt_female2, col_stem = "year")
input_dt_female2[, value := age_length * year_length]
input_dt_female2[, c("age_length", "year_length") := NULL]

# create 3rd level for females
input_dt_female3 <- data.table::CJ(
  year_start = 2005:2009,
  sex = "female",
  age_start = 0:94
)
gen_end(
  input_dt_female3, setdiff(id_cols, c("age_end", "year_end")),
  col_stem = "year", right_most_endpoint = 2010
)
gen_end(
  input_dt_female3, setdiff(id_cols, "age_end"),
  col_stem = "age", right_most_endpoint = 95
)
gen_length(input_dt_female3, col_stem = "age")
gen_length(input_dt_female3, col_stem = "year")
input_dt_female3[, value := age_length * year_length]
input_dt_female3[, c("age_length", "year_length") := NULL]

# combine together
input_dt <- list(
  "male1" = input_dt_male1,
  "male2" = input_dt_male2,
  "female1" = input_dt_female1,
  "female2" = input_dt_female2,
  "female3" = input_dt_female3
)

# modify values so that scaling will correct for scalars applied
input_dt[["male1"]][, value := value * 4]
input_dt[["female1"]][, value := value * 4]
input_dt[["male2"]][, value := value * 2]
input_dt[["female2"]][, value := value * 2]

# modify values expected after scaling applied
expected_dt <- copy(input_dt)
expected_dt[["male2"]][, value := value * 2]
expected_dt[["female2"]][, value := value * 2]
expected_dt[["female3"]][, value := value * 4]

# final formatting
input_dt <- rbindlist(input_dt)
setkeyv(input_dt, id_cols)
expected_dt <- rbindlist(expected_dt)
setkeyv(expected_dt, id_cols)

description <- "scaling an interval variable with varying interval id cols
works"
testthat::test_that(description, {
  output_dt <- scale(
    dt = input_dt,
    id_cols = id_cols,
    value_cols = value_cols,
    col_stem = "age",
    col_type = "interval",
    collapse_interval_cols = T
  )
  testthat::expect_identical(output_dt, expected_dt)
})

description <- "throws error when scaling an interval variable and there are
missing intervals"
testthat::test_that(description, {

  # missing one single year age group in females only
  new_input_dt <- input_dt[!(age_start == 24 & year_start == 2008)]

  new_expected_dt <- expected_dt[!(age_start == 24 & year_start == 2008)]
  gen_length(new_expected_dt, "age")
  new_expected_dt[between(age_start, 20, 24) & age_length == 1,
                  value := value / 4]
  new_expected_dt[, age_length := NULL]

  expect_error(
    scale(
      dt = new_input_dt,
      id_cols = id_cols,
      value_cols = value_cols,
      col_stem = "age",
      col_type = "interval",
      collapse_interval_cols = TRUE
    ),
    regexp = "intervals in `dt` are missing making it impossible to collapse"
  )

  output_dt <- scale(
    dt = new_input_dt,
    id_cols = id_cols,
    value_cols = value_cols,
    col_stem = "age",
    col_type = "interval",
    missing_dt_severity = "none",
    collapse_interval_cols = TRUE
  )
  expect_identical(output_dt, new_expected_dt)
})

# Scale multiplicative values ---------------------------------------------

sex_mapping <- data.table(parent = "all", child = c("female", "male"))
id_cols <- c("year", "sex")
value_cols <- "value"

# set up test input data.table
input_dt <- CJ(year = 2010, sex = c("female", "male"), value = 0.9)
input_dt_both <- CJ(year = 2010, sex = "all", value = 0.95)
input_dt <- rbind(input_dt, input_dt_both, use.names = T)
setkeyv(input_dt, id_cols)

# set up expected output table
expected_dt <- CJ(year = 2010, sex = c("female", "male"), value = sqrt(0.95))
expected_dt <- rbind(expected_dt, input_dt_both, use.names = T)
setkeyv(expected_dt, id_cols)

description <- "scaling using 'prod' as the aggregation function works"
test_that(description, {
  output_dt <- scale(
    dt = input_dt,
    id_cols = id_cols,
    value_cols = "value",
    col_stem = "sex",
    col_type = "categorical",
    mapping = sex_mapping,
    agg_function = prod
  )
  expect_identical(output_dt, expected_dt)
})

# Scale categorical variable with multiple levels in mapping --------------

# Inputs:
# - all present day & historical provinces in Iran as defined in `iran_mapping`
#   includes all the nodes in the mapping https://ihmeuw-demographics.github.io/hierarchyUtils/articles/hierarchyUtils.html#aggregate-locations-1
# - each province's value is equal to the number of present day provinces that make up the province
# - Iran national is equal to 2 times the number of present day provinces
# Output:
# - Iran national is the same
# - All provinces are equal to 2 times the original value

id_cols <- c("location", "year")
value_cols <- c("value")

# set up test input data.table so that value is equal to number of child nodes
# for each location
input_dt <- CJ(
  location = iran_mapping[, unique(c(child, parent))],
  year = 2011,
  value = 1
)
input_dt[location %in% c("Tehran 2006", "Zanjan 1976-1996",
                         "Mazandaran 1956-1996", "East Azarbayejan 1956-1986",
                         "Khuzestan and Lorestan 1956", "Isfahan and Yazd 1966"),
         value := 2]
input_dt[location %in% c("Tehran 1986-1995", "Gilan 1956-1966",
                         "Kermanshahan 1956", "Khorasan 1956-1996",
                         "Isfahan and Yazd 1956"),
         value := 3]
input_dt[location %in% c("Markazi 1966-1976", "Fars and Ports 1956"), value := 4]
input_dt[location %in% "Markazi 1956", value := 5]
input_dt[location %in% "Iran (Islamic Republic of)", value := 31]

# multiply top node by 2 to force scaling
input_dt[location == "Iran (Islamic Republic of)", value := value * 2]
setkeyv(input_dt, id_cols)

# set up expected output table
expected_dt <- copy(input_dt)
expected_dt[location != "Iran (Islamic Republic of)", value := value * 2]
setkeyv(expected_dt, id_cols)

description <- "scaling of categorical variable with multiple levels works"
test_that(description, {
  output_dt <- scale(
    dt = input_dt,
    id_cols = id_cols,
    value_cols = value_cols,
    col_stem = "location",
    col_type = "categorical",
    mapping = iran_mapping
  )
  expect_identical(output_dt, expected_dt)
})

description <- "scaling of categorical variable with missing intermediate levels
works when `collapse_missing=TRUE` and throws an error otherwise"
test_that(description, {

  # only include the present day provinces and the national level
  new_input_dt <- input_dt[!grepl("[0-9]+", location)]
  new_expected_dt <- expected_dt[!grepl("[0-9]+", location)]

  expect_error(
    scale(
      dt = new_input_dt,
      id_cols = id_cols,
      value_cols = value_cols,
      col_stem = "location",
      col_type = "categorical",
      mapping = iran_mapping
    ),
    regexp = "expected input data is missing"
  )

  output_dt <- scale(
    dt = new_input_dt,
    id_cols = id_cols,
    value_cols = value_cols,
    col_stem = "location",
    col_type = "categorical",
    mapping = iran_mapping,
    collapse_missing = TRUE
  )
  expect_identical(output_dt, new_expected_dt)
})

description <- "scaling of categorical variable with top level missing throws an
error"
test_that(description, {

  new_input_dt <- input_dt[!location %in% c("Iran (Islamic Republic of)")]

  expect_error(
    scale(
      dt = new_input_dt,
      id_cols = id_cols,
      value_cols = value_cols,
      col_stem = "location",
      col_type = "categorical",
      mapping = iran_mapping
    ),
    regexp = "expected input data is missing"
  )
  expect_error(
    scale(
      dt = new_input_dt,
      id_cols = id_cols,
      value_cols = value_cols,
      col_stem = "location",
      col_type = "categorical",
      mapping = iran_mapping,
      collapse_missing = TRUE
    ),
    regexp = "expected input data is missing"
  )
})

# Small special case tests ------------------------------------------------

mapping <- data.table(child = 2:3, parent = 1)
input_dt <- data.table(location_id = 1:3, sex = "all")
input_dt[, value := rnorm(.N)]
test_that("scaling a numeric 'categorical' variable works", {
  expect_error(
    scale(
      dt = input_dt,
      id_cols = c("location_id", "sex"),
      value_cols = "value",
      col_stem = "location_id",
      col_type = "categorical",
      mapping = mapping
    ),
    NA
  )
})

# set up test input data.table
# 0-4, 5-10, 0-10 age groups
input_dt <- data.table(
  year = 2010,
  age_start = c(0, 5, 0), age_end = c(4, 10, 10),
  value = 1
)
description <- "scaling interval variables with weird overlapping intervals
errors out"
testthat::test_that(description, {
  testthat::expect_error(
    scale(
      dt = input_dt,
      id_cols = c("year", "age_start", "age_end"),
      value_cols = "value",
      col_stem = "age",
      col_type = "interval"
    ),
    regexp = "expected input data is missing"
  )
})

# set up test input data.table
# 0-5, 5-10, 4-6, 0-10 age groups
input_dt <- data.table(
  year = 2010,
  age_start = c(0, 5, 4, 0),
  age_end = c(5, 10, 6, 10),
  value = 1
)
description <- "scaling age intervals errors out when given
weird overlapping intervals"
testthat::test_that(description, {
  testthat::expect_error(
    scale(
      dt = input_dt,
      id_cols = c("year", "age_start", "age_end"),
      value_cols = "value",
      col_stem = "age",
      col_type = "interval"
    ),
    regexp = "Some overlapping intervals are in `dt`"
  )
})
