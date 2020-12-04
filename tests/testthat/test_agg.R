
# Aggregate categorical variable with varying interval id cols ------------

# Inputs:
# - males: 5-calendar-year interval and 5-year age groups
# - females: 1-calendar-year interval and 1-year age groups
# Output:
# - all sexes combined: 5 calendar-year interval and 5-year age groups

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

input_dt <- rbind(input_dt_male, input_dt_female)
gen_end(input_dt, id_cols = setdiff(id_cols, "age_end"), col_stem = "age")
setkeyv(input_dt, id_cols)

# set up expected output data.table
expected_dt <- CJ(
  year_start = 2005, year_end = 2010,
  sex = "all",
  age_start = seq(0, 95, 5),
  value = 50
)
expected_dt[age_start == 95, value := 10]
gen_end(expected_dt, id_cols = setdiff(id_cols, "age_end"), col_stem = "age")
setkeyv(expected_dt, id_cols)

description <- "aggregating a categorical variable with varying interval id cols
works"
test_that(description, {

  # since interval id columns are not collapsed to most common intervals
  expect_error(
    agg(
      dt = input_dt,
      id_cols = id_cols, value_cols = value_cols,
      col_stem = "sex", col_type = "categorical",
      mapping = sex_mapping
    ),
    regexp = "expected input data is missing"
  )

  output_dt <- agg(
    dt = input_dt,
    id_cols = id_cols, value_cols = value_cols,
    col_stem = "sex", col_type = "categorical",
    mapping = sex_mapping,
    collapse_interval_cols = T
  )
  expect_identical(output_dt, expected_dt)
})

description <- "error is thrown when categorical aggregate is already included
in input or works when `present_agg_severity = 'none'`"
test_that(description, {

  new_input_dt <- rbind(input_dt, expected_dt)
  setkeyv(new_input_dt, key(input_dt))

  expect_error(
    agg(
      dt = new_input_dt,
      id_cols = id_cols, value_cols = value_cols,
      col_stem = "sex", col_type = "categorical",
      mapping = sex_mapping,
      collapse_interval_cols = T
    ),
    regexp = "aggregate data is already present"
  )

  output_dt <- agg(
    dt = new_input_dt,
    id_cols = id_cols, value_cols = value_cols,
    col_stem = "sex", col_type = "categorical",
    mapping = sex_mapping,
    present_agg_severity = "none",
    collapse_interval_cols = T
  )
  expect_identical(output_dt, expected_dt)
})

description <- "error is thrown when aggregating a categorical variable and
levels are missing"
test_that(description, {

  new_input_dt <- input_dt[sex != "female" & year_start == 2008]

  expect_error(
    agg(
      dt = new_input_dt,
      id_cols = id_cols, value_cols = value_cols,
      col_stem = "sex", col_type = "categorical",
      mapping = sex_mapping
    ),
    regexp = "expected input data is missing"
  )
})

description <- "error is thrown when aggregating categorical variable with
missing interval id column values or makes possible aggregates with
`missing_dt_severity` = 'none'`"
test_that(description, {

  new_input_dt <- input_dt[!(age_start == 24 & year_start == 2008) &
                             !(age_start == 46 & year_start == 2006)]
  new_expected_dt <- expected_dt[!(age_start %in% c(20, 45))]

  expect_error(
    agg(dt = new_input_dt,
        id_cols = id_cols, value_cols = value_cols,
        col_stem = "sex", col_type = "categorical",
        mapping = sex_mapping,
        collapse_interval_cols = TRUE
    ),
    regexp = "intervals in `dt` are missing making it impossible to collapse"
  )

  output_dt <- agg(
    dt = new_input_dt,
    id_cols = id_cols, value_cols = value_cols,
    col_stem = "sex", col_type = "categorical",
    mapping = sex_mapping,
    missing_dt_severity = "none",
    collapse_interval_cols = TRUE
  )
  expect_identical(output_dt, new_expected_dt)
})

# Aggregate interval variable with varying interval id cols ------------

# Inputs:
# - males: 5-calendar-year interval and 5-year age groups
# - females: 1-calendar-year interval and 1-year age groups
# Output:
# - aggregate age groups as defined in `age_mapping` for existing male and female
#   calendar year and age intervals

# set up mapping for aggregation over age
age_mapping <- data.table(
  age_start = c(0, 0, 15, 40, 85),
  age_end = c(Inf, 5, 60, 70, Inf)
)
id_cols <- c("year_start", "year_end", "sex", "age_start", "age_end")
value_cols <- c("value")

# set up expected output data.table
# expected value is number of single year age groups in aggregate interval
expected_dt_female <- CJ(year_start = 2005:2009, sex = "female")
gen_end(
  expected_dt_female,
  id_cols = c("year_start", "sex"),
  col_stem = "year",
  right_most_endpoint = 2010
)
expected_dt_female <- expected_dt_female[
  , data.table(age_mapping),
  by = c("year_start", "year_end", "sex")
]
expected_dt_female[, value := rep(c(96, 5, 45, 30, 11), 5)]

expected_dt_male <- CJ(year_start = 2005, year_end = 2010, sex = "male")
expected_dt_male <- expected_dt_male[
  , data.table(age_mapping),
  by = c("year_start", "year_end", "sex")
]
expected_dt_male[, value := c(96, 5, 45, 30, 11) * 5]

expected_dt <- rbind(expected_dt_female, expected_dt_male, use.names = T)
setcolorder(expected_dt, c(id_cols, value_cols))
setkeyv(expected_dt, id_cols)

description <- "aggregating an interval variable with varying interval id cols
works"
testthat::test_that(description, {
  output_dt <- agg(
    dt = input_dt,
    id_cols = id_cols, value_cols = value_cols,
    col_stem = "age", col_type = "interval",
    mapping = age_mapping,
    present_agg_severity = "skip"
  )
  testthat::expect_identical(output_dt, expected_dt)
})

description <- "throws error when aggregating an interval variable and there
are missing intervals"
testthat::test_that(description, {

  new_input_dt <- input_dt[!(sex == "female" & (age_start <= 2 | age_end >= 95))]
  setkeyv(new_input_dt, id_cols)

  new_expected_dt <- expected_dt[!(sex == "female" & (age_start <= 2 | age_end >= 95))]
  setkeyv(new_expected_dt, id_cols)

  # check severity
  expect_error(
    agg(dt = new_input_dt,
        id_cols = id_cols, value_cols = value_cols,
        col_stem = "age", col_type = "interval",
        mapping = age_mapping,
        missing_dt_severity = "stop",
        present_agg_severity = "skip"
    ),
    regexp = "intervals in `dt` are missing making it impossible to collapse"
  )

  output_dt <- agg(
    dt = new_input_dt,
    id_cols = id_cols, value_cols = value_cols,
    col_stem = "age", col_type = "interval",
    mapping = age_mapping,
    missing_dt_severity = "none",
    present_agg_severity = "skip"
  )
  expect_identical(output_dt, new_expected_dt)
})

description <- "error is thrown when interval aggregate is already included
in input or works when `present_agg_severity = 'none'`"
testthat::test_that(description, {

  new_input_dt <- unique(rbind(input_dt, expected_dt))
  setkeyv(new_input_dt, id_cols)

  setkeyv(expected_dt, id_cols)

  expect_error(
    agg(dt = new_input_dt,
        id_cols = id_cols, value_cols = value_cols,
        col_stem = "age", col_type = "interval",
        mapping = age_mapping
    ),
    regexp = "overlapping intervals were identified in `dt`"
  )
})


# Aggregate categorical variable with multiple levels in mapping ----------

# Inputs:
# - present day provinces (only the most detailed nodes) in Iran as defined in `iran_mapping`
#   see https://ihmeuw-demographics.github.io/hierarchyUtils/articles/hierarchyUtils.html#aggregate-locations-1
#   for visualization of mapping
# Output:
# - all historical provinces and the national level

id_cols <- c("location", "year")
value_cols <- c("value")

# set up test input data.table with only the present day provinces
input_dt <- CJ(
  location = iran_mapping[!grepl("[0-9]+", child), child],
  year = 2011,
  value = 1
)
setkeyv(input_dt, id_cols)

# set up expected output table with all unique locations
# the expected value is the number of leaf nodes under each aggregate
expected_dt <- CJ(location = unique(iran_mapping$parent),
                  year = 2011)
expected_dt[
  location %in% c(
    "Tehran 2006", "Zanjan 1976-1996", "Mazandaran 1956-1996",
    "East Azarbayejan 1956-1986", "Khuzestan and Lorestan 1956",
    "Isfahan and Yazd 1966"
  ),
  value := 2
]
expected_dt[
  location %in% c(
    "Tehran 1986-1995", "Gilan 1956-1966", "Kermanshahan 1956",
    "Khorasan 1956-1996", "Isfahan and Yazd 1956"
  ),
  value := 3
]
expected_dt[
  location %in% c("Markazi 1966-1976", "Fars and Ports 1956"),
  value := 4
]
expected_dt[location %in% "Markazi 1956", value := 5]
expected_dt[location %in% "Iran (Islamic Republic of)", value := 31]
setkeyv(expected_dt, id_cols)

description <- "aggregation of categorical variable with multiple levels works
(only most-detailed levels of mapping included in input `dt`)"
test_that(description, {
  output_dt <- agg(
    dt = input_dt,
    id_cols = id_cols, value_cols = value_cols,
    col_stem = "location", col_type = "categorical",
    mapping = iran_mapping
  )
  expect_identical(output_dt, expected_dt)
})

description <- "aggregation of categorical variable missing some leaf nodes in
mapping works when `missing_dt_severity = 'none'"
test_that(description, {

  new_input_dt <- input_dt[!location %in% c("Tehran", "Alborz")]
  setkeyv(new_input_dt, id_cols)
  new_expected_dt <- expected_dt[
    !location %in% c(
      "Tehran 2006", "Tehran 1986-1995", "Markazi 1966-1976", "Markazi 1956",
      "Iran (Islamic Republic of)"
    )
  ]
  setkeyv(new_expected_dt, id_cols)

  # check severity
  expect_error(
    agg(
      dt = new_input_dt,
      id_cols = id_cols, value_cols = value_cols,
      col_stem = "location", col_type = "categorical",
      mapping = iran_mapping,
      missing_dt_severity = "stop"
    ),
    regexp = "expected input data is missing"
  )

  output_dt <- agg(
    dt = new_input_dt,
    id_cols = id_cols, value_cols = value_cols,
    col_stem = "location", col_type = "categorical",
    mapping = iran_mapping,
    missing_dt_severity = "none"
  )
  expect_identical(output_dt, new_expected_dt)
})

# Test aggregating categorical variable with different interval id --------

# Inputs:
# - present day provinces (only the most detailed nodes) in Iran as defined in
#   `iran_mapping`. 'Alborz' only has all-ages, all other provinces have 10-year
#   age groups.
#   see https://ihmeuw-demographics.github.io/hierarchyUtils/articles/hierarchyUtils.html#aggregate-locations-1
#   for visualization of mapping
# Output:
# - all aggregate locations containing 'Alborz' should have all-ages row, all
#   other aggregate locations should have 10-year age groups

id_cols <- c("location", "year", "age_start", "age_end")
value_cols <- c("value")

input_dt_iran2 <- rbind(
  input_dt[
    location == "Alborz"
    , list(age_start = 0, age_end = Inf, value = 11),
    by = c("location", "year")
  ],
  input_dt[
    location != "Alborz"
    , list(age_start = seq(0, 100, 10), age_end = c(seq(10, 100, 10), Inf), value = 1),
    by = c("location", "year")
  ]
)
setkeyv(input_dt_iran2, id_cols)

parent_alborz_locations <- c(
  "Iran (Islamic Republic of)", "Markazi 1956", "Markazi 1966-1976", "Semnan",
  "Markazi", "Tehran 1986-1995", "Tehran 2006", "Qom"
)
expected_dt_iran2 <- rbind(
  expected_dt[
    location %in% parent_alborz_locations
    , list(age_start = 0, age_end = Inf, value = value * 11),
    by = c("location", "year")
  ],
  expected_dt[
    !location %in% parent_alborz_locations
    , list(age_start = seq(0, 100, 10), age_end = c(seq(10, 100, 10), Inf), value = value),
    by = c("location", "year")
  ]
)
setkeyv(expected_dt_iran2, id_cols)

description <- "aggregating a categorical variable with varying interval id cols
works"
description <- "aggregation of categorical variable with multiple levels works
(only most-detailed levels of mapping included in input `dt`)"
test_that(description, {
  output_dt <- agg(
    dt = input_dt_iran2,
    id_cols = id_cols, value_cols = value_cols,
    col_stem = "location", col_type = "categorical",
    mapping = iran_mapping,
    collapse_interval_cols = TRUE
  )
  expect_identical(output_dt, expected_dt_iran2)
})

# Small special case tests ------------------------------------------------

# set up test input data.table
# 0-5, 5-10, 4-6 age groups
input_dt <- data.table(
  year = 2010,
  age_start = c(0, 5, 4), age_end = c(5, 10, 6),
  value = 1
)

description <- "aggregating interval variables with weird overlapping intervals
errors out"
testthat::test_that(description, {
  testthat::expect_error(
    agg(
      dt = input_dt,
      id_cols = c("year", "age_start", "age_end"),
      value_cols = "value",
      col_stem = "age",
      col_type = "interval",
      mapping = data.table(age_start = 0, age_end = 10)
    ),
    msg = "overlapping intervals were identified in `dt`"
  )
})
