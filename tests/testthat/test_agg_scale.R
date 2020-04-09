library(data.table)
library(testthat)

# Aggregate to both sexes combined with different ages & years ------------

sex_mapping <- data.table(parent = "both",
                          child = c("female", "male"))
id_cols <- c("year_start", "year_end", "sex", "age_start", "age_end")
value_cols <- c("value")

# set up test input data.table
input_dt_male <- CJ(year_start = 2005, year_end = 2010,
                    sex = "male",
                    age_start = seq(0, 95, 5),
                    value = 25)
input_dt_male[age_start == 95, value := 5]
input_dt_female <- CJ(year_start = 2005:2009,
                      sex = "female",
                      age_start = seq(0, 95, 1),
                      value = 1)
gen_end(input_dt_female, setdiff(id_cols, c("year_end", "age_end")),
        col_stem = "year", right_most_endpoint = 2010)
input_dt <- rbind(input_dt_male, input_dt_female)
gen_end(input_dt, setdiff(id_cols, "age_end"), col_stem = "age")
setkeyv(input_dt, id_cols)

expected_dt <- CJ(year_start = 2005, year_end = 2010,
                  sex = "both",
                  age_start = seq(0, 95, 5),
                  value = 50)
expected_dt[age_start == 95, value := 10]
gen_end(expected_dt, setdiff(id_cols, "age_end"), col_stem = "age")
setkeyv(expected_dt, id_cols)

description <- "aggregation to boths sexes combined with different ages and
years works"
test_that(description, {
  output_dt <- agg(dt = input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "sex",
                   col_type = "categorical",
                   mapping = sex_mapping,
                   collapse_interval_cols = T)
  expect_identical(output_dt, expected_dt)
})

description <- "aggregation to boths sexes combined errors when aggregate ages
are included in dataset"
test_that(description, {
  input_dt_agg_age <- CJ(year_start = 2005, year_end = 2010,
                         sex = c("female", "male"),
                         age_start = 0, age_end = Inf,
                         value = 480)
  new_input_dt <- rbind(input_dt, input_dt_agg_age)

  expect_error(agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "sex",
                   col_type = "categorical",
                   mapping = sex_mapping),
               regexp = "input data is missing in `dt`")

  expect_error(agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "sex",
                   col_type = "categorical",
                   mapping = sex_mapping,
                   collapse_interval_cols = T),
               regexp = "Some aggregates are already in `dt`")
})

description <- "error is thrown when both sexes combined aggregate is already
included in input"
test_that(description, {
  new_input_dt <- rbind(input_dt, expected_dt)
  expect_error(agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "sex",
                   col_type = "categorical",
                   mapping = sex_mapping),
               regexp = "Some aggregates are already in `dt`.")
})

description <- "error is thrown when aggregating data with one missing sex"
test_that(description, {
  new_input_dt <- input_dt[sex != "female"]
  expect_error(agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "sex",
                   col_type = "categorical",
                   mapping = sex_mapping),
               regexp = "input data is missing in `dt`")
})

description <- "error is thrown when aggregating data with some missing ages or
years or is silent and aggregates possible intervals if requested to not error
out"
test_that(description, {
  new_input_dt <- input_dt[!(age_start == 24 & year_start == 2008) &
                             !(age_start == 46 & year_start == 2006)]
  new_expected_dt <- expected_dt[!(age_start %in% c(20, 45))]
  expect_error(agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "sex",
                   col_type = "categorical",
                   mapping = sex_mapping),
               regexp = "input data is missing in `dt`")
  expect_error(agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "sex",
                   col_type = "categorical",
                   mapping = sex_mapping,
                   collapse_interval_cols = TRUE),
               regexp = "input data is missing in `dt`")
  expect_error(agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "sex",
                   col_type = "categorical",
                   mapping = sex_mapping,
                   missing_dt_severity = "none"),
               regexp = "`dt` is empty")
  output_dt <- agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "sex",
                   col_type = "categorical",
                   mapping = sex_mapping,
                   missing_dt_severity = "none",
                   collapse_interval_cols = TRUE)
  expect_identical(output_dt, new_expected_dt)
})

# Scale sex-specific values with different ages & years -------------------

# set up test input data.table
input_dt_both <- CJ(year_start = 2005, year_end = 2010,
                    sex = "both",
                    age_start = c(0, 15, 60))
gen_end(input_dt_both, setdiff(id_cols, "age_end"), col_stem = "age")
input_dt_both[age_start == 0, value := 300]
input_dt_both[age_start == 15, value := 900]
input_dt_both[age_start == 60, value := 720]
input_dt <- rbind(input_dt, input_dt_both, use.names = T)
setkeyv(input_dt, id_cols)

# set up expected output table
expected_dt <- copy(input_dt)
expected_dt[sex != "both", value := value * 2]
setkeyv(expected_dt, id_cols)

description <- "scaling additive sex-specific values to both sexes combined
works"
test_that(description, {
  output_dt <- scale(dt = input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "sex",
                     col_type = "categorical",
                     mapping = sex_mapping,
                     collapse_interval_cols = T)
  expect_identical(output_dt, expected_dt)
})

description <- "error is thrown when scaling data with one missing sex"
test_that(description, {
  new_input_dt <- input_dt[sex != "female"]
  expect_error(scale(dt = new_input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "sex",
                     col_type = "categorical",
                     mapping = sex_mapping,
                     collapse_interval_cols = T),
               regexp = "input data is missing in `dt`")

  new_input_dt <- input_dt[sex != "both"]
  expect_error(scale(dt = new_input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "sex",
                     col_type = "categorical",
                     mapping = sex_mapping,
                     collapse_interval_cols = T),
               regexp = "input data is missing in `dt`")

})

description <- "error is thrown when scaling data with some missing ages or
years or is silent and scales possible intervals if requested to not error out"
test_that(description, {
  new_input_dt <- input_dt[!(age_start == 24 & year_start == 2008) &
                             !(age_start == 46 & year_start == 2006)]
  new_expected_dt <- copy(expected_dt)
  new_expected_dt <- new_expected_dt[!(age_start == 24 & year_start == 2008) &
                                       !(age_start == 46 & year_start == 2006)]
  new_expected_dt[between(age_start, 15, 59) & sex != "both", value := value / 2]
  expect_error(scale(dt = new_input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "sex",
                     col_type = "categorical",
                     mapping = sex_mapping,
                     collapse_interval_cols = T),
               regexp = "input data is missing in `dt`")

  output_dt <- scale(dt = new_input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "sex",
                     col_type = "categorical",
                     mapping = sex_mapping,
                     missing_dt_severity = "none",
                     collapse_interval_cols = T)
  expect_identical(output_dt, new_expected_dt)
})

# Aggregate over age intervals --------------------------------------------

# set up mapping for aggregation over age
age_mapping <- data.table(age_start = c(0, 0, 15, 40, 85),
                          age_end = c(Inf, 5, 60, 70, Inf))
id_cols <- c("year_start", "year_end", "sex", "age_start", "age_end")
value_cols <- c("value")

# set up input data.table
input_dt <- input_dt[sex != "both"]

# set up expected output data.table
# expected value is number of single year age groups in aggregate interval
expected_dt_female <- CJ(year_start = 2005:2009, sex = "female")
gen_end(expected_dt_female, id_cols = c("year_start", "sex"),
        col_stem = "year", right_most_endpoint = 2010)
expected_dt_female <- expected_dt_female[, data.table(age_mapping),
                                         by = c("year_start", "year_end", "sex")]
expected_dt_female[, value := c(96, 5, 45, 30, 11)]

expected_dt_male <- CJ(year_start = 2005, year_end = 2010, sex = "male")
expected_dt_male <- expected_dt_male[, data.table(age_mapping),
                                     by = c("year_start", "year_end", "sex")]
expected_dt_male[, value := c(96, 5, 45, 30, 11) * 5]

expected_dt <- rbind(expected_dt_female, expected_dt_male, use.names = T)
setcolorder(expected_dt, c(id_cols, value_cols))
setkeyv(expected_dt, id_cols)

testthat::test_that("aggregating age intervals works", {
  output_dt <- agg(dt = input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "age",
                   col_type = "interval",
                   mapping = age_mapping)
  testthat::expect_equal(output_dt, expected_dt)
})

description <- "aggregation of age-specific data with missing intervals throws
error, warning, message, or is silent and aggregates to possible aggregate ages
if requested to not error out"
testthat::test_that(description, {
  new_input_dt <- input_dt[age_start > 2 & age_end < 95]
  setkeyv(new_input_dt, id_cols)

  new_expected_dt <- expected_dt[age_start > 2 & age_end < 95]
  setkeyv(new_expected_dt, id_cols)

  # check severity
  expect_error(agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "age",
                   col_type = "interval",
                   mapping = age_mapping,
                   missing_dt_severity = "stop"),
               regexp = "input data is missing in `dt`")

  output_dt <- agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "age",
                   col_type = "interval",
                   mapping = age_mapping,
                   missing_dt_severity = "none")
  expect_identical(output_dt, new_expected_dt)
})

description <- "aggregation of age-specific data with aggregates already
included works when `drop_present_aggs` argument is set to TRUE and throws an
error otherwise"
testthat::test_that(description, {
  new_input_dt <- unique(rbind(input_dt, expected_dt))
  setkeyv(new_input_dt, id_cols)

  setkeyv(expected_dt, id_cols)

  expect_error(agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "age",
                   col_type = "interval",
                   mapping = age_mapping),
               regexp = "Some overlapping intervals are already in `dt`")
  output_dt <- agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "age",
                   col_type = "interval",
                   mapping = age_mapping,
                   drop_present_aggs = TRUE)
  expect_identical(output_dt, expected_dt)
})

# Scale over age intervals ------------------------------------------------

input_dt_detailed <- copy(input_dt)

age_mapping <- data.table(age_start = seq(0, 90, 5),
                          age_end = seq(5, 95, 5))
input_dt_agg1 <- agg(dt = input_dt_detailed,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "age",
                     col_type = "interval",
                     mapping = age_mapping)

age_mapping <- data.table(age_start = 0, age_end = Inf)
input_dt_agg2 <- agg(dt = input_dt_detailed,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "age",
                     col_type = "interval",
                     mapping = age_mapping)
year_mapping <- data.table(year_start = 2005, year_end = 2010)
input_dt_agg2 <- agg(dt = input_dt_agg2,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "year",
                     col_type = "interval",
                     mapping = year_mapping)

# combine together different inputs
input_dt <- rbind(input_dt_detailed, input_dt_agg1, input_dt_agg2,
                  use.names = T)
input_dt <- unique(input_dt)
setkeyv(input_dt, id_cols)

gen_length(input_dt, "age")
input_dt[age_start == 0 & age_end == Inf, value := value * 4]
input_dt[age_start == 95 & age_end == Inf, value := value * 2]
input_dt[age_length == 5, value := value * 2]

# aggregate the all-ages data for females to

# prepare expected outputs
expected_dt <- copy(input_dt)
expected_dt[age_length == 1, value := value * 4]
expected_dt[age_length == 5 | (age_start == 95 & age_end == Inf),
            value := value * 2]
setkeyv(expected_dt, id_cols)

input_dt[, age_length := NULL]
expected_dt[, age_length := NULL]

testthat::test_that("scaling age intervals works", {
  output_dt <- scale(dt = input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "age",
                     col_type = "interval",
                     collapse_interval_cols = T)
  testthat::expect_equal(output_dt, expected_dt)
})

description <- "error is thrown when scaling over an interval variable but there
are missing intervals"
testthat::test_that(description, {
  new_input_dt <- input_dt[!(age_start == 24 & year_start == 2008)]
  new_expected_dt <- expected_dt[!(age_start == 24 & year_start == 2008)]
  gen_length(new_expected_dt, "age")
  new_expected_dt[between(age_start, 20, 24) & age_length == 1 &
                    year_start == 2008, value := value / 4]
  new_expected_dt[, age_length := NULL]
  expect_error(scale(dt = new_input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "age",
                     col_type = "interval",
                     collapse_interval_cols = TRUE),
               regexp = "input data is missing in `dt`")

  output_dt <- scale(dt = new_input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "age",
                     col_type = "interval",
                     missing_dt_severity = "none",
                     collapse_interval_cols = TRUE)
  expect_identical(output_dt, new_expected_dt)
})

# Scale multiplicative sex-specific values to both sexes combined ---------

id_cols <- c("year", "sex")
value_cols <- "value"

# set up test input data.table
input_dt <- CJ(year = 2010, sex = c("female", "male"),
               value = 0.9)
input_dt_both <- CJ(year = 2010, sex = "both",
                    value = 0.95)
input_dt <- rbind(input_dt, input_dt_both, use.names = T)
setkeyv(input_dt, id_cols)

# set up expected output table
expected_dt <- CJ(year = 2010, sex = c("female", "male"),
                  value = sqrt(0.95))
expected_dt <- rbind(expected_dt, input_dt_both, use.names = T)
setkeyv(expected_dt, id_cols)

description <- "scaling multiplicative sex-specific values to both sexes
combined works"
test_that(description, {
  output_dt <- scale(dt = input_dt,
                     id_cols = id_cols,
                     value_cols = "value",
                     col_stem = "sex",
                     col_type = "categorical",
                     mapping = sex_mapping,
                     agg_function = prod)
  expect_identical(output_dt, expected_dt)
})

# Test different weird age intervals are caught ---------------------------

id_cols <- c("year", "age_start", "age_end")
value_cols <- "value"

# set up test input data.table
input_dt <- data.table(year = 2010,
                       age_start = c(0, 5, 4),
                       age_end = c(5, 10, 6),
                       value = 1)
description <- "aggregating age intervals errors out when given
weird overlapping intervals"
testthat::test_that(description, {
  testthat::expect_error(agg(dt = input_dt,
                             id_cols = id_cols,
                             value_cols = value_cols,
                             col_stem = "age",
                             col_type = "interval",
                             mapping = data.table(age_start = 0, age_end = 10)),
                         regexp = "Some overlapping intervals are already in `dt`")
})

# set up test input data.table
input_dt <- data.table(year = 2010,
                       age_start = c(0, 5, 0),
                       age_end = c(4, 10, 10),
                       value = 1)
description <- "scaling age intervals errors out when missing intervals"
testthat::test_that(description, {
  testthat::expect_error(scale(dt = input_dt,
                               id_cols = id_cols,
                               value_cols = value_cols,
                               col_stem = "age",
                               col_type = "interval"),
                         regexp = "input data is missing in `dt`")
})

# set up test input data.table
input_dt <- data.table(year = 2010,
                       age_start = c(0, 5, 4, 0),
                       age_end = c(5, 10, 6, 10),
                       value = 1)
description <- "scaling age intervals errors out when given
weird overlapping intervals"
testthat::test_that(description, {
  testthat::expect_error(scale(dt = input_dt,
                               id_cols = id_cols,
                               value_cols = value_cols,
                               col_stem = "age",
                               col_type = "interval"),
                         regexp = "Some overlapping intervals are in `dt`")
})

# Aggregate present day Iran Provinces ------------------------------------

id_cols <- c("location", "year")
value_cols <- c("value")

# set up test input data.table with only the present day provinces
input_dt <- CJ(location = iran_mapping[!grepl("[0-9]+", child), child],
               year = 2011,
               value = 1)
setkeyv(input_dt, id_cols)

# set up expected output table with all unique locations
# the expected value is the number of leaf nodes under each aggregate
expected_dt <- CJ(location = unique(iran_mapping$parent),
                  year = 2011)
expected_dt[location %in% c("Tehran 2006", "Zanjan 1976-1996",
                            "Mazandaran 1956-1996",
                            "East Azarbayejan 1956-1986",
                            "Khuzestan and Lorestan 1956",
                            "Isfahan and Yazd 1966"),
            value := 2]
expected_dt[location %in% c("Tehran 1986-1995", "Gilan 1956-1966",
                            "Kermanshahan 1956", "Khorasan 1956-1996",
                            "Isfahan and Yazd 1956"),
            value := 3]
expected_dt[location %in% c("Markazi 1966-1976", "Fars and Ports 1956"),
            value := 4]
expected_dt[location %in% "Markazi 1956",
            value := 5]
expected_dt[location %in% "Iran (Islamic Republic of)",
            value := 31]
setkeyv(expected_dt, id_cols)

test_that("aggregation of Iran data with only present day provinces works", {
  output_dt <- agg(dt = input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "location",
                   col_type = "categorical",
                   mapping = iran_mapping)
  expect_identical(output_dt, expected_dt)
})

description <- "aggregation of Iran data with Tehran 2006 instead of
present day 'Tehran' and 'Alborz' works"
test_that(description, {
  new_input_dt <- rbind(input_dt[!location %in% c("Tehran", "Alborz")],
                        expected_dt[location %in% c("Tehran 2006")], use.names = T)
  setkeyv(new_input_dt, id_cols)
  new_expected_dt <- expected_dt[!location %in% c("Tehran 2006")]
  setkeyv(new_expected_dt, id_cols)

  output_dt <- agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "location",
                   col_type = "categorical",
                   mapping = iran_mapping,
                   missing_dt_severity = "none")
  expect_identical(output_dt, new_expected_dt)
})

description <- "aggregation of Iran data with Tehran 2006 instead of
present day 'Tehran' and 'Alborz' (accidently including 'Tehran') errors out"
test_that(description, {
  new_input_dt <- rbind(input_dt[!location %in% c("Alborz")],
                        expected_dt[location %in% c("Tehran 2006")],
                        use.names = T)
  setkeyv(new_input_dt, id_cols)

  expect_error(agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "location",
                   col_type = "categorical",
                   mapping = iran_mapping,
                   missing_dt_severity = "none"),
               regexp = "Some aggregates are already in `dt`")
})

description <- "aggregation of Iran data with missing 'Tehran' and 'Alborz'
throws error, warning, message, or is silent and aggregates to possible
locations if requested to not error out"
test_that(description, {
  new_input_dt <- input_dt[!location %in% c("Tehran", "Alborz")]
  setkeyv(new_input_dt, id_cols)
  new_expected_dt <- expected_dt[!location %in%
                                   c("Tehran 2006", "Tehran 1986-1995",
                                     "Markazi 1966-1976", "Markazi 1956",
                                     "Iran (Islamic Republic of)")]
  setkeyv(new_expected_dt, id_cols)

  # check severity
  expect_error(agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "location",
                   col_type = "categorical",
                   mapping = iran_mapping,
                   missing_dt_severity = "stop"),
               regexp = "input data is missing in `dt`")

  output_dt <- agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "location",
                   col_type = "categorical",
                   mapping = iran_mapping,
                   missing_dt_severity = "none")
  expect_identical(output_dt, new_expected_dt)
})

description <- "aggregation of Iran data with aggregates already included works
when `drop_present_aggs` argument is set to TRUE and throws an error otherwise"
test_that(description, {
  new_input_dt <- rbind(input_dt, expected_dt, use.names = T)
  setkeyv(new_input_dt, id_cols)
  expect_error(agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "location",
                   col_type = "categorical",
                   mapping = iran_mapping),
               regexp = "Some aggregates are already in `dt`")

  output_dt <- agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "location",
                   col_type = "categorical",
                   mapping = iran_mapping,
                   drop_present_aggs = TRUE)
  expect_identical(output_dt, expected_dt)
})

# Scale present day Iran Provinces ----------------------------------------

# set up test input data.table with present day and aggregate provinces
input_dt <- rbind(input_dt, expected_dt)
setkeyv(input_dt, id_cols)
input_dt[location == "Iran (Islamic Republic of)", value := value * 2]

# set up expected output table with all unique locations
expected_dt <- copy(input_dt)
expected_dt[location != "Iran (Islamic Republic of)", value := value * 2]
setkeyv(expected_dt, id_cols)

description <- "scaling of Iran data with national value doubled and historical
locations included works"
test_that(description, {
  output_dt <- scale(dt = input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "location",
                     col_type = "categorical",
                     mapping = iran_mapping)
  expect_identical(output_dt, expected_dt)
})

description <- "scaling of Iran data with national value doubled and historical
locations not included works when `collapse_missing` argument is set to TRUE and
throws an error otherwise"
test_that(description, {
  new_input_dt <- input_dt[!grepl("[0-9]+", location)]
  new_expected_dt <- expected_dt[!grepl("[0-9]+", location)]

  expect_error(scale(dt = new_input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "location",
                     col_type = "categorical",
                     mapping = iran_mapping),
               regexp = "input data is missing in `dt`")

  output_dt <- scale(dt = new_input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "location",
                     col_type = "categorical",
                     mapping = iran_mapping,
                     collapse_missing = TRUE)
  expect_identical(output_dt, new_expected_dt)
})

description <- "scaling of Iran data with national (root) value missing throws
an error"
test_that(description, {
  new_input_dt <- input_dt[!location %in% c("Iran (Islamic Republic of)")]
  expect_error(scale(dt = new_input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "location",
                     col_type = "categorical",
                     mapping = iran_mapping),
               regexp = "input data is missing in `dt`")
  expect_error(scale(dt = new_input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "location",
                     col_type = "categorical",
                     mapping = iran_mapping,
                     collapse_missing = TRUE),
               regexp = "input data is missing in `dt`")
})
