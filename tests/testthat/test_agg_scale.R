library(data.table)
library(testthat)

# Aggregate to both sexes combined ----------------------------------------

sex_mapping <- data.table(parent = c("female", "male"),
                          child = "both")
id_cols <- c("year", "sex")
value_cols <- c("value1", "value2")

# set up test input data.table
input_dt <- CJ(year = 2010, sex = c("female", "male"),
               value1 = 1, value2 = 2)
setkeyv(input_dt, id_cols)

# set up expected output table
expected_dt <- CJ(year = 2010, sex = "both",
                  value1 = 2, value2 = 4)
setkeyv(expected_dt, id_cols)

test_that("aggregation to boths sexes combined works", {
  output_dt <- agg(dt = input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "sex",
                   col_type = "categorical",
                   mapping = sex_mapping)
  expect_identical(output_dt, expected_dt)
})

description <- "error is thrown when aggregate is already included in input"
test_that(description, {
  new_input_dt <- rbind(input_dt, expected_dt)
  expect_error(agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "sex",
                   col_type = "categorical",
                   mapping = sex_mapping))
})

# Scale additive sex-specific values to both sexes combined ---------------

# set up test input data.table
input_dt_both <- CJ(year = 2010, sex = "both",
                    value1 = 3, value2 = 5)
input_dt <- rbind(input_dt, input_dt_both, use.names = T)
setkeyv(input_dt, id_cols)

# set up expected output table
expected_dt <- CJ(year = 2010, sex = c("female", "male"),
                  value1 = 1.5, value2 = 2.5)
expected_dt <- rbind(expected_dt, input_dt_both, use.names = T)
setkeyv(expected_dt, id_cols)

description <- "scaling additive sex-specific values to both sexes combined
works"
test_that(description, {
  output_dt <- scale(dt = input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "sex",
                     col_type = "categorical",
                     mapping = sex_mapping)
  expect_identical(output_dt, expected_dt)
})

# Scale multiplicative sex-specific values to both sexes combined ---------

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
                   missing_dt_severity = "none"))
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
                   missing_dt_severity = "stop"))
  expect_warning(agg(dt = new_input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "location",
                     col_type = "categorical",
                     mapping = iran_mapping,
                     missing_dt_severity = "warning"))
  expect_message(agg(dt = new_input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "location",
                     col_type = "categorical",
                     mapping = iran_mapping,
                     missing_dt_severity = "message"))
  expect_silent(agg(dt = new_input_dt,
                    id_cols = id_cols,
                    value_cols = value_cols,
                    col_stem = "location",
                    col_type = "categorical",
                    mapping = iran_mapping,
                    missing_dt_severity = "none"))

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
                   mapping = iran_mapping))

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
                     mapping = iran_mapping))

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
                     mapping = iran_mapping))
  expect_error(scale(dt = new_input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "location",
                     col_type = "categorical",
                     mapping = iran_mapping,
                     collapse_missing = TRUE))
})

# Aggregate over age intervals --------------------------------------------

id_cols <- c("year", "age_start", "age_end")
value_cols <- c("value1", "value2")

# set up test input data.table
input_dt1 <- data.table(year = 2010,
                        age_start = seq(0, 95, 1),
                        value1 = 1, value2 = 2)
input_dt2 <- data.table(year = 2011,
                        age_start = seq(0, 95, 5),
                        value1 = 1, value2= 2)
input_dt <- rbind(input_dt1, input_dt2, use.names = T)
gen_end(input_dt, id_cols = setdiff(id_cols, "age_end"), col_stem = "age")
setkeyv(input_dt, id_cols)

# set up mapping for aggregation over age
age_mapping <- data.table(age_start = c(0, 0, 15, 40, 85), age_end = c(Inf, 5, 60, 70, Inf))

# set up expected output data.table
# expected value is number of single year age groups in aggregate interval
expected_dt1 <- copy(age_mapping)
expected_dt1[, year := 2010]
expected_dt1[, value1 := c(96, 5, 45, 30, 11)]
expected_dt1[, value2 := value1 * 2]
setcolorder(expected_dt1, colnames(input_dt))

# expected value is number of five year age groups in aggregate interval
expected_dt2 <- copy(age_mapping)
expected_dt2[, year := 2011]
expected_dt2[, value1 := c(20, 1, 9, 6, 3)]
expected_dt2[, value2 := value1 * 2]

expected_dt <- rbind(expected_dt1, expected_dt2, use.names = T)
setcolorder(expected_dt2, c(id_cols, value_cols))
setkeyv(expected_dt, id_cols)

testthat::test_that("`aggregating age intervals works", {
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
                   missing_dt_severity = "stop"))
  expect_warning(agg(dt = new_input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "age",
                     col_type = "interval",
                     mapping = age_mapping,
                     missing_dt_severity = "warning"))
  expect_message(agg(dt = new_input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "age",
                     col_type = "interval",
                     mapping = age_mapping,
                     missing_dt_severity = "message"))
  expect_silent(agg(dt = new_input_dt,
                    id_cols = id_cols,
                    value_cols = value_cols,
                    col_stem = "age",
                    col_type = "interval",
                    mapping = age_mapping,
                    missing_dt_severity = "none"))

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
  new_input_dt <- rbind(input_dt, expected_dt[!(year == 2011 & age_start == 0)])
  setkeyv(new_input_dt, id_cols)

  setkeyv(expected_dt, id_cols)

  expect_error(agg(dt = new_input_dt,
                   id_cols = id_cols,
                   value_cols = value_cols,
                   col_stem = "age",
                   col_type = "interval",
                   mapping = age_mapping))
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

id_cols <- c("year", "age_start", "age_end")
value_cols <- "value"

# set up test input data.table
input_dt_detailed <- data.table(year = 2010,
                                age_start = seq(0, 95, 1),
                                value = 1)
gen_end(input_dt_detailed, id_cols = setdiff(id_cols, "age_end"), col_stem = "age")
# this is present below
input_dt_detailed <- input_dt_detailed[!is.infinite(age_end)]

input_dt_agg1 <- data.table(year = 2010,
                            age_start = seq(0, 95, 5),
                            value = 10)
gen_end(input_dt_agg1, id_cols = "age_start", col_stem = "age")
input_dt_agg1[is.infinite(age_end), value := 2]

input_dt_agg2 <- data.table(year = 2010, age_start = 0, value = 384)
gen_end(input_dt_agg2, id_cols = "age_start", col_stem = "age")

input_dt <- rbind(input_dt_detailed, input_dt_agg1, input_dt_agg2,
                  use.names = T)
setkeyv(input_dt, id_cols)

expected_dt_detailed <- copy(input_dt_detailed)
expected_dt_detailed[, value := value * 4]

expected_dt_agg1 <- copy(input_dt_agg1)
expected_dt_agg1[, value := value * 2]

expected_dt <- rbind(expected_dt_detailed, expected_dt_agg1, input_dt_agg2,
                     use.names = T)
setkeyv(expected_dt, id_cols)

testthat::test_that("`scaling age intervals works", {
  output_dt <- scale(dt = input_dt,
                     id_cols = id_cols,
                     value_cols = value_cols,
                     col_stem = "age",
                     col_type = "interval")
  testthat::expect_equal(output_dt, expected_dt)
})

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
                             mapping = data.table(age_start = 0, age_end = 10)))
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
                               col_type = "interval"))
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
                               col_type = "interval"))
})
