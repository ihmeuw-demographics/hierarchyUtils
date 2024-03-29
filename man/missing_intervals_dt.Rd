% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interval_assertions.R
\name{assert_no_missing_intervals_dt}
\alias{assert_no_missing_intervals_dt}
\alias{identify_missing_intervals_dt}
\title{Check if the interval column in a data.table missing any expected intervals}
\usage{
assert_no_missing_intervals_dt(
  dt,
  id_cols,
  col_stem,
  expected_ints_dt,
  quiet = FALSE
)

identify_missing_intervals_dt(
  dt,
  id_cols,
  col_stem,
  expected_ints_dt,
  quiet = FALSE
)
}
\arguments{
\item{dt}{[\code{data.table()}]\cr
Data containing the interval variable to check. Should include all 'id_cols'.}

\item{id_cols}{[\code{character()}]\cr
ID columns that uniquely identify each row of \code{dt}. Should include
'{col_stem}_start' and '{col_stem}_end'.}

\item{col_stem}{[\code{character(1)}]\cr
The name of the interval variable to check, should not include the
'_start' or '_end' suffix.}

\item{expected_ints_dt}{[\code{data.table()}]\cr
The expected intervals that should be completely included in \code{ints_dt}.
Should include only '{col_stem}_start' and '{col_stem}_end' columns. Can
also be \code{NULL} in which case \code{expected_ints_dt} will automatically be set
to the minimum and maximum of each unique set of intervals in \code{dt}.}

\item{quiet}{[\code{logical(1)}]\cr
Should progress messages be suppressed as the function is run? Default is
False.}
}
\value{
\code{identify_missing_intervals_dt} returns a [\code{data.table()}] with
\code{id_cols} that are missing expected intervals. If no intervals are missing
then a zero-row [\code{data.table()}] is returned.
\code{assert_no_missing_intervals_dt} returns nothing but throws an error if
\code{identify_missing_intervals} returns a non-empty data.table.
}
\description{
Checks to see if the specified interval variable is missing any
expected intervals.
}
\details{
\code{identify_missing_intervals_dt} works by first identifying each unique set of
intervals in \code{dt}. Then checks one at a time the groups of rows of \code{dt}
that match each set of intervals.

\code{expected_ints_dt = NULL} will automatically check that there are no missing
intervals between the minimum and maximum interval in each unique set. This
may miss identifying missing intervals at the beginning or end of the range.
}
\examples{
input_dt <- data.table::data.table(
  year = c(rep(2010, 20), rep(2015, 96)),
  age_start = c(seq(0, 95, 5), seq(0, 95, 1)),
  age_end = c(seq(5, 95, 5), Inf, seq(1, 95, 1), Inf),
  value = 1
)
input_dt <- input_dt[!age_start \%in\% c(0, 10, 95)]

# expect intervals to cover the entire 0-Inf range
missing_dt <- identify_missing_intervals_dt(
  dt = input_dt,
  id_cols = c("year", "age_start", "age_end"),
  col_stem = "age",
  expected_ints_dt = data.table::data.table(age_start = 0, age_end = Inf)
)

# expect intervals to cover between the minimum and maximum of each grouping
missing_dt <- identify_missing_intervals_dt(
  dt = input_dt,
  id_cols = c("year", "age_start", "age_end"),
  col_stem = "age",
  expected_ints_dt = NULL
)

}
