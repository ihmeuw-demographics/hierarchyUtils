% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/categorical_trees.R
\name{check_agg_scale_subtree_dt}
\alias{check_agg_scale_subtree_dt}
\title{Check subtree data.table before aggregation and scaling}
\usage{
check_agg_scale_subtree_dt(
  dt,
  id_cols,
  col_stem,
  col_type,
  missing_dt_severity,
  expected_col_stem
)
}
\arguments{
\item{dt}{[\code{data.table()}]\cr
Data to be aggregated or scaled.}

\item{id_cols}{[\code{character()}]\cr
ID columns that uniquely identify each row of \code{dt}.}

\item{col_stem}{[\code{character(1)}]\cr
The name of the variable to be aggregated or scaled over. If aggregating an
'interval' variable should not include the '_start' or '_end' suffix.}

\item{col_type}{[\code{character(1)}]\cr
The type of variable that is being aggregated or scaled over. Can be either
'categorical' or 'interval'.}

\item{missing_dt_severity}{[\code{character(1)}]\cr
How severe should the consequences of missing data that prevents
aggregation or scaling from occurring be? Can be either 'stop', 'warning',
'message', or 'none'. If not "stop", then only the possible aggregations or
scaling is done using the available data.}

\item{expected_col_stem}{[\code{character()}]\cr
expected values for the \code{col_stem} variable in \code{dt}.}
}
\value{
\code{dt} with any missing data dropped.
}
\description{
Check subtree data.table before aggregation and scaling after
interval id columns have been collapsed if specified. At this point
data.table should be square with all expected values for the \code{col_stem}
variable.
}