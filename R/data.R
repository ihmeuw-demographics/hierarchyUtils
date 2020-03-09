#' Iran Provinces Mapping from 1950 to Today
#'
#' @description An example mapping of Iran and its provinces from 1950 to
#' present day. The mapping accounts for province boundary changes over time.
#'
#' @format A data.table with 47 rows and 2 columns:
#'   * parent: the aggregate location.
#'   * child: one of the constituent locations.
#'
#' @details
#' Each row in the data.table represents the relationship between Iran, its
#' historical provinces, and present day provinces.
#'
#' This mapping can be used for aggregating [agg()] or scaling [scale()] values.
#'
#' @seealso [create_agg_tree()], [create_scale_tree()], [vis_tree()]
"iran_mapping"
