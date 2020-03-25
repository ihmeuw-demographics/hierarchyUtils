#' @title Iran Census Population Counts
#'
#' @description Publicly available sex and age specific population counts for
#' Iran at the national level and provinces that existed during the census year.
#'
#' @format A data.table with 10764 rows and 9 columns:
#'   * location: name of the location (corresponds to a location in
#'   `iran_mapping`).
#'   * year: year the census was conducted.
#'   * sex: either 'female' or 'male'.
#'   * age_start: start of the age interval in years.
#'   * age_end: end of the age interval in years.
#'   * source_name: name of the source that provided the census data population
#'   count.
#'   * nid: unique identifier for the catalog record in the Global Health Data
#'   Exchange (GHDx) that directly provided the population count.
#'   * underlying_nid: unique identifier for the catalog record in the Global
#'   Health Data Exchange (GHDx) that the source can be attributed to if
#'   provided by a data vendor (like the Demographic Year Book, DYB).
#'   * population: population count for the location-sex-age_group-source.
"iran_pop"

#' @title Iran Provinces Mapping from 1950 to Today
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
