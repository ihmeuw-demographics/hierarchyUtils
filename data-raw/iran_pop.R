library(data.table)
library(demInternal) # internal IHME package

historical_loc_fpath <- ""

# load gbd sex mapping
sex_mapping <- data.table(sex_id = 1:3, sex = c("male", "female", "both"))
# load gbd age mapping
age_mapping <- demInternal::get_age_map(
  type = "all",
  gbd_year = 2019
)

# load gbd location mappings
gbd_location_mapping <- demInternal::get_locations(
  location_set_name = "population computation",
  gbd_year = 2019
)
gbd_location_mapping <- gbd_location_mapping[grepl("IRN", ihme_loc_id)]
gbd_historical_location_mapping <- fread(historical_loc_fpath)

# identify IRN national, present day provinces, and historical provinces
irn_location_ids <- unique(c(gbd_location_mapping$location_id,
                             gbd_historical_location_mapping$parent_location_id,
                             gbd_historical_location_mapping$child_location_id))
# get official gbd location names for IRN locations
location_ids <- demInternal::get_locations(location_set_name = "all")
location_ids <- location_ids[location_id %in% irn_location_ids,
                             list(location_id, location = location_name)]

# load GBD census data and subset to publicly available data
iran_pop <- demInternal::get_dem_outputs(
  process_name = "census raw data",
  gbd_year = 2019,
  location_ids = irn_location_ids
)
iran_pop[source_name == "Statistical Center of Iran",
         source_name := "Statistical Centre of Iran"]
iran_pop <- iran_pop[source_name %in% c("DYB", "Statistical Centre of Iran")]
iran_pop <- iran_pop[!year_id %in% c(1991, 1994)] # not actually a census year

# merge on location, sex, age_start, age_end
iran_pop <-merge(iran_pop, sex_mapping, by = "sex_id", all.x = T)
iran_pop <-merge(iran_pop, age_mapping, by = "age_group_id", all.x = T)
iran_pop <-merge(iran_pop, location_ids, by = "location_id", all.x = T)

# format output data
iran_pop <- iran_pop[, list(location, year = year_id, sex,
                            age_start, age_end, source_name,
                            nid, underlying_nid, population = mean)]
setkeyv(iran_pop, setdiff(names(iran_pop), "mean"))

# fix underlying data
iran_pop <- iran_pop[year == 2006 & location == "Tehran 1986-1995",
                     location := "Tehran 2006"]

usethis::use_data(iran_pop, overwrite = TRUE)
