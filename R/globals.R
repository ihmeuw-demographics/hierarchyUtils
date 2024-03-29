# this is needed for non-standard evaluation in data.table (and some other packages).
# multiple links suggest using `utils::globalVariables` to remove notes when checking the package.
# https://www.r-bloggers.com/no-visible-binding-for-global-variable/
# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
utils::globalVariables(c("start", "end", "available_vars",
                         "check", "group", "overlap", "issue",
                         "data_exists", "data_expected", "aggregate_exists",
                         "common_start", "common_end",
                         "vis_group", "colour", "full",
                         "N", "na.omit", "unknown", "weight", "include_NA"))
