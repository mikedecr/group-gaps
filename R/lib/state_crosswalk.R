box::use(memoise[memoize])

state_name_abb_lookup <- memoize(function() {
    state_mapper <- stats::setNames(datasets::state.abb, datasets::state.name)
    dc_mapper <- c("District of Columbia" = "DC")
    c(state_mapper, dc_mapper)
})

state_name_to_abb <- function(name) {
    mapper <- state_name_abb_lookup()
    mapper[[name]]
}
