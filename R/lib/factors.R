# NOTE; this probably doesn't belong here?
# undo fct_cross...

fct_factorize <- function(.f, split=":", names=NULL) {
    var = as.character(.f)
    d = stringr::str_split(var, split, simplify = TRUE) |>
        as.data.frame()
    if (is.null(names) == FALSE) {
        return(stats::setNames(d, names))
    }
    return(d)
}

# # test: 2 x 3 data frame result
# m = fct_factorize(c("1:2:3", "4:5:6"),
#                   split = ":",
#                   names = c("A", "B", "C"))
# test_result = tribble(~A, ~B, ~C,
#                        1,  2,  3,
#                        4,  5,  6)
# stopifnot(all(m == test_result))

