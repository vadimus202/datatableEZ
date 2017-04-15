load <- c("DT")

.onAttach <- function(...) {
    needed <- load[!is_attached(load)]

    if (length(needed) == 0)
        return()

    suppressPackageStartupMessages(
        lapply(needed, library, character.only = TRUE, warn.conflicts = FALSE)
    )
}

is_attached <- function(x) {
    paste0("package:", x) %in% search()
}
