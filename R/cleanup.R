
get_function_names <- function(script) {
    readr::read_lines(script) %>%
        purrr::keep(function(x) {
            stringr::str_detect(x, "^[a-z].*(<-|=) function")
        }) %>%
        stringr::str_split(pattern = " (<-|=) ") %>%
        purrr::map_chr(1)
}

check_used_file <- function(function_name, script, list_hits = FALSE) {
    function_name <- stringr::str_c(function_name, "\\(")
    readr::read_lines(script) %>%
        stringr::str_detect(function_name) %>%
        any()
}

check_used_dir <- function(function_name, dir, list_hits = FALSE) {
    dir_scripts <- fs::dir_ls(dir, glob = "*.R", recursive = TRUE)
    hits <- dir_scripts %>%
        purrr::map_lgl(function(script) {
            check_used_file(function_name, script)
        })
    if (list_hits) {
        hits[hits]
    } else {
        any(hits)
    }
}

find_unused_functions <- function(script, target_dir) {
    get_function_names(script) %>%
        set_names(.) %>%
        purrr::map_lgl(function(function_name) {
            check_used_dir(function_name, target_dir)
        }) %>%
        .[!(.)] %>%
        names()
}


# code check --------------------------------------------------------------

source_scripts <- c("R/tables.R", "R/charts.R", "R/synapse_helpers.R") %>%
    set_names(.)
map(source_scripts, function(source_script) {
    find_unused_functions(source_script, "scripts")
})
