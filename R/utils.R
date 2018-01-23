library(tidyverse)
library(rvest)
library(httr)


# functions ---------------------------------------------------------------

# get script lines
get_script_lines <- function(html_path) {
    read_html(mock_chart_filename) %>%
        html_node("head") %>%
        html_children() %>%
        keep(function(x) {
            html_name(x) %>%
                has_element("script")
        }) %>%
        map_df(function(x) {
            list(
                as.character(x),
                html_attr(x, "src")
            ) %>%
                flatten() %>%
                set_names(c("target", "target_attr"))
        })
}

get_link_lines <- function(html_path) {
    read_html(mock_chart_filename) %>%
        html_node("head") %>%
        html_children() %>%
        keep(function(x) {
            html_name(x) %>%
                has_element("link")
        }) %>%
        map_df(function(x) {
            list(
                as.character(x),
                html_attr(x, "href")
            ) %>%
                flatten() %>%
                set_names(c("target", "target_attr"))
        })
}

# script info parsing functions
is_js_lib <- function(x) {
    str_detect(x, "^([a-z]+\\-)+([0-9]\\.)+")
}

is_js_file <- function(x) {
    str_detect(x, "\\.js$")
}

parse_js_src <- function(src) {
    list(
        target_lib = src %>%
            str_split("/") %>%
            map_chr(function(x) keep(x, is_js_lib)),
        target_file = src %>%
            str_split("/") %>%
            map_chr(function(x) keep(x, is_js_file))
    ) %>%
        as_tibble() %>%
        mutate(target_version = str_extract(target_lib, "([0-9]+\\.*)+$"))
}

# cdn lookup function
cdn_search <- function(js_file, js_version) {
    query <- glue::glue(
        "https://api.cdnjs.com/libraries?search={name}&fields=version",
        name = js_file
    )
    results <- httr::GET(query) %>%
        content("text") %>%
        jsonlite::fromJSON() %>%
        .$results
    if (length(results)) {
        results %>%
            filter(version == js_version) %>%
            as_tibble()
    } else {
        tibble(name = character(), latest = character(), version = character())
    }

}

path_replace_gh <- function(path,
                            repo = "Sage-Bionetworks/js-host-test",
                            folder = "inst/www") {
    gh_base <- glue::glue(
        "https://raw.githubusercontent.com/{repo}/master/{folder}",
        repo = repo,
        folder = folder
    )
    path_root <- str_c(str_split(path, "/")[[1]][1], "/")
    path_folder <- str_replace(path, path_root, "")
    file.path(gh_base, path_folder)
}


# test scripts ------------------------------------------------------------

script_lines <- get_script_lines(mock_chart_filename)

# script_data <- script_lines %>%
#     mutate(src_parts = map(target_attr, parse_js_src)) %>%
#     unnest(src_parts)

# script_data %>%
#     mutate(match = map2(target_file, target_version, function(x, y) {cdn_search(x, y)})) %>%
#     unnest(match)

script_lines %>%
    mutate(replacement_attr = path_replace_gh(target_attr)) %>%
    select(replacement_attr)

# test links --------------------------------------------------------------

link_lines <- get_link_lines(mock_chart_filename)

link_lines %>%
    mutate(replacement_attr = path_replace_gh(target_attr)) %>%
    select(replacement_attr)

# test html edit ----------------------------------------------------------

html_lines <- read_lines(mock_chart_filename)
update_script_lines <- script_lines %>%
    mutate(replacement_attr = path_replace_gh(target_attr),
           fixed_html = walk2(target_attr, replacement_attr, function(x, y) {
               html_lines <<- str_replace(html_lines, x, y)
           })) %>%
    select(-fixed_html)
update_link_lines <- link_lines %>%
    mutate(replacement_attr = path_replace_gh(target_attr),
           fixed_html = walk2(target_attr, replacement_attr, function(x, y) {
               html_lines <<- str_replace(html_lines, x, y)
           })) %>%
    select(-fixed_html)

new_chart_filename <- str_c("fixed_", mock_chart_filename)
write_lines(html_lines, new_chart_filename)
