library(tidyverse)
library(rvest)
library(httr)


# functions ---------------------------------------------------------------

# get script lines
get_script_lines <- function(html_path) {
    read_html(html_path) %>%
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
    read_html(html_path) %>%
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
# example code for CDN lookup ---------------------------------------------

# script_lines <- get_script_lines(mock_chart_filename)

# script_data <- script_lines %>%
#     mutate(src_parts = map(target_attr, parse_js_src)) %>%
#     unnest(src_parts)

# script_data %>%
#     mutate(match = map2(target_file, target_version, function(x, y) {cdn_search(x, y)})) %>%
#     unnest(match)

# GitHub path replacement
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

# CDN path replacement
path_replace_cdn <- function(path,
                             repo = "https://cdn-www.synapse.org",
                             folder = "research") {

    public_assets <- list(
        "plotlyjs-1.29.2/plotly-latest.min.js" = "https://cdn.plot.ly/plotly-1.29.2.min.js",
        "jquery-1.11.3/jquery.min.js" = "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js",
        "font-awesome-4.5.0/css/font-awesome.min.css" = "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css",
        "bootstrap-3.3.5/css/bootstrap.min.css" = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
    )

    path_root <- str_c(str_split(path, "/")[[1]][1], "/")
    path_folder <- str_replace(path, path_root, "")

    if (path_folder %in% names(public_assets)) {
        return(public_assets[[path_folder]])
    }

    file.path(repo, folder, path_folder)
}


# replace paths for a set of HTML lines
update_html_lines <- function(html_lines, target_lines) {
    update_target_lines <- target_lines %>%
        rowwise() %>%
        mutate(replacement_attr = path_replace_cdn(target_attr),
               updated_html = walk2(
                   target_attr, replacement_attr, function(x, y) {
                       html_lines <<- str_replace(html_lines, x, y)
                   }
               )
        ) %>%
        ungroup() %>%
        select(-updated_html)
    html_lines
}

# update HTML files to use hosted JS assets
fix_js_assets <- function(html_path) {

    fixed_html_path <- file.path(
        dirname(html_path),
        str_c("fixed_", basename(html_path))
    )
    html_lines <- read_lines(html_path)
    script_lines <- get_script_lines(html_path)
    link_lines <- get_link_lines(html_path)

    html_lines <- html_lines %>%
        update_html_lines(script_lines) %>%
        update_html_lines(link_lines)

    write_lines(html_lines, fixed_html_path)
    fixed_html_path
}
