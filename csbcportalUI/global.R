# ------------------------------------------------------------------------------
# Shiny template - CSBC Summary Report Analytics
# ------------------------------------------------------------------------------
usePackage <- function(p)
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage('lubridate')
usePackage('tidyverse')
usePackage('plotly')
usePackage('crosstalk')
usePackage('shinythemes')
usePackage('DT')
usePackage('streamgraph')
usePackage('dplyr')
usePackage('ggridges')
usePackage('ggthemes')
usePackage('kableExtra')

library(shiny)
library(shinythemes)
# library(flexdashboard)

library(lubridate)
library(tidyverse)
library(plotly)
library(crosstalk)
library(DT)
library(synapser)

library(streamgraph)
library(dplyr)
library(ggridges)
library(ggthemes)
library(kableExtra)

source("../R/synapse_helpers.R")
source("../R/tables.R")
source("../R/charts.R")
# -----------------------------------------------------------------------
synLogin()
# -----------------------------------------------------------------------
csbc_summary_df <- get_table_df("syn11968325", cache = TRUE)

csbc_summary_df <- csbc_summary_df %>%
    mutate_at(.vars = vars(dplyr::matches("(createdOn|modifiedOn)")),
              .funs = funs(lubridate::as_datetime(floor(. / 1000))))

# -----------------------------------------------------------------------
project_vars <- c("projectId", "name_project", "institution", "consortium",
                  "grantNumber", "grantType", "teamMembersProfileId",
                  "teamProfileId", "createdOn_project", "modifiedOn_project",
                  "publication_count", "publication_geodata_produced")
count_vars <- c("fileId", "individualID", "specimenID", "cellLine",
                "assay", "tool")
center_study_summary_df <- csbc_summary_df %>%
    mutate(tool = ifelse(!is.na(softwareLanguage), study, NA)) %>%
    group_by(.dots = c(project_vars, "study")) %>%
    summarise_at(.vars = count_vars,
                 .funs = funs(n_distinct(., na.rm = TRUE))) %>%
    replace_na(list(study = "Not Annotated")) %>%
    mutate(study = ifelse(fileId == 0, NA, study))


# -----------------------------------------------------------------------
center_summary_df <- center_study_summary_df %>%
    group_by(projectId) %>%
    summarise(study = n_distinct(study, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(
        center_study_summary_df %>%
            select(-study) %>%
            group_by(.dots = project_vars) %>%
            summarise_all(sum),
        by = "projectId"
    )

# -----------------------------------------------------------------------
files <- sum(center_study_summary_df$fileId)

samples <- sum(
    sum(center_study_summary_df$individualID),
    sum(center_study_summary_df$cellLine),
    sum(center_study_summary_df$specimenID)
)

pubs <- csbc_summary_df %>%
    group_by(projectId) %>%
    summarise(value = unique(publication_count)) %>%
    pull(value) %>%
    sum()

centers_w_files <- center_summary_df %>%
    group_by(projectId) %>%
    summarize(file_count = sum(fileId)) %>%
    filter(file_count > 0) %>% nrow()
# -----------------------------------------------------------------------
