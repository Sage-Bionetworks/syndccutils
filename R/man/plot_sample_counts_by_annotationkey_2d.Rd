% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/charts.R
\name{plot_sample_counts_by_annotationkey_2d}
\alias{plot_sample_counts_by_annotationkey_2d}
\title{Plot the breakdown of samples in a file view based on distinct values
within two specified annotation keys.}
\usage{
plot_sample_counts_by_annotationkey_2d(view_df,
  sample_key = c("individualID", "specimenID", "cellLine"),
  annotation_keys, filter_missing = TRUE)
}
\arguments{
\item{view_df}{File view data frame}

\item{sample_key}{string indicating sample type}

\item{annotation_keys}{Annotation keys to group by}

\item{filter_missing}{remove records with missing annotation values}
}
\description{
Plot the breakdown of samples in a file view based on distinct values
within two specified annotation keys.
}
\examples{
\dontrun{
plot_keys <- list(assay = "Assay", tumorType = "Tumor Type")
plot_sample_counts_by_annotationkey_2d(fileview_df, "cellLine", plot_keys)
}
}
