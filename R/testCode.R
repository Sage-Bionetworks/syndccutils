##here is a short script that tries to test things

require(synapseClient)
synapseLogin()

source('./synapse_helpers.R')
source("./tables.R")
source('./charts.R')


fv_df<-get_table_df('syn9630847')

proj_info=summarize_project_info(fv_df)
assay_stats=summarize_project_datafile_counts(fv_df, proj_info)

plot_assay_stats_by_tumortype(assay_stats)


