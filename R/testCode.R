##here is a short script that tries to test things

require(synapseClient)
synapseLogin()

source('./synapse_helpers.R')
source("./tables.R")
source('./charts.R')


fv_df<-get_table_df('syn9630847')

proj_info=summarize_project_info(fv_df)
project_stats=summarize_project_datafile_counts(fv_df, proj_info)

p<-plot_project_file_counts_by_center(project_stats)

#next up, write code to save files and embed html in wiki

assay_stats <- summarize_assay_stats(fv_df)
p<- plot_assay_stats_by_disease(assay_stats)


