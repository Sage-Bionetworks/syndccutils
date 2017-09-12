##here is a short script that tries to test things

require(synapseClient)
synapseLogin()

source('./synapse_helpers.R')
source("./tables.R")
source('./charts.R')

syn_id='syn9630847'
fv_df<-get_table_df(syn_id)

proj_info=summarize_project_info(fv_df)
project_stats=summarize_project_datafile_counts(fv_df, proj_info,syn_id)
wiki_markdown <- table_as_wiki(project_stats,columnsAsCode=c(12))

p<-plot_project_file_counts_by_center(project_stats)


#next up, write code to save files and embed html in wiki
assay_stats <- summarize_assay_stats(fv_df,syn_id)
wiki_markdown <- table_as_wiki(assay_stats,columnsAsCode=c(9))
p<- plot_assay_stats_by_disease(assay_stats)


