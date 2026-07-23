suppressPackageStartupMessages(library("optparse"))

option_list <- list(
    make_option(c("--synId"), action="store",
                default=NULL,
                help="Synapse id of existing table that will be replaced by csv"),
    make_option(c("--csv"), action="store",
                default=NULL,
                help="Table that will replace synapse table with synapse id synId"))


descr <- "\
Use 'csv' to replace contents of Synapse table with Synapse ID synId. This uses fread, which
better handles embedded quotation marks than read.table.
e.g., a csv file like
V1,V2
\"1\",\"he calls himself \\\"horse\\\"\"
will have second column entry
he calls himself \"horse\"
"

parser <- OptionParser(usage = "%prog [options]", option_list=option_list, description=descr)

arguments <- parse_args(parser, positional_arguments = TRUE)
opt <- arguments$options

if( is.null(opt$`synId`) || is.null(opt$`csv`) ) {
  print_help(parser)
  q(status=1)
}

library(synapser)

synLogin()

synId <- opt$synId
file <- opt$csv

library(data.table)
csv <- fread(file)
## csv <- read.table(file, sep=",", header=TRUE, stringsAsFactors = FALSE, comment.char = "")
csv <- as.data.frame(csv)
csv <- csv[, !(colnames(csv) %in% c("ROW_ID", "ROW_VERSION"))]


current <- synTableQuery(paste0("SELECT * FROM ", synId))
synDelete(current) # delete current rows
new <- Table(synId, csv)
synStore(new)
