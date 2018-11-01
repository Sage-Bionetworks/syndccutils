#!/usr/bin/env Rscript

usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, repos = "http://cran.us.r-project.org", dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("pacman")

suppressPackageStartupMessages(p_load("GEOquery"))
suppressPackageStartupMessages(p_load("SRAdb"))
suppressPackageStartupMessages(p_load("plyr"))
suppressPackageStartupMessages(p_load("optparse"))

option_list <- list(
                    make_option(c("--gse"), action="store",
                                default=NULL,
                                help="GEO Series accession number (e.g., \"GSE89777\")"),
                    make_option(c("--output-file"), action="store",
                                default="metadata.tsv",
                                help="File to store the metadata in TSV format [default: %default%"))

descr <- "\
Extract GEO annotations from the GEO data set with identifier specified by 'GSE'.  If the data set has SRA entries, the ftp directory is expected to be specified in GEO annotation fields that include the pattern 'supplementary_file'.  This field will be parsed to extract the SRA identifier, which will be used to determine the URL of the FTP file.  Whether an SRA data set or not, the URL will be included in the 'url' column of the output.
"

parser <- OptionParser(usage = "%prog [options]", option_list=option_list, description=descr)

arguments <- parse_args(parser, positional_arguments = TRUE)
opt <- arguments$options

## Collect input parameters
gse.identifier <- opt$gse
output.file <- opt$`output-file`

if ( length(arguments$args) != 0 ) {
  print_help(parser)
  q(status=1)
}

if ( is.null(gse.identifier) ) {
  print_help(parser)
  q(status=1)
}

options('download.file.method.GEOquery' = 'libcurl')

## Get the GSE object
message(paste0("Retrieving the GSE object for identifier: ", gse.identifier, "\n"))
gse.geo <- getGEO(gse.identifier, getGPL=FALSE)

## Iterate over each of the GSMs associated with this GSE
metadata.tbl <- ldply(sampleNames(phenoData(gse.geo[[1]])), 
                      .fun = function(gsm.identifier) {

                        message(paste0("Retrieving metadata for GSM identifier: ", gsm.identifier, "\n"))
                          
                        ## Get the GSM object
                        gsm.geo <- getGEO(gsm.identifier)
        
                        ## Extract its metadata
                        md <- Meta(gsm.geo)
                        
                        ## Metadata is a list of lists.
                        ## Go through each entry and concatenate the individual lists using a ';' delimiter
                        as.data.frame(lapply(md, function(entry) paste(entry, collapse=";")))
                      })

## If these are SRA entries, the entity listed in supplementary_file will be a directory
## not a URL.  Do a little more work to find that URL.
supp.file.columns <- colnames(metadata.tbl)[grepl(colnames(metadata.tbl), pattern="supplementary_file")]
if(length(supp.file.columns) == 0) {
    stop(paste0("Could not find a column name with pattern \"supplementary_file\" in columns:\n", paste(colnames(metadata.tbl), collapse=", "), "\n"))
}

if(length(supp.file.columns) > 1) {
    warning(paste0("Got multiple columns with pattern \"supplementary_file\"\nJust using the first of the following:\n", paste(supp.file.columns, collapse=", "), "\n"))
}

metadata.tbl$url <- as.character(metadata.tbl[, supp.file.columns[1]])

cat("Outputting metadata file before resolving URLs to FTP sites\n")
write.table(file = output.file, metadata.tbl, sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)

if(any(metadata.tbl$type == "SRA")) {
  sra.db.dest.file <- "SRAmetadb.sqlite"
  if(!file.exists(sra.db.dest.file)) {
      url <- "https://starbuck1.s3.amazonaws.com/sradb/SRAmetadb.sqlite.gz"
      gzip.file <- paste0(sra.db.dest.file, ".gz")
      download.file(url = url, destfile = gzip.file, mode = "wb", method = "libcurl")
      system(paste0("gunzip ", gzip.file))
    ## sra.db.dest.file <- getSRAdbFile(destfile = paste0(sra.db.dest.file, ".gz"))
  }
  con <- dbConnect(RSQLite::SQLite(), sra.db.dest.file)

  if(!("relation" %in% colnames(metadata.tbl))) {
      stop("Was expecting to parse SRA identifier from relation column, but no such column exists\n")
  }

  ## Overwrite the url field (extracted from the supplementary file above) with a url to the raw
  ## data accessible through the SRA identifier--if there is one.
  new.tbl <- ldply(1:nrow(metadata.tbl),
                   .fun = function(i) {
                       df <- data.frame(metadata.tbl[i, ])
                       rownames(df) <- NULL
                       if(metadata.tbl$type[i] == "SRA") {
                           ## Extract the SRA identifier
                           ## NB: this is probably also in the relation column
                           ## We once tried to extract this from the URL, but it was too
                           ## simplistic.  
                           ## url <- metadata.tbl$url[i]
                           ## sra.identifier <- gsub("^.*\\/([^\\/]+)$", "\\1", url)
                           ## Instead, extract from the relation field
                           relation.str <- as.character(metadata.tbl$relation[i])
                           relations <- unlist(strsplit(x = relation.str, split=";[ ]*"))
                           flag <- grepl(relations, pattern="SRA")
                           if(!(length(which(flag) == 1))) {
                               cat(paste0("Could not find SRA in relation string: ", relation.str, "\n"))
                           } else  {
                               sra.relation <- relations[flag]
                               if(!grepl(sra.relation, pattern = "term=")) {
                                   stop(paste0("Could not parse term= in relation string: ", sra.relation, "\n"))
                               }
                               sra.identifier <- gsub("^.*term=([^\\/]+)$", "\\1", sra.relation)
                               cat(paste0("Parsed SRA identifier ", sra.identifier, " from ", relation.str, "\n"))
                               url <- listSRAfile(sra.identifier, con)$ftp
                               if(length(url) > 0) {
                                   df <- data.frame(metadata.tbl[rep(i, length(url)), ])
                                   df$url <- url
                                   rownames(df) <- NULL
                               }
                           }
                       }
                       df
                   })
  metadata.tbl <- new.tbl
  
  d <- dbDisconnect(con)
}

cat("Outputting metadata file after resolving URLs to FTP sites\n")
write.table(file = output.file, metadata.tbl, sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)

message("Successfully completed\n")
q(status = 0)
