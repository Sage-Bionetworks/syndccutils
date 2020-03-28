#!/usr/bin/env Rscript

##########################################################
####  Link GEO datasets associated with a publication ####
####  into synapse and update dataset portal view to  ####
####  include those datasets.                         ####
##########################################################

## Sample usage:
## $ Rscript link-publication-geo-datasets.R \
##              --publication-table-synId "syn10923842" \
##              --dataset-view-synId "syn18488466" \
##              --grant-table-synId "syn10142562"

## Load packages
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, repos = "http://cran.us.r-project.org", dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("pacman")

suppressPackageStartupMessages(p_load("plyr"))
suppressPackageStartupMessages(p_load("optparse"))
suppressPackageStartupMessages(p_load("synapser"))

option_list <- list(
                    make_option(c("--publication-table-synId"), action="store",
                                default=NULL,
                                help="Synapse ID of table or view holding portal publications."),
                    make_option(c("--grant-table-synId"), action="store",
                                default=NULL,
                                help="Synapse ID of table or view holding portal grants."),
                    make_option(c("--dataset-view-synId"), action="store",
                                default=NULL,
                                help="Synapse ID of table or view holding portal datasets. See description regarding table versus view."),
                    make_option(c("--dataset-id-column"), action="store",
                                default="datasetId",
                                help="Name of column in dataset table or view holding the identifier of the corresponding dataset (default: %default)"),
                    make_option(c("--publication-table-geo-id-column"), action="store",
                                default="GSE",
                                help="Name of column in publication table or view holding the comma-separated list of any GEO identifiers (i.e., GSEXXXX) of any of associated datasets (default: %default)"),
                    make_option(c("--grant-id-column"), action="store",
                                default="grantId",
                                help="Name of column in all tables / views (publication, dataset, and grant) holding the Synapse ID of the Synapse Project representing the grant associated with the publication. The dataset will be stored in the /datasets folder under this Project (default: %default)"),
                    make_option(c("--grant-name-column"), action="store",
                                default="grantName",
                                help="Name of column in all tables / views (publication, dataset, and grant) holding the grant name (default: %default)")
				
)



descr <- "\
For each publication listed in the publication table / view, link any associated GEO datasets into Synapse. Only do so if the dataset is not already included in the dataset table / view. This script does not update the dataset table / view _explicitly_. However, it does create a folder for the dataset under the /datasets folder of each grant's Synapse project. If that dataset entries are in a view and if that /datasets folder is included in the scope of that view, then the dataset will automatically be added to the dataset view. Files are linked with a minimum of annotations -- currently only the 'title' GEO annotation.
"

parser <- OptionParser(usage = "%prog [options]", option_list=option_list, description=descr)

arguments <- parse_args(parser, positional_arguments = TRUE)
opt <- arguments$options

## Collect input parameters
pub.table.synId <- opt$`publication-table-synId`
grant.table.synId <- opt$`grant-table-synId`
dataset.view.synId <- opt$`dataset-view-synId`
dataset.id.col <- opt$`dataset-id-column`
pub.table.geo.id.col <- opt$`publication-table-geo-id-column`
grant.id.col <- opt$`grant-id-column`
grant.name.col <- opt$`grant-name-column`

synLogin()

if ( length(arguments$args) != 0 ) {
  print_help(parser)
  q(status=1)
}

if ( any(is.null(c(pub.table.synId, dataset.view.synId, grant.table.synId))) ) {
  print_help(parser)
  q(status=1)
}

pub.table.synId <- opt$`publication-table-synId`
grant.table.synId <- opt$`grant-table-synId`
dataset.view.synId <- opt$`dataset-view-synId`


## Download the publication and dataset tables / views
pub.tbl <- synTableQuery(paste0("SELECT * FROM ", pub.table.synId))$asDataFrame()

grant.tbl <- synTableQuery(paste0("SELECT * FROM ", grant.table.synId))$asDataFrame()
grant.tbl <- na.omit(unique(grant.tbl[, c(grant.id.col, grant.name.col)]))
rownames(grant.tbl) <- grant.tbl[, grant.id.col]

dataset.tbl <- synTableQuery(paste0("SELECT * FROM ", dataset.view.synId))$asDataFrame()

get.synapse.id.of.folder.content_ <- function(folder.synId, file.or.dir.name) {
  children <- synGetChildren(folder.synId)
  children <- as.list(children)
  for(kid in children) {
    if(kid$name == file.or.dir.name) { return(kid$id) }
  }
  return(NULL)
}

## Return the synId of folder.name under Synapse ID parentId or create it if it doesn't exist.
get.folder.synId <- function(folder.name, parentId) {
  synId <- get.synapse.id.of.folder.content_(parentId, folder.name)
  if(!is.null(synId)) { return(synId) }
  folder <- Folder(folder.name, parent=parentId)
  obj <- synStore(folder)
  synId <- get.synapse.id(obj)
  synId
}

get.synapse.id <- function(obj) {
  obj$properties$id
}

## Iterate over each publication in the table
l_ply(1:nrow(pub.tbl), .parallel = FALSE,
      .fun = function(i) {
               ## Get the GEO datasets associated with this publication
	       gses <- as.character(pub.tbl[i, pub.table.geo.id.col])
	       if(is.null(gses) || is.na(gses)) { return() }
	       gses <- unlist(strsplit(gses, split=",[ ]*"))
	       gses <- unlist(strsplit(gses, split=";[ ]*"))
	       if(length(gses) == 0) { return() }
	       message(paste0("Publication ", i, " processing GSES: ",
	                  paste(gses, collapse = ", "), "\n"))

               names(gses) <- gses

               ## Extract the Synapse ID of the Project associated with this
	       ## publication's grant. NB: there could be multiple grants associated
	       ## with this publication. That's OK -- just take the first. We just
	       ## need a place to store this dataset -- it's actual location will
	       ## be tracked in the datasets table and we will be able to find it
	       ## there based on its datasetId, regardless of where it is stored.
	       grant.synId <- as.character(pub.tbl[i, grant.id.col])
	       grant.synId <- unlist(strsplit(grant.synId, split=",[ ]*"))
	       grant.synId <- unlist(strsplit(grant.synId, split=";[ ]*"))
	       all.grant.synIds <- grant.synId
	       grant.synId <- grant.synId[1]
	       all.grant.names <-
	         unlist(llply(all.grant.synIds,
		              .fun = function(synId) {
			               grant.name <- NULL
	                               if(synId %in% rownames(grant.tbl)) {
                                         grant.name <- as.character(grant.tbl[synId, grant.name.col])
	                               }
				       return(grant.name)
				     }))
               all.grant.names <- paste0(all.grant.names, collapse = ", ")
	       all.grant.synIds <- paste0(all.grant.synIds, collapse = ", ")

               force.dataset.processing <- TRUE
	       force.geo.annotation <- FALSE

               ## Iterate over each GEO dataset associated with this publication
	       l_ply(gses,
	             .fun = function(gse) {

                              ## Has this GEO dataset already been linked?
			      ## i.e., does it exist in the dataset table?
                              if(!force.dataset.processing) {
			        if(gse %in% dataset.tbl[, dataset.id.col]) {
			          message(paste0(gse, " already exists in dataset table. Skipping.\n"))
				  return()
				}
			      }
			      message(paste0("Adding ", gse, " to dataset table\n"))

                              ## Get the GEO annotations for this dataset
			      anno.file <- paste0(gse, "-metadata.tsv")
			      if(!file.exists(anno.file) || force.geo.annotation) {
  			        cmd <- paste0("Rscript ./get-geo-annotations.R --gse=", gse, " > ", anno.file)
			        system(cmd)
		              }
			      if(!file.exists(anno.file) || ( file.size(anno.file) == 0)) {
			        message(paste0("Could not download annotations for GEO dataset ", gse, "\n"))
				return()
			      }
			      anno.tbl <- read.table(anno.file, sep = "\t", header = TRUE, as.is = TRUE, stringsAsFactors = FALSE, comment.char = "", quote = "\"")
			      if(nrow(anno.tbl) == 0) {
			        message(paste0("Annotation table for GEO dataset ", gse, " had no rows!\n"))
				return()
			      }
			      datasets.synId <- get.folder.synId("datasets", parentId = grant.synId)
			      geo.dataset.synId <- get.folder.synId(gse, parentId = datasets.synId)
			      ## Set some annotations on the dataset folder / object
			      dataset.obj <- synGet(geo.dataset.synId, downloadFile = FALSE)
                              annos <- synGetAnnotations(dataset.obj)
			      annos[["datasetName"]] <- gse
			      annos[["datasetId"]] <- gse
			      annos[["datasets"]] <- gse
			      annos[["grantId"]] <- all.grant.synIds
			      annos[["grantName"]] <- all.grant.names
			      annos[["is.dataset"]] <- "TRUE"
			      synSetAnnotations(dataset.obj, annotations=annos)
			      

			      ## Iterate over each row / sample in the annotation table
			      file.cols <- c("url", "sra.url")
			      file.cols <- file.cols[file.cols %in% colnames(anno.tbl)]
			      if(length(file.cols) == 0) { return() }

                              l_ply(1:nrow(anno.tbl), .parallel = FALSE,
			            .fun = function(j) {
				             l_ply(file.cols, .parallel = FALSE,
					           .fun = function(file.col) {
				                            files <- as.character(anno.tbl[j, file.col])
							    files <- unlist(strsplit(files, split = ",[ ]*"))
							    for(file in files) {
  							      if(is.null(file)) { return() }
							      if(is.na(file)) { return() }
							      if(file == "NONE") { return() }
							      if(!grepl(file, pattern="ftp")) { return() }
							      message(paste0("Linking ", file, " to folder /datasets/", gse, " (synId = ", geo.dataset.synId, ")\n"))
				                              f <- File(file, parentId = geo.dataset.synId, synapseStore = FALSE)
					                      obj <- synStore(f)
					                      synId <- get.synapse.id(obj)
							      splits <- unlist(strsplit(file, split="/"))
							      fileName = splits[length(splits)]

                                                              ## Add a few annotations to the link
                                                              annos <- synGetAnnotations(obj)
                                                              annos[["fileName"]] <- fileName
  			                                      annos[["datasetName"]] <- gse
			                                      annos[["datasetId"]] <- gse
			                                      annos[["datasets"]] <- gse

			                                      annos[["grantId"]] <- all.grant.synIds
			                                      annos[["grantName"]] <- all.grant.names
							      anno.cols <- list("instrument_model" = "platform", "title" = "title", "geo_accession" = "geo_accession")
							      for(anno.col in names(anno.cols)) {
							        if(!(anno.col %in% colnames(anno.tbl))) { next }
							        annos[[anno.cols[[anno.col]]]] <- anno.tbl[j, anno.col]
							      }
			                                      synSetAnnotations(obj, annotations=annos)
							    }
							  })
					   })
			      
                            })
             }) 

cat("Exiting succesfully\n")
q(status = 0)
