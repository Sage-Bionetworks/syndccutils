# data from synapse
tool_table <- synTableQuery("SELECT * FROM syn11380099")$asDataFrame()
center_info <- synTableQuery("SELECT id,name FROM syn10142562")$asDataFrame()
center_info <- center_info[c("id","name")]
colnames(center_info) <- c("center","centerName")

tool_table <- merge(tool_table,center_info,by="center",all.x = TRUE)
tool_table$softwareType[is.na(tool_table$softwareType)] <-"other"
tool_table[is.na(tool_table)] <- "N/A"

abstracts_list <- fromJSON(file="tools.json") 

# create lists and search list with methodIDs
input_list <- unique(tool_table$inputDataType)
output_list <- unique(tool_table$outputDataType)
center_list <- unique(tool_table$centerName)

input_search <- split(tool_table$methodID,tool_table$inputDataType)
input_search <- lapply(input_search,function(x){unique(x)})

output_search <- split(tool_table$methodID,tool_table$outputDataType)
output_search <- lapply(output_search,function(x){unique(x)})

center_search <- split(tool_table$methodID,tool_table$centerName)
center_search <- lapply(center_search,function(x){unique(x)})

# re-assemble the table with html 
var_list <- c("methodID","methodName","center","centerName","softwareLanguage","softwareType","synapseSite","PMID","URL")

tool_info <- ddply(tool_table,.variables = var_list,summarise, 
                   inputDataType=paste0("<ul><li>",paste(unique(inputDataType),collapse = "</li><li>"),"</li></ul>"),
                   outputDataType=paste0("<ul><li>",paste(unique(outputDataType),collapse = "</li><li>"),"</li></ul>"))

row.names(tool_info) <- tool_info$methodID
tool_info$methodID <- NULL