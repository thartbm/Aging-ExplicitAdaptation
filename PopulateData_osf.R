PopulateData_osf <- function(){

  
  if (length(list.files(path = './data2')) < 3) {
    osfProject <- osf_retrieve_node('QZHMY')
    osfDataFiles <- osfProject %>%
      osf_ls_files() %>%
      filter(name == "data") %>%
      osf_ls_files(n_max = 100)
    
    # for(i in 1:nrow(osfDataFiles)) {
    #   row <- osfDataFiles[i,]
    #   # do stuff with row
    #   osf_download(row, path = paste("./data2", row$name, sep="/"))
    # }
    
    # #make a list of download links
    # osfDownloadLinks <- data.frame()
    # for(i in 1:nrow(osfDataFiles)) {
    #   row <- osfDataFiles[i,]
    #   # do stuff with row
    #   osfDownloadLinks <- c(osfDownloadLinks, row$meta[[1]]$links$download)
    
    #save this thing
    saveRDS(osfDataFiles, file = 'osfDataFiles.rds')
  }
}

# load.DownloadDataframe <- function(url,filename) {
#   
#   if (file.exists(filename)) {
#     
#     df <- read.csv(filename, stringsAsFactors=FALSE)
#     
#   } else {
#     
#     df <- read.csv(url(url),stringsAsFactors=FALSE)
#     
#     write.csv(df,filename,row.names=FALSE,quote=FALSE)
#     
#   }
#   
#   return(df)
#   
# }

