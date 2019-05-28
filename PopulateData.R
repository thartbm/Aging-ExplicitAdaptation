# populate the /data directory using the osfDataFiles object
PopulateData <- function(){
  osfDataFiles<- readRDS('osfDataFiles.rds') 
  if (length(list.files(path = '/data')) < 3) {
    for(i in 1:nrow(osfDataFiles)){
      row <- osfDataFiles[i,]
      
      download.file(url = row$meta[[1]]$links$download, destfile = paste("data", row$name, sep="/"))

      }
  }
}