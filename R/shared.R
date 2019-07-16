

getStyle <- function() {
  

  # This is for plots for aging:
  groups    =  c('30implicit', 
                 '30explicit', 
                 'aging_implicit', 
                 'aging_explicit')
  rotations =  c(30,
                 30,          
                 30,               
                 30)
  solidcolors =  c(rgb(229, 22,  54,  255, max = 255), 
                   rgb(255, 128, 0,   255, max = 255), 
                   rgb(136, 0,   238, 255, max = 255),
                   rgb(136, 153, 255, 255, max = 255))
  
  transcolors =  c(rgb(229, 22,  54,  47,  max = 255), 
                   rgb(255, 128, 0,   47,  max = 255), 
                   rgb(136, 0,   238, 47,  max = 255),
                   rgb(136, 153, 255, 47,  max = 255))
  
  linestyles = c(2,
                 1,
                 2,
                 1)
  labels <-    c('younger non-instructed',
                 'younger instructed',
                 'older non-instructed',
                 'older instructed')
  
  
  styles <- data.frame(groups,rotations,solidcolors,transcolors,linestyles,labels)
  colnames(styles) <- c('group','rotation','color_solid','color_trans','linestyle','label')
  
  return(styles)
  
}




t.interval = function(data, variance = var(data, na.rm=T), conf.level = 0.95) {
  
  z = qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
  
  xbar = mean(data, na.rm=T)
  sdx = sqrt(variance/length(which(is.finite(data))))
  
  return(c(xbar - z * sdx, xbar + z * sdx))
  
}

ensureData <- function(check=TRUE) {
  
  cat('Checking if all data is available locally.\n')
  
  data_files <- c('30explicit_curves.csv' =     'https://osf.io/92xut/download',
                  '30implicit_curves.csv' =     'https://osf.io/4sazb/download',
                  'aging_explicit_curves.csv' = 'https://osf.io/q32v9/download',
                  'aging_implicit_curves.csv' = 'https://osf.io/efgyk/download',
                  
                  '30explicit_nocursors.csv'     = 'https://osf.io/utw7g/download',
                  '30implicit_nocursors.csv'     = 'https://osf.io/rkd6w/download',
                  'aging_explicit_nocursors.csv' = 'https://osf.io/9hksg/download',
                  'aging_implicit_nocursors.csv' = 'https://osf.io/hktd5/download',
                  
                  # RAW LOCALIZATION DATA, not used in scripts:
                  #'30explicit_localization.csv'     = 'https://osf.io/x38nq/download',
                  #'30implicit_localization.csv'     = 'https://osf.io/qykzt/download',
                  #'aging_explicit_localization.csv' = 'https://osf.io/4wkh2/download',
                  #'aging_implicit_localization.csv' = 'https://osf.io/yck5w/download',
                  
                  # confidence intervals for localization throughout the used workspace
                  # calculate with a sample t-distribution
                  '30explicit_localization_tCI.csv'     = 'https://osf.io/z3rct/download',
                  '30implicit_localization_tCI.csv'     = 'https://osf.io/bjwm6/download',
                  'aging_explicit_localization_tCI.csv' = 'https://osf.io/ntx7s/download',
                  'aging_implicit_localization_tCI.csv' = 'https://osf.io/r7f8z/download',
                  
                  # localization at three points in the workspace
                  # used for ANOVA's, and the averages in the figures
                  '30explicit_loc_p3_AOV.csv'     = 'https://osf.io/84tw2/download',
                  '30implicit_loc_p3_AOV.csv'     = 'https://osf.io/2vem6/download',
                  'aging_explicit_loc_p3_AOV.csv' = 'https://osf.io/f83p7/download',
                  'aging_implicit_loc_p3_AOV.csv' = 'https://osf.io/s32mb/download',
                  
                  # demographics of participants
                  'participants.csv' = 'https://osf.io/k4t7j/download'
                )
  
  for (filename in names(data_files)) {
    
    folderfilename <- sprintf('data/%s',filename)
    
    if (!check | !file.exists(folderfilename)) {
      
      url = as.character(data_files[filename])
      
      cat(sprintf("Downloading: '%s' from '%s'\n", filename, url))
      
      df <- read.csv(url(url),stringsAsFactors=FALSE)
      
      write.csv(df,folderfilename,row.names=FALSE,quote=FALSE)
      
    }
    
  }
  
}

etaSquaredTtest <- function(g1,g2=NA,na.rm=TRUE,mu=0) {
  
  doOneSample <- FALSE
  doTwoSample <- FALSE
  
  if (length(g2) == 1) {
    if (is.na(g2)) {
      doOneSample <- TRUE
    } else {
      # set mu to the single value in g2 and do a one sample one anyway?
    }
  } else {
    doTwoSample <- TRUE
  }
  
  if (doOneSample) {
    
    # compare group 1 mean with mu as explanation
    SStotal <- sum((g1-mean(g1,na.rm=na.rm))^2)
    SSeffect <- sum(((mean(g1, na.rm=na.rm) - mu)^2)*length(g1))
    # 
    # 
    return(SSeffect / SStotal)
    
  }
  
  if (doTwoSample) {
    
    overallmean <- mean(c(g1,g2),na.rm=na.rm)
    # compare overall mean with group means as explanation
    SStotal <- sum((c(g1,g2) - overallmean)^2, na.rm=na.rm)
    SSeffect <- sum(length(g1)*(mean(g1,na.rm=na.rm)-overallmean)^2, length(g2)*(mean(g2,na.rm=na.rm)-overallmean)^2)
    return(SSeffect / SStotal)
      
  }
  
}

checkParticipantsData <- function() {
  
  participants <- read.csv('data/participants.csv', stringsAsFactors=F)
  groups <- c('30explicit','30implicit','aging_explicit','aging_implicit')
  file_extensions <- c('curves','nocursors','loc_p3_AOV')
  
  for (extension in file_extensions) {
    
    participants[extension] <- NA
    
    for (group in groups) {
    
      df <- read.csv(sprintf('data/%s_%s.csv',group,extension), stringsAsFactors=F)
      
      if (extension %in% c('curves')) {
        dapa <- names(df)
      } else {
        dapa <- unique(df$participant)
      }
      
      participants[participants$participant %in% dapa,extension] <- TRUE
      
      
    }
    
  }
  
  return (participants)
  
}
