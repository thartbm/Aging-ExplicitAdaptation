

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


# this function calculates a 95% confidence interval if the mean by applying the moments for a sample t distribution:

t.interval = function(data, variance = var(data, na.rm=T), conf.level = 0.95) {
  
  z = qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
  
  xbar = mean(data, na.rm=T)
  sdx = sqrt(variance/length(which(is.finite(data))))
  
  return(c(xbar - z * sdx, xbar + z * sdx))
  
}

# this function can do the same, but can also bootstrap a 95% confidence interval, and in that case use different functions apart from the mean:

getConfidenceInterval <- function(data, variance = var(data), conf.level = 0.95, method='t-distr', resamples=1000, FUN=mean, returndist=FALSE) {
  
  if (method %in% c('t-distr','t')) {
    
    z = qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
    
    xbar = mean(data)
    sdx = sqrt(variance/length(data))
    
    return(c(xbar - z * sdx, xbar + z * sdx))
    
  }
  
  # add sample z-distribution?
  
  # for bootstrapping:
  
  if (method %in% c('bootstrap','b')) {
    
    data <- data[which(is.finite(data))] #need is.finite due to NA values
    
    samplematrix <- matrix(sample(data, size = resamples*length(data), replace = TRUE), nrow = resamples)
    BS <- apply(samplematrix, c(1), FUN=FUN) 
    
    lo <- (1-conf.level)/2.
    hi <- 1 - lo
    
    if (returndist) {
      percentiles <- data.frame(percentile=seq(.01,.99,.01),value=quantile(BS, probs=seq(.01,.99,.01)))
      densdist <- density(BS, bw='SJ', from=min(percentiles$value), to=max(percentiles$value))  
      return(list('percentiles'=percentiles, 'density'=densdist, 'CI95'=quantile(BS, probs = c(lo,hi))))
    } else {
      return(quantile(BS, probs = c(lo,hi)))
    }
    
  }
  
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
                  
                  '30explicit_allnocursors.csv'     = 'https://osf.io/bwjxc/download',
                  '30implicit_allnocursors.csv'     = 'https://osf.io/vkgjp/download',
                  'aging_explicit_allnocursors.csv' = 'https://osf.io/xd643/download',
                  'aging_implicit_allnocursors.csv' = 'https://osf.io/tnyj6/download',
                  
                  # RAW LOCALIZATION DATA, now used in scripts:
                  # (only to count percentage missing data)
                  '30explicit_localization.csv'     = 'https://osf.io/x38nq/download',
                  '30implicit_localization.csv'     = 'https://osf.io/qykzt/download',
                  'aging_explicit_localization.csv' = 'https://osf.io/4wkh2/download',
                  'aging_implicit_localization.csv' = 'https://osf.io/yck5w/download',
                  
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
  
  cat("Check complete.")
  if (check) {
    cat(" (set 'check=FALSE' to force downloading and overwrite current files)")
  }
  cat("\n")
  
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
  
  # this function reads the participant IDs from the participants.csv file
  # and then checks if they are all present in the files with reaching
  # and localization data
  # it will add columns to the participants data frame for each data file,
  # and set the value to TRUE if the participant has data in that file
  
  participants <- read.csv('data/participants.csv', stringsAsFactors=F)
  groups <- c('30explicit','30implicit','aging_explicit','aging_implicit')
  file_extensions <- c('curves','nocursors','loc_p3_AOV','localization','allnocursors')
  
  for (extension in file_extensions) {
    
    participants[extension] <- FALSE
    
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

rejectedData <- function() {
  
  groups <- c('30explicit','30implicit','aging_explicit','aging_implicit')
  learning <- c()
  
  localization <- list('ALact'= c(),
                       'ALpas'= c(),
                       'ROact'= c(),
                       'ROpas'= c())
  
  sessions  <- list('AL'=0,'RO'=1)
  movements <- list('act'=0,'pas'=1)
  
  nocursors <- list('aligned'=c(),'exclude'=c(),'include'=c())
  nctypes <- c('aligned','exclude','include')
  
  for (group in groups) {
    
    df <- read.csv(sprintf('data/%s_curves.csv',group), stringsAsFactors = F)
    
    RD <- as.matrix(df)
    learning <- c(learning, (length(which(is.na(RD))) / prod(dim(RD)) ) * 100 )
    
    df <- read.csv(sprintf('data/%s_localization.csv', group), stringsAsFactors = F)
    
    N <- length(unique(df$participant))
    
    for (session in names(sessions)) {
      
      for (movement in names(movements)) {
        
        subdf <- df[which(df$rotated == sessions[[session]] & df$passive == movements[[movement]]),]
        
        locID <- sprintf('%s%s',session,movement)
        localization[[locID]] <- c( localization[[locID]], (1 - ((dim(subdf)[1]) / (N * 72))) * 100)
        
      }
      
    }
    
    df <- read.csv(sprintf('data/%s_allnocursors.csv',group), stringsAsFactors = F)
    
    N <- length(unique(df$participant))
    
    for (nctype in nctypes) {
      
      subdf <- df[which(df$condition == nctype),]
      nocursors[[nctype]] <- c(nocursors[[nctype]], (length(which(is.na(subdf$angular_deviation))) / (N * 36)) * 100)
      
    }
    
  }
  

  for (group in groups) {
    

    
  }
  
  rejected <- data.frame('learningcurves'=learning, 
                         'loc-alignedactive'=localization[['ALact']],
                         'loc-alignedpassive'=localization[['ALpas']],
                         'loc-rotatedactive'=localization[['ROact']],
                         'loc-rotatedpassive'=localization[['ROpas']],
                         'nc-aligned'=nocursors[['aligned']],
                         'nc-exclude'=nocursors[['exclude']],
                         'nc-include'=nocursors[['include']])
  
  columns <- names(rejected)
  rejected <- as.data.frame(t(as.matrix(rejected)), row.names=columns)
  names(rejected) <- groups
  rejmat <- as.matrix(rejected)
  
  younger <- c(mean(rejmat[1,1:2]), mean(rejmat[2:4,1:2]), mean(rejmat[6:8,1:2]))
  older   <- c(mean(rejmat[1,3:4]), mean(rejmat[2:4,3:4]), mean(rejmat[6:8,3:4]))
  short <- data.frame('younger'=younger, 'older'=older, row.names=c('learningcurves','localization','nocursors'))
  
  
  return(list('full'=rejected, 'short'=short))
  
}

