
plotLearningCurves <- function(target='inline') {
  
  styles <- getStyle()
  
  if (target == 'svg') {
    svglite(file='doc/Fig3.svg', width=7.5, height=3, system_fonts=list(sans='Arial'))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  
  layout(matrix(c(1,1,2,3,4,5), nrow=2, ncol=3, byrow = TRUE), widths=c(1,1,1), heights=c(1,1))
  
  # # # # # # # # # #
  # panel A: actual learning curves
  
  ylims=c(-.1*max(styles$rotation),max(styles$rotation)+(.2*max(styles$rotation)))
  plot(c(-1,36),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(-1,36),ylim=ylims,xlab='trial',ylab='reach deviation [°]',xaxt='n',yaxt='n',bty='n',main='',font.main=1)
  
  #mtext('A', side=3, outer=TRUE, at=c(0,1), line=-1, adj=0, padj=1)
  mtext('A', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1)
  
  for (groupno in c(1:length(styles$group))) {
    
    group  <- styles$group[groupno]
    curves <- read.csv(sprintf('data/%s_curves.csv',group), stringsAsFactors=FALSE)  
    curve  <- apply(curves, c(1), mean, na.rm=T)
    
    lines(c(1:15),curve[1:15],col=as.character(styles$color_solid[groupno]),lty=styles$linestyle[groupno],lw=2)
    
    lines(c(21:35),curve[76:90],col=as.character(styles$color_solid[groupno]),lty=styles$linestyle[groupno],lw=2)
  }
  
  # axis(side=1, at=c(1,10,20,30))
  axis(side=1, at=c(1,5,10,25,30,35), labels=c('1','5','10','80','85','90'),cex.axis=0.85)
  axis(side=2, at=c(0,10,20,30),cex.axis=0.85)
  
  legend(20,15,styles$label,col=as.character(styles$color_solid),lty=styles$linestyle,bty='n',lw=2,cex=0.60,seg.len=3)
  
  
  # # # # # # # # # #
  # panel B: blocked learning curves
  
  plot(c(0,5),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,4.5),ylim=ylims,xlab='trial set',ylab='',xaxt='n',yaxt='n',bty='n',main='',font.main=1)
  
  #mtext('B', side=3, outer=TRUE, at=c(2/3,1), line=-1, adj=0, padj=1)
  #mtext('B', outer=FALSE, side=3, at=c(0,0), line=1, adj=0, padj=1)
  mtext('B', outer=FALSE, side=3, line=1, adj=0, padj=1)
  
  blockdefs <- list(c(1,3),c(4,3),c(76,15))
  
  alllines <- array(NA,dim=c(length(styles$group),length(blockdefs)))
  allpolys <- array(NA,dim=c(length(styles$group),2*length(blockdefs)))
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    
    blocked <- getBlockedLearningCurves(group, blockdefs)
    
    alllines[groupno,] <- apply(blocked, c(2), mean, na.rm=T)
    
    blockedCI <- apply(blocked, c(2), t.interval)
    allpolys[groupno,] <- c(blockedCI[1,], rev(blockedCI[2,]))
    
  }
  
  # first plot all the polygons representing confidence intervals, so those are in the background
  
  for (groupno in c(1:length(styles$group))) {
    
    polX <- c(c(1,2,4),rev(c(1,2,4)))
    polY <- allpolys[groupno,]
    polygon(polX,polY,col=as.character(styles$color_trans[groupno]),border=NA)
    
  }
  
  # then plot the lines representing the means, so those are in the foreground
  
  for (groupno in c(1:length(styles$group))) {
    
    lines(c(1,2,4),alllines[groupno,],col=as.character(styles$color_solid[groupno]),lty=styles$linestyle[groupno],lw=2)
    
  }
  
  # legend(2,0.45,styles$label,col=as.character(styles$color),lty=styles$linestyle,bty='n',cex=0.7)
  
  axis(side=1, at=c(1,2,4), labels=c('1-3','4-6','76-90'),cex.axis=0.85)
  axis(side=2, at=c(0,10,20,30),labels=c('0','10','20','30'),cex.axis=0.85)
  
  
  # # # # # # # # # #
  # panel C: individual participants in the final trial set
  
  plot(c(0,5),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,4.5),ylim=ylims,xlab='group',ylab='',xaxt='n',yaxt='n',bty='n',main='',font.main=1)
  
  mtext('C', outer=FALSE, side=3, line=1, adj=0, padj=1)
  
  blockdefs <- list(c(1,3))
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    
    blocked <- getBlockedLearningCurves(group, blockdefs)
    
    X <- rep(groupno,length(blocked))
    Y <- c(blocked)
    points(x=X,y=Y,pch=16,cex=1.5,col=as.character(styles$color_trans[groupno]))
    
    meandist <- getConfidenceInterval(data=c(blocked), method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
    
    DX <- meandist$density$x
    DY <- meandist$density$y / max(meandist$density$y) / 2.5
    
    DX <- c(DX[1], DX, DX[length(DX)])
    DY <- c(0,     DY, 0)
    
    # these lines draw the distribution of bootstrapped means, and the 95% CI
    #polygon(x=DY+groupno+(1/3), y=DX, border=FALSE, col=as.character(styles$color_trans[groupno]))
    #lines(x=rep(groupno+(1/3),2),y=meandist$CI95,col=as.character(styles$color_solid[groupno]))
    #print(meandist$CI95)
    #points(x=groupno+(1/3),y=mean(c(blocked)),pch=16,cex=1.5,col=as.character(styles$color_solid[groupno]))
    
  }
  
  axis(side=1, at=c(1,2,3,4),labels=c('YN','YI','ON','OI'))
  axis(side=2, at=c(0,10,20,30),labels=c('0','10','20','30'),cex.axis=0.85)
  
  # # # # # # # # # #
  # panel D: individual participants in the final trial set
  
  plot(c(0,5),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,4.5),ylim=ylims,xlab='group',ylab='',xaxt='n',yaxt='n',bty='n',main='',font.main=1)
  
  mtext('D', outer=FALSE, side=3, line=1, adj=0, padj=1)
  
  blockdefs <- list(c(4,3))
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    
    blocked <- getBlockedLearningCurves(group, blockdefs)
    
    X <- rep(groupno,length(blocked))
    Y <- c(blocked)
    points(x=X,y=Y,pch=16,cex=1.5,col=as.character(styles$color_trans[groupno]))
    
    meandist <- getConfidenceInterval(data=c(blocked), method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
    
    DX <- meandist$density$x
    DY <- meandist$density$y / max(meandist$density$y) / 2.5
    
    DX <- c(DX[1], DX, DX[length(DX)])
    DY <- c(0,     DY, 0)
    
    #polygon(x=DY+groupno+(1/3), y=DX, border=FALSE, col=as.character(styles$color_trans[groupno]))
    #lines(x=rep(groupno+(1/3),2),y=meandist$CI95,col=as.character(styles$color_solid[groupno]))
    #print(meandist$CI95)
    #points(x=groupno+(1/3),y=mean(c(blocked)),pch=16,cex=1.5,col=as.character(styles$color_solid[groupno]))
    
  }
  
  axis(side=1, at=c(1,2,3,4),labels=c('YN','YI','ON','OI'))
  axis(side=2, at=c(0,10,20,30),labels=c('0','10','20','30'),cex.axis=0.85)
  
  # # # # # # # # # #
  # panel E: individual participants in the final trial set
  
  plot(c(0,5),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,4.5),ylim=ylims,xlab='group',ylab='',xaxt='n',yaxt='n',bty='n',main='',font.main=1)
  
  mtext('E', outer=FALSE, side=3, line=1, adj=0, padj=1)
  
  blockdefs <- list(c(76,15))
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    
    blocked <- getBlockedLearningCurves(group, blockdefs)
    
    X <- rep(groupno,length(blocked))
    Y <- c(blocked)
    points(x=X,y=Y,pch=16,cex=1.5,col=as.character(styles$color_trans[groupno]))
    
    meandist <- getConfidenceInterval(data=c(blocked), method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
    
    DX <- meandist$density$x
    DY <- meandist$density$y / max(meandist$density$y) / 2.5
    
    DX <- c(DX[1], DX, DX[length(DX)])
    DY <- c(0,     DY, 0)
    
    #polygon(x=DY+groupno+(1/3), y=DX, border=FALSE, col=as.character(styles$color_trans[groupno]))
    #lines(x=rep(groupno+(1/3),2),y=meandist$CI95,col=as.character(styles$color_solid[groupno]))
    #print(meandist$CI95)
    #points(x=groupno+(1/3),y=mean(c(blocked)),pch=16,cex=1.5,col=as.character(styles$color_solid[groupno]))
    
  }
  
  axis(side=1, at=c(1,2,3,4),labels=c('YN','YI','ON','OI'))
  axis(side=2, at=c(0,10,20,30),labels=c('0','10','20','30'),cex.axis=0.85)
  
  
  
  if (target == 'svg') {
    dev.off()
  }

}


getBlockedLearningCurves <- function(group, blockdefs) {
  
  curves <- read.csv(sprintf('data/%s_curves.csv',group), stringsAsFactors=FALSE)  
  
  # R <- dim(curves)[1] # should always be 90
  N <- dim(curves)[2]
  
  blocked <- array(NA, dim=c(N,length(blockdefs)))
  
  for (ppno in c(1:N)) {
    
    for (blockno in c(1:length(blockdefs))) {
      
      blockdef <- blockdefs[[blockno]]
      blockstart <- blockdef[1]
      blockend <- blockstart + blockdef[2] - 1
      samples <- curves[blockstart:blockend,ppno]
      blocked[ppno,blockno] <- mean(samples, na.rm=TRUE)
      
    }
    
  }
  
  return(blocked)
  
}

learningCurveANOVA <- function() {
  
  default.contrasts <- options('contrasts')
  options(contrasts=c('contr.sum','contr.poly'))
  #options('contrasts' <- default.contrasts)
  
  styles <- getStyle()
  blockdefs <- list(c(1,3),c(4,3),c(76,15))
  
  LC4aov <- getLearningCurves4ANOVA(styles, blockdefs)                      
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  print(ezANOVA(data=LC4aov, wid=participant, dv=reachdeviation, within=block,between=c(instructed, agegroup),type=3))
  
  options('contrasts' <- default.contrasts)
  
}

getLearningCurves4ANOVA <- function(styles, blockdefs) {
  
  # set up vectors that will form a data frame for the ANOVA(s):
  agegroup       <- c()
  instructed     <- c()
  participant    <- c()
  block          <- c()
  reachdeviation <- c()
  
  # keeping count of unique participants:
  startingID <- 0
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    
    # set up some basic descriptors that apply to the group:
    if (substr(group, 1, 5) == 'aging') {
      thisagegroup <- 'older'
    } else {
      thisagegroup <- 'younger'
    }
    thisinstructed <- grepl('explicit', group)
    
    # block the data:
    blocked <- getBlockedLearningCurves(group, blockdefs)
    # this is now the exact same data as the data that is plotted!
    
    # we need to know the number of participants to replicate some values:
    N <- dim(blocked)[1]
    
    for (blockno in c(1:length(blockdefs))) {
      
      agegroup        <- c(agegroup, rep(thisagegroup, N))
      instructed      <- c(instructed, rep(thisinstructed, N))
      participant     <- c(participant, c(startingID : (startingID + N - 1)))
      block           <- c(block, rep(blockno, N))
      reachdeviation  <- c(reachdeviation, blocked[,blockno])
      
    }
    
    startingID <- startingID + N
    
  }
  
  # put it in a data frame:
  LCaov <- data.frame(agegroup, instructed, participant, block, reachdeviation)
  
  # set relevant columns as factors:
  LCaov$agegroup <- as.factor(LCaov$agegroup)
  LCaov$instructed <- as.factor(LCaov$instructed)
  LCaov$block <- as.factor(LCaov$block)
  
  return(LCaov)
  
}


blockLearningANOVA <- function(block=1) {
  
  default.contrasts <- options('contrasts')
  options(contrasts=c('contr.sum','contr.poly'))
  
  
  styles <- getStyle()
  blockdefs <- list(c(1,3),c(4,3),c(76,15))
  
  LC4aov <- getLearningCurves4ANOVA(styles, blockdefs)
  
  LC4aov <- LC4aov[which(LC4aov$block == block),]
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  print(ezANOVA(data=LC4aov, wid=participant, dv=reachdeviation, between=c(instructed, agegroup),type=3))
  
  options('contrasts' <- default.contrasts)
  
}

blockLearningTtest <- function(block=1, groups=list(list('agegroup'='older', 'instructed'=TRUE),list('agegroup'='older', 'instructed'=FALSE))) {
  
  styles <- getStyle()
  blockdefs <- list(c(1,3),c(4,6),c(76,15))
  
  LC4aov <- getLearningCurves4ANOVA(styles, blockdefs)
  
  LC4aov <- LC4aov[which(LC4aov$block == block),]
  
  DVs <- list()
  
  main <- ""
  
  # first collect the data from the data frame:
  for (groupno in c(1,2)) {
    
    properties <- groups[[groupno]]
    
    if (properties$instructed) {
      instr <- 'instructed'
    } else {
      instr <- 'non-instructed'
    }
    main <- sprintf('%s%s%s %s',main,c('',' vs. ')[groupno],properties$agegroup, instr)
    
    DVs[[groupno]] <- LC4aov$reachdeviation[which(LC4aov$agegroup==properties$agegroup & LC4aov$instructed==properties$instructed)]
    
  }
  
  cat(sprintf('\n%s\n',main))
  print(t.test(DVs[[1]], DVs[[2]]))
  
  cat(sprintf('eta squared: %0.5f\n', etaSquaredTtest(DVs[[1]], DVs[[2]])))
  
}