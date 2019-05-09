

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
