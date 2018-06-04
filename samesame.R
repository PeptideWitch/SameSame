library(shiny)
library(dplyr)
library(tidyr)
library(data.table)
library(purrr)
library(pscl)
library(ggplot2)
library(MASS)
library(qvalue)

main <- function() {

  print(PQFDR)
  
  combinations1 <- combn(6,3, nbins = 3, simplify=FALSE)
  
  # #Import the CSV files
  # producefile <- function(id, num) {
  #   temp <- list.files(pattern="*.csv")
  #   data <- lapply(temp, fread, select = c(id, num), header=TRUE)
  #   return(data)
  # }
  # 
  # datahere <- producefile("Identifier", "rI")
  
  #Perform data normalisation
  normaldata <- lapply(seq_along(datahere), function(x){
    # cbind(datahere[[x]][,1], datahere[[x]][,2] + 0.01) # Spectral fraction added to sample
    cbind(datahere[[x]][,1], datahere[[x]][,2] / sum(datahere[[x]][,2])) # Simple normalisation
    })
  
  ##Inner Join Method for TTesting:
  
  samesame <- function(y, ID){
    
    reducecontrol <- normaldata[combinations1[[y]]] %>% Reduce(function(dtf1,dtf2) inner_join(dtf1,dtf2,by=ID), .)
    colnames(reducecontrol) <- c(ID,combinations1[[y]])
    
    coltouse = 21 - y
  
    reducetreatment <- normaldata[combinations1[[coltouse]]] %>% Reduce(function(dtf1,dtf2) inner_join(dtf1, dtf2,by=ID), .)
    colnames(reducetreatment) <- c(ID,combinations1[[coltouse]])
  
    reducefinal <- right_join(reducecontrol, reducetreatment, by=ID) %>% replace(., is.na(.), 0)
  
    ttestresults <- sapply(seq(1:nrow(reducetreatment)), function(x){
     t.test(reducefinal[x,2:4], reducefinal[x,5:7])$p.value
      })
    
    ttestresults2 <- list(reducefinal[,1], ttestresults)
  
    return(ttestresults2)
    }
  
  ttestresults <- lapply(seq(1:10), samesame, ID)
  
  #Making the histogram chart
  plot.new() 
  png(filename="tplot.png")
  dev.control("enable")
  par(mfrow=c(3,4))
  hist(ttestresults[[1]][[2]], nclass=20)
  hist(ttestresults[[2]][[2]], nclass=20)
  hist(ttestresults[[3]][[2]], nclass=20)
  hist(ttestresults[[4]][[2]], nclass=20)
  hist(ttestresults[[5]][[2]], nclass=20)
  hist(ttestresults[[6]][[2]], nclass=20)
  hist(ttestresults[[7]][[2]], nclass=20)
  hist(ttestresults[[8]][[2]], nclass=20)
  hist(ttestresults[[9]][[2]], nclass=20)
  hist(ttestresults[[10]][[2]], nclass=20)
  tplot <- recordPlot()
  saveRDS(tplot, "tplot.rds")
  dev.off()
  
  ## Q Value analysis
  
  dotheq <- function(m){
    print(m)
  
    if (m == "BioQ") {
      Q <- lapply(seq_along(1:10), function(x){
        QQ <- qvalue(p = ttestresults[[x]][[2]])
        QFDR <- QQ$lfdr
        QVAL <- QQ$qvalue
        return(list(ttestresults[[x]][[1]], QVAL, QFDR))
    })
    }
  
    else {
      Q <- lapply(seq_along(1:10), function(x){
        QQ <- p.adjust(ttestresults[[x]][[2]], method=m)
        return(list(ttestresults[[x]][[1]], QQ))
    })
    }
  }
  
  qtestvalues <- dotheq(qmethod)
  qtestvalues[[1]][[2]]
  
  findtheq3 <- function(x,a,b){
    sf <- 100 * (sum(a < (x / 100)) / b )
    return(sf)
  }
  
  findtheq2 <- function(z,a,b){
    x <- z-1
    lock <- TRUE
    while(lock == TRUE) {
      x <- x + 1
      sf <- (sum(a < (x / 100)) / b)
      if (sf > PQFDR) {lock <- FALSE} # This is the PQ-FDR cutoff value
    }
    return(x / 100)
  }
  
  findtheq <- function(x){
      sumvalue <- NROW(qtestvalues[[x]][[2]])
      data <- qtestvalues[[x]][[2]]
      FDRreturn <- sapply(1, findtheq2, data, sumvalue)
      FDRreturn2 <- sapply(seq_along(1:100), findtheq3, data, sumvalue)
      return(list(FDRreturn, FDRreturn2))
    }
  
  qresults <- lapply(seq_along(1:10), findtheq)
  
  qaverage <- mean(sapply(qresults, `[[`, 1))
  qaveragehist <- rowMeans(sapply(qresults, `[[`, 2))
  print(qaveragehist)
  
  #2nd histogram
  plot.new() 
  png(filename="qplot.png")
  dev.control("enable")
  par(mfrow=c(3,4))
  barplot(qresults[[1]][[2]], main="Combination 1", xlab="Significance Cutoff [0.01 to 1]", ylab="PQ-FDR", col="Grey",space = 0, border=NA)
  barplot(qresults[[2]][[2]], main="Combination 2", xlab="Significance Cutoff [0.01 to 1]", ylab="PQ-FDR", col="Grey",space = 0, border=NA)
  barplot(qresults[[3]][[2]], main="Combination 3", xlab="Significance Cutoff [0.01 to 1]", ylab="PQ-FDR", col="Grey",space = 0, border=NA)
  barplot(qresults[[4]][[2]], main="Combination 4", xlab="Significance Cutoff [0.01 to 1]", ylab="PQ-FDR", col="Grey",space = 0, border=NA)
  barplot(qresults[[5]][[2]], main="Combination 5", ylab="PQ-FDR", col="Grey",space = 0, border=NA)
  barplot(qresults[[6]][[2]], main="Combination 6", ylab="PQ-FDR", col="Grey",space = 0, border=NA)
  barplot(qresults[[7]][[2]], main="Combination 7", ylab="PQ-FDR", col="Grey",space = 0, border=NA)
  barplot(qresults[[8]][[2]], main="Combination 8", ylab="PQ-FDR", col="Grey",space = 0, border=NA)
  barplot(qresults[[9]][[2]], main="Combination 9", ylab="PQ-FDR", col="Grey",space = 0, border=NA)
  barplot(qresults[[10]][[2]], main="Combination 10", ylab="PQ-FDR", col="Grey", space = 0, border=NA)
  barplot(qaveragehist, main="Average PQ-FDR", ylab="PQ-FDR", col="Purple", space = 0, border=NA)
  qplot <- recordPlot()
  saveRDS(qplot, "qplot.rds")
  dev.off()
  
  assign('ttestresults',ttestresults,envir=.GlobalEnv)
  assign('normaldata',normaldata,envir=.GlobalEnv)
  assign('qtestvalues',qtestvalues,envir=.GlobalEnv)
  assign('qaverage',qaverage,envir=.GlobalEnv)
  assign('qresults',qresults,envir=.GlobalEnv)
  assign('barplot',barplot,envir=.GlobalEnv)
  assign('hist',hist,envir=.GlobalEnv)
  assign('tplot',tplot,envir=.GlobalEnv)
  assign('qplot',qplot,envir=.GlobalEnv)
  return()
}

main()
