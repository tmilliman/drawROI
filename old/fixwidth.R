fixWidthText <- function(inText, width = 80){
  fwt <- vector()
  inText <- gsub(x = inText, '  ', ' ', fixed = T)
  
  while(inText!=''){
    n <- nchar(inText)
    
    if(n < width) {
      fwt[length(fwt)+1] <- inText
      inText <- ''
      next()
    }
    txt79 <- substring(inText, width - 1, width - 1)
    txt80 <- substring(inText, width, width)
    txt81 <- substring(inText, width + 1, width + 1)
    
    if(txt79==' '){
      fwt[length(fwt)+1] <- substring(inText, 1, width - 1)
      inText <- substring(inText, width, n)
    }else if(txt80==' '){
      fwt[length(fwt)+1] <- substring(inText, 1, width )
      inText <- substring(inText, width +1 , n)
    }else if(txt81==' '){
      fwt[length(fwt)+1] <- substring(inText, 1, width )
      inText <- substring(inText, width + 2 , n)
    }else{
      fwt[length(fwt)+1] <- paste0(substring(inText, 1, width - 1), '-')
      inText <- substring(inText, width-1, n)
    }
  }
  fwt
}


