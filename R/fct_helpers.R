# createLink for asia.ensembl.org ------------------------------------------------------
aensLink <- function(val,ens) {
  sprintf('<a href="https://asia.ensembl.org/Homo_sapiens/Gene/Summary?g=%s" target="_blank" class="btn btn-primary">%s</a>',val,ens)
}

# median methods for group
median.cut <- function(data,variables,time,event) {
  df <- NULL
  i <- NULL
  for (i in variables) {
    v <- as.numeric(data[i][,1])
    tmp <- ifelse(v > median(v),"low","high")
    df <- cbind(df,tmp)
  }
  df <- as.data.frame(df)
  colnames(df) <- variables
  res <- cbind(time = data[time][,1],event = data[event][,1],df)
  colnames(res)[1:2] <- c(time,event)
  return(res)
}

# Trisection
tri.cut <- function(data,variables,time,event) {
  ls <- list()
  df <- NULL
  i <- NULL
  for (i in variables) {
    v <- as.numeric(data[i][,1])
    o <- 0
    ot <- as.numeric(quantile(v,1/3))
    st <- as.numeric(quantile(v,2/3)) 
    if (o ==ot | ot == st) {
      ls[[i]] <- i
    }
    else  {
      categories <- cut(v, breaks = c(o, ot, st, Inf),
                        labels = c("low", "medium", "high"),right = FALSE)
      tmp <- as.character(categories)
      df <- cbind(df,tmp)
    }
  }
  df <- as.data.frame(df)
  colnames(df) <- variables[which(!(variables %in% unlist(ls)))]
  res <- cbind(time = data[time][,1],event = data[event][,1],df)
  colnames(res)[1:2] <- c(time,event)
  return(list(res,ls))
}