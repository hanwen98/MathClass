pollutantmean <- function(directory, pollutant, id) {
  files <- list.files(directory,full.names = TRUE)
  dat_id = data.frame()
  for (i in id) {
    dat_id <- rbind(dat_id, read.csv(files[i]))
  }
  mean(dat_id[, pollutant], na.rm=TRUE)
}

complete <- function(directory, id){
 
  files <- list.files(directory,full.names = TRUE)
  nobs <- list()
  
  for (i in seq_along(id)){
    dat <- read.csv(files[i])
    n <- sum(!is.na(dat[2]))
    nobs[[i]] <- n
  }
output <- cbind(id, nobs)
}

corr <- function(directory, threshold = 0){
  files <- list.files(directory,full.names = TRUE)
  tmp <- vector(mode = "list", length = length(files))
  for (i in seq_along(files)) {
    tmp[[i]] <- read.csv(files[[i]])
  }
  output <- do.call(rbind, tmp)
  nit <- output[,"nitrate"]
  sul <- output[,"sulfate"]
  pollutant <- cbind(nit,sul)
  n <- sum(!is.na(pollutant[1]))
  if (n > threshold){
    cor(nit, sul, na.rm = T)
  }
  
}

mean<-pollutantmean("specdata", "nitrate", 70:72)
complete <- complete("specdata", c(2, 4, 8, 10, 12))
cr <- corr("specdata", 400)

