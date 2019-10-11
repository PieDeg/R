
factorize_months <- function(months_input){
  output <- factor(months_input, levels=month.name,
                   labels=month.name)
}

format_monthyear <- function(mydates){
  mydates_sorted <- sort(unique(mydates))
  factors <- unique(format(mydates_sorted, "%b%y"))
  output <- factor(format(mydates,"%b%y"), levels=factors, labels=factors)
  return(output)
}

factorize_weekdays <- function(weekdays_input){
  output <- factor(weekdays_input, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
                   labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
}

read.xlsFolder <- function(path){
  # Reads all excel files within a folder (path) and combines in one dataframe (df_out)
  if(substr(path,nchar(path),nchar(path))!="/") {
    path <- paste0(path,"/")
  }
  filenames <- list.files(path)
  
  df_out <- sapply(paste0(path,filenames), read_excel, simplify=FALSE) %>%
    bind_rows()
  
  return(df_out)
  print(sprintf("Files read: %s",paste(filenames, collapse=', ')))
} 

reformat_fieldnames <- function(fieldnames){
  # automatically tries to simplify fieldnames by removing spaces and () etc.
  newnames <- gsub(pattern="\\(.*\\)", replacement="", x=fieldnames)
  newnames <- gsub(pattern=" ", replacement="", x=newnames)
  newnames <- gsub(pattern=".", replacement="", x=newnames)
  
  return(newnames)
}