#' Parse ASRS records obtained from online query
#'
#' Adjust record headers into single line headers, and split monthly
#' records.
#'
#' @param folder_path The folder path where ASRS records obtained from queries are stored.
#' @export
#' @return A list of tables, each containing one month of reports.
parse_record <- function(folder_path){

  # Collapse the first 2 header lines into one using "|" as separator.
  format_record_header <- function(raw_asrs_path){
    # Combine 2 rows into 1 separated by |
    irregular_header <- fread(raw_asrs_path,nrows=2)
    header <- str_c(irregular_header[1],"|",irregular_header[2])
    # The ACN header 1st row is empty in the raw format
    header[1] <- "ACN"
    # Fread adds one NA|NA column when trying to parse the raw data. We delete it here.
    header <- header[1:(length(header)-1)]
    # Re-Load Data
    corpus <- fread(raw_asrs_path,skip=3)
    # Remove the NA|NA column on reload
    corpus[,V97:=NULL]
    #Add correct header to data
    colnames(corpus) <- header

    return(corpus)
  }
  # Adds "_" to separate year from month on date field
  format_record_date <-function(x){
    #x is a string with format YYYYMM
    yyyy <- substr(x,1,4)
    mm <- as.integer(substr(x,5,6))
    paste0(yyyy,"_",month.abb[mm])
  }


  # Format header
  files_path <- list.files(folder_path,full.names = TRUE)
  file_names <- list.files(folder_path)
  monthly_corpora <- lapply(files_path,format_record_header)
  names(monthly_corpora) <- file_names

  # Use the date column to split dataset into monthly files
  # Note the original files may be for example every X months or year or etc
  # This standardize it per month in preparation for topic flow
  monthly_corpora <- rbindlist(monthly_corpora) # create a single data.table
  monthly_corpora <- split(monthly_corpora,by="Time|Date") #split into a list of months

  names(monthly_corpora) <- sapply(names(monthly_corpora),format_record_date)

  return(monthly_corpora)

}

#' Save parsed ASRS records obtained from online query
#'
#' @param monthly_reports_list A R list of monthly reports as obtained by \code{parse_record}.
#' @param folder_path The folder path where ASRS records obtained from queries are stored.
#' @export
#' @return No value is returned, and list of monthly reports is saved to `folder_path`/
write_record <- function(monthly_reports_list,folder_path){
  sapply(names(monthly_reports_list),
            function (x) fwrite(monthly_reports_list[[x]],
                                file=str_c(folder_path,x)))

}
