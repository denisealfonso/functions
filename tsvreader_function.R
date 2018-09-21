library(R.utils)
library(dplyr)

#filename is name of tsv table
#nlines is the number of lines to read at a time, default set to 1
#nmax = total number of rows to read from file. default set to maximum row


reader_function <- function(filename, nlines=1, nmax=(countLines(file))) {
  #import first 3 rows of tsv file with header to establish columns
  new_table <- read.table(filename, nrows = 3, header = TRUE, sep = "\t", fill=TRUE, quote="")
  
  #set maxcols as the number of columns identified in the first 3 rows of new_table
  maxcols <- ncol(new_table)
  
  #start_row is one more than the nrows in new_table
  start_row <- 4
  
  #as long as the start_row is less than or equal to the nmax argument, the loop will keep going. once the start_row exceeds nmax (the last row), the while loop will end
  while (start_row <= nmax){
    #read new table into temp_table. it will skip one less than start_row so that the first row is start_row. no headers so it binds to temp_table
    #will read nlines at a time
    temp_table <- read.table(filename, nrows = nlines, skip = (start_row-1), header = FALSE, sep = "\t", fill=TRUE, quote="")
    
    #check total number of columns and set to new cols
    newcols = ncol(temp_table)
    
    #only want to read lines that have the same number of columns as maxcols established by the first 3 ows of new_table
    if (maxcols == newcols) {
      #if ncolumns are the same, add temp_table rows to new_table
      new_table <- bind_rows(c(new_table, temp_table))
      
      #increase start_row by nlines 
      start_row <- start_row + nlines
    }
    else{
      #if ncolumns are NOT the same, then just increase the start_row value by nlines and go through loop again
      start_row <- start_row + nlines
    }
    
  }

  return(new_table)
}