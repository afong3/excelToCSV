library(readxl)
library(stringr)

#pathToXL is a string of the path to a given .xlsx file
#inputDirectory is a 
excel_to_csv <- function(pathToXL, inputDirectory){ #converts xlsx path to a path in a directory called "duplicateCSV" with the same subdirectories 
  sheets <- excel_sheets(pathToXL) #return value is a dataframe of all the sheetnames for a given workbook
  sheetCount <- length(sheets)
  
  duplicate_directory <- function(inputPath, parentDirectory){ #output the duplicate directory for the csv
    duplicate_as_excel <- gsub(pattern = parentDirectory, replacement = paste0(parentDirectory, "/duplicateCSV"), inputPath) ###CHANGE NEW DIRECTORY NAME: this saves file path for duplicate excel file
    duplicate_as_csv <- gsub(pattern = "xlsx", replacement = "csv", duplicate_as_excel)
    return(
      paste(
        duplicate_as_csv
      )
    )
  } #essentially saves an identical directory of the file without disturbing original directory
  
  for(i in 1:sheetCount){
    myData <- read_excel(path = pathToXL, sheet = i, col_names = FALSE) #assigns Excel sheet as dataframe w/o column names; can change this depending on what the datasets actually contain
    filename <- paste0(gsub(pattern = ".xlsx", basename(pathToXL), replacement = ""), "_", sheets[i]) #removes .xlsx from filename and adds sheet name to filename
    fullPath <- duplicate_directory(pathToXL, inputDirectory) #this object is a text string with the full file path in it
    directory <- dirname(fullPath)
   
     if(dir.exists(directory) == FALSE){ #creates the duplicate directory if one doesn't already exist
      dir.create(directory, recursive = TRUE)
    }
    write.csv(myData, file = paste0(directory, "/", filename, ".csv" )) #CSV made
  }
}

go_through_directories <- function(parentDirectory){
  
  foldersInWD <- list.dirs(parentDirectory) #puts every sub directory in parentDirectory into a list
  for(i in 1:length(foldersInWD)){
    dirName <- foldersInWD[i]
    subDir <- dir(dirName) #lists contents (directories, files) of dirName
    hits <- grep(pattern = ".xlsx", x = subDir) #shows index of .xlsx files within the sub directory
#    browser()
    if(length(hits) != 0){
      for(index in 1:length(hits)){
        thisFile <- paste0(dirName, "/", subDir[hits[index]])
        excel_to_csv(thisFile, parentDirectory) 
      }
    }
  }
}

go_through_directories("parentDirectory")###################NOTE: DEFINE PARENT DIRECTOR AS "/arc/project/st-ghenry-1/")