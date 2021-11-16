#Quinton Hayre
setwd("/Users/Quintonhayre/Desktop/R_Biocomp/Biocomp-Exercise09/")

# Write a function that takes a directory name as an argument called dir plus any other arguments requried to accomplish the specified task.
# The function should read data from each file in the specified directory and calculate the coefficient of variation 
  #(standard deviation divided by the mean) for a user specified column. These values should be returned as a vector.
# To calculate a reliable coefficient of variation we would like to have 50 observations, but we also don’t want to force 
  #the user to use our high standard for the data. Make your function, by default, report an error if any file has less than 50 observations, 
  #but allow the user to override this behavior and only receive a warningif 50 observations are not present in a file.
# For an extra credit point, add arguments and associated code to your function to situations where a file doesn’t have 
  #the correct number of columns or the provided data includes NA’s.

#Questions to ask: 
  #1) Is the vector that I am returning a single number per file (length same as number of files)
  #2) Can I assume that all files in the directory have the same column names and that are entirely of numeric class (can)
  #3) How do I regularly commit (don't worry)
  #4) Can you clarify the what is meant by the overrise for warnings (ask the user if they are okay moving on)
  #5) Can I assume all of the files are of the same type (ie CSV)?
  #6) I wrote my code based on a common column name rather than column number. So I made my exception based on if the columnname exists in all 3 and if not then you are asked to enter a column name that is included in each

#filenames = list.files(path = "/Users/Quintonhayre/Desktop/R_Biocomp/Biocomp-Exercise09/hi", all.files = FALSE, full.names = TRUE)


#working on the column name problem 
answer = function(dir, columnname) {
  filenames = list.files(path = dir,
                         all.files = FALSE,
                         full.names = TRUE)
  m = c()
  for (i in 1:length(filenames)) {
    filename = read.csv(file = filenames[i], header = TRUE) #Assuming all are CSV files
    while (!(columnname %in% colnames(filename))){ #To address the column name not being entered correctly error
      print(paste(columnname, "is not a column name in file", filenames[i]))
      columnname = as.character(readline(prompt = "Please enter another column name that is present in all files: "))
    }
    name = filename[, columnname]
    res1 = (sd(name, na.rm = T) / mean(name, na.rm = T)) #na.rm should address the NA problem (worked when I added NAs)
    if (nrow(filename) > 49) {
      m = c(m, res1)
    }
    else{
      print(paste("Error", filenames[i], "has less than 50 observations!"))
      x = as.character(readline(prompt = "Do you wish to override for this file (yes or no)?: "))
      if (x != "yes" && x != "no"){
        x = as.character(readline(prompt = "Please enter yes or no: "))
      }else if (x == "yes") {
        m = c(m, res1)
      }
    }
  }
  print("Vector coefficent of variation in order of listed and run files:")
  return (m)
}#end of function 

#Practice use based on directory I made: wrong column name
answer("/Users/Quintonhayre/Desktop/R_Biocomp/Biocomp-Exercise09/hi", "Numbies2")
#Practice works well
answer("/Users/Quintonhayre/Desktop/R_Biocomp/Biocomp-Exercise09/hi", "Numbies")




















