# The purpose of this script is to stack PlusAI results in common format.
# We will create a stacked file from all xlsx files in J:\Experiment Data\PlusAI\data\.

# We write to a file with the name stacked-plusai-results.csv
# We write these columns: Full_Prompt, Survey_Date, Response, Gender, Age, Geography, Weight, RT, User_ID

library(tidyverse)
library(readxl)
library(fs)

# Find the files to be stacked.
setwd("J:/Experiment Data/PlusAI/data")
filenames = fs::dir_ls()

outputFileName = "J:/Experiment Data/PlusAI/stacked-plusai-results.csv"
firstLoop = TRUE

# Print for debugging and timing.
print(paste("File loop begins", Sys.time()))

# Loop through each file.
for (filename in filenames) {
  #print("Begin loop for")
  print(filename)

  # Desired table format is: Full_Prompt, Survey_Date, Response, Gender, Age, Geography, Weight, RT, User_ID
  
  # Get the overview sheet to extract question text.
  overview = read_excel(filename, sheet = "Overview")

  # Get the sheet with the complete responses.
  complete_responses = read_excel(filename, sheet = "Complete responses")
  
  # Drop columns we don't need.
  complete_responses = select(complete_responses, -c("Time (UTC)", "Survey Completion","Publisher Category"))
  
  exact_prompt = as.character(overview[1,c("Question text")])
  print(exact_prompt)
  
  # Rename some columns.
  complete_responses = rename(complete_responses, "Response" = `Question #1 Answer`)
  complete_responses = rename(complete_responses, "RT" = `Response Time #1 (ms)`)
  complete_responses = rename(complete_responses, "User_ID" = `User ID`)
  
  # Add exact prompt column.
  complete_responses$Full_Prompt = exact_prompt

  # Survey_Date is defined by date response collection began.
  # The best place to get that information is from the filename.
  filename_length = nchar(filename)
  # The last 5 chars in filename are .xlsx.  The 10 chars before that are the date.
  Survey_Date = substr(filename, filename_length - 14, filename_length - 5)
  complete_responses$Survey_Date = Survey_Date
  #print(Survey_Date)
  
  # Reorder columns.
  complete_responses = complete_responses %>% select(Full_Prompt, Survey_Date, Response, Gender, Age, Geography, Weight, RT, User_ID)
  #print(head(complete_responses))
  
  # Write out to csv.
  if(firstLoop){
    # Use the first loop to write column names.
    write_csv(complete_responses, outputFileName, append = FALSE)
  }
  else {
    # Append on every case except first loop.
    write_csv(complete_responses, outputFileName, append = TRUE)
  }

  #print("End loop for")
  #print(filename)
  #print("------------")
  firstLoop = FALSE
}

# Print for debugging and timing.
print(paste("File loop ends", Sys.time()))

