# Custom function used below to simplify extraction of date-time information
extract.date.time <- function(filepath.n, Draw = "Morning") {
  # Extracts date.time info from recording filepath, for use in pattern. Will only need to provide correct
  # recording filepath number
  # Default is Morning, can specify Dusk
  if (Draw == "Morning") {
    name <- MorningDraw$file.path[filepath.n]
    date.time <- substr(name, nchar(name) - 18, nchar(name) - 4)
  }
  else if (Draw == "Dusk"){
    name <- DuskDraw$file.path[filepath.n]
    date.time <- substr(name, nchar(name) - 18, nchar(name) - 4)
  }
  return(date.time)
}
####

# Custom function used below to simplify selection of recordings
select_recordings <- function(draw, time_of_day = "Morning", initial_file_index, target_folder){
  continue_looping <- TRUE
  counter <- initial_file_index
  while(continue_looping == TRUE){
    print(paste("playing recording #", counter))
    play(draw$file.path[counter])
    answer <- tolower(readline(prompt = "Is the recording good enough to transcribe?? (y or n, or x to escape the function)"))
    if(answer == "y"){
      print(paste("using recording #", counter))
      # Early 1 - After you find a good recording (first one), 
      # replace correct file number in variable 'recording_to_use_...' below
      # recording_to_use_early_deployment <- 2
      date.time <- extract.date.time(counter, time_of_day)
      
      #copy all files with the appropriate date/time
      pattern <- paste0("*", date.time, ".*")
      files <- section.list$file.path[grepl(pattern = pattern, section.list$file.path)]
      files #check to make sure the list of files looks correct before copying - should be 9 files, 
      # one for each ARU in grid PSU
      file.copy(files, target_folder)
      print("recordings copied over")
      continue_looping <- FALSE
    }
    else if(answer == "n"){
      counter <- counter + 1
    }
    else if(answer == "x"){
      continue_looping <- FALSE
      print("loop stopped, if you ran out of recordings, re-draw above before running this function again")
    }
    else {
      print("key(s) entered are not accepted, only y, n, or x accepted")
    }
  }
  return(counter)
}
