# Custom function used below to simplify extraction of date-time information
extract.date.time <- function(filepath.n, Draw = "Morning") {
  # Extracts date.time info from recording filepath, for use in pattern. Will only need to provide correct
  # recording filepath number
  # Default is Morning, can specify Dusk or Nocturnal
  if (Draw == "Morning") {
    name <- MorningDraw$file.path[filepath.n]
    date.time <- substr(name, nchar(name) - 18, nchar(name) - 4)
  }
  else if (Draw == "Dusk"){
    name <- DuskDraw$file.path[filepath.n]
    date.time <- substr(name, nchar(name) - 18, nchar(name) - 4)
  }
  else if (Draw == "Nocturnal"){
    name <- NocturnalDraw$file.path[filepath.n]
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
    
    answer <- tolower(readline(
      prompt = "Is the recording good enough to transcribe?? (y or n, or x to escape the function)"))
    
    if(answer == "y"){
      print(paste("using recording #", counter))
      
      # Early 1 - After you find a good recording (first one), 
      # replace correct file number in variable 'recording_to_use_...' below
      # recording_to_use_early_deployment <- 2
      date.time <- extract.date.time(counter, time_of_day)
      
      # Copy all files within ±2 minutes of selected recording
      target_time <- as.POSIXct(
        date.time,
        format = "%Y%m%d_%H%M%S",
        tz = "UTC"
      )
      
      file_times <- as.POSIXct(
        sub(".*?(\\d{8}_\\d{6}).*", "\\1", section.list$file.path),
        format = "%Y%m%d_%H%M%S",
        tz = "UTC"
      )
      
      files <- section.list$file.path[
        abs(as.numeric(difftime(file_times, target_time, units = "mins"))) <= 10
      ]
      
      files  # check to make sure the list looks correct
      
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
