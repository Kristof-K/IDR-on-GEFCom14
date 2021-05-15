
# path to the directory containing the GEFCom14 data
path <- "D:Studium/Semester6/BachelorArbeit/GEFCom2014_Data"

# function loading a specific track
loadTrack = function() {
  subdir <- "Solar/Task\ 1"
  file <- "predictors1.csv"
  
  data = read.table(paste(path, subdir, file, sep="/"), header=TRUE, dec=".",
                    sep=",")
  return(data)
}
