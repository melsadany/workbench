correct_path <- function(file) {
  device <- ifelse(any(grepl("LSS", system("ls ~", intern = T))), "IDAS", 
                   ifelse(any(grepl("iCloud", system("ls ~", intern = T))),"me","argon"))
  if (device == "IDAS") {
    if (grepl("wdata", file)) {
      f <- sub(".*wdata", "~/LSS/jmichaelson-wdata", file)
    }else if (grepl("sdata", file)) {
      f <- sub(".*sdata", "~/LSS/jmichaelson-sdata", file)
    }
  }else if(device=="argon"){
    if (grepl("wdata", file)) {
      f <- sub(".*wdata", "/Dedicated/jmichaelson-wdata", file)
    } else if (grepl("sdata", file)) {
      f <- sub(".*sdata", "/Dedicated/jmichaelson-sdata", file)
    }
  }else if(device=="me"){
    if (grepl("wdata", file)) {
      f <- sub(".*wdata", "/Volumes/jmichaelson-wdata", file)
    } else if (grepl("sdata", file)) {
      f <- sub(".*sdata", "/Volumes/jmichaelson-sdata", file)
    }
  }
  print(f)
  return(f)
}