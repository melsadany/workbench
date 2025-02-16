correct_path <- function(file) {
  device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
  if (device == "IDAS") {
    if (grepl("wdata", file)) {
      f <- sub(".*wdata", "~/LSS/jmichaelson-wdata", file)
    }else if (grepl("sdata", file)) {
      f <- sub(".*sdata", "~/LSS/jmichaelson-sdata", file)
    }
  }else {
    if (grepl("wdata", file)) {
      f <- sub(".*wdata", "/Dedicated/jmichaelson-wdata", file)
    } else if (grepl("sdata", file)) {
      f <- sub(".*sdata", "/Dedicated/jmichaelson-sdata", file)
    }
  }
  print(f)
  return(f)
}