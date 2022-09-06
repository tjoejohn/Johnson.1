  smart_wd <- function (repo, folder=NULL) {
    if(missing(folder))
    {
      setwd(print(paste(if (Sys.info()[["sysname"]]=="Windows") {("C:/GitHub")} else {
        if (Sys.info()[["sysname"]]=="Darwin"){("~/GitHub")} else ("for Linux run: setwd('/home/[INSERT YOUR USERNAME]/GitHub')")
      },"/",repo, sep = "")))}
    else 
     
    {
      setwd(print(paste(if (Sys.info()[["sysname"]]=="Windows") {("C:/GitHub")} else {
        if (Sys.info()[["sysname"]]=="Darwin"){("~/GitHub")} else ("for Linux run: setwd('/home/[INSERT YOUR USERNAME]/GitHub')")
      },"/",repo, "/", folder, sep = "")))}
  }


smart_wd(repo = "R4Eco_2022", folder = "Startup")

