CEOdataStartupMessage <- function()
{
  msg <- c(paste0("CEOdata version ", 
           utils::packageVersion("CEOdata")),
           "\nPlease acknowledge the CEO in your publications.\nType \"vignette('using_CEOdata')\" for help.")
  return(msg)
}

.onAttach <- function(lib, pkg)
{
  # startup message
  msg <- CEOdataStartupMessage()
  if(!interactive())
    msg[1] <- paste("Package 'CEOdata' version", utils::packageVersion("CEOdata"))
  packageStartupMessage(msg)      
  invisible()
  options(encoding = "UTF-8")
}