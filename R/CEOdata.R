#' Import datasets / microdata from the "Centre d'Estudis d'Opinio"
#'
#' Easy and convenient access to the datasets / microdata of the "Centre
#' d'Estudis d'Opinio", the catalan institution for polling and public opinion.
#' The package uses the data stored in the servers of the CEO and returns it in
#' a tidy format (tibble).
#'
#' @encoding UTF-8
#' @param kind Character vector with the sort of microdata required. Defaults to "barometer", that contains the whole set of Barometers from 2014 (presential interviews). "barometer_until_2013" contains the interviews performed by phone until 2013, with a somewhat different questionnaire and structure. For such dataset you need a third-party software installed in your computer to be able to uncompress the RAR original file.
#' @param reo Character vector of length one that allows to get the dataset of a specific REO (Registre d'Estudis d'Opinio, the internal register ID used by the CEO) to download. By default (when \code{reo = NA}) it uses the \code{kind} argument.
#' @param raw Logical value to indicate if SPSS labels are transformed into factors. Defaults to TRUE. Otherwise it returns the matrices as imported by haven::read_spss(). Does not apply to data from singular REOs.
#' @param complementary_variables Logical value as to whether include (default) complementary variables such as date (Data). Defaults to TRUE. Names of such new variables only use upper case in the first letter. Complementary variables are added at the end. Does not apply to data from singular REOs.
#' @param date_start Character vector with a starting date ("DD-MM-YYYY") for the data.
#' @param date_end Character vector with an end date ("DD-MM-YYYY") for the data.
#' @export
#' @return A tibble with the individuals' responses to the questionnaire retrieved.
#' @examples
#'\dontrun{
#' d <- CEOdata()
#'
#' # Get the number of individuals surveyed and the number of variables recorded.
#' dim(d)
#'
#' # Get the identifiers of the different Barometers retrieved
#' unique(d$BOP_NUM)
#'}
CEOdata <- function(kind = "barometer",
                    reo = NA,
                    raw = FALSE,
                    complementary_variables = TRUE,
                    date_start = NA, date_end = NA) {
  if (is.na(reo)) {
    #
    # Define URLs
    #
    url.phone.barometer <- "https://ceo.gencat.cat/web/.content/20_barometre/Matrius_BOP/2013_Microdades_anonimitzades_fusio_cine_telf.zip"
    file.phone.barometer.rar <- "2013_Microdades_anonimitzades_fusio_cine_telf.zip"
    file.phone.barometer <- "Microdades anonimitzades fusio cine telf.sav"
    url.presential.barometer <- "https://ceo.gencat.cat/web/.content/20_barometre/Matrius_BOP/Microdades_Des20.zip.zip"
    file.presential.barometer <- "Microdades anonimitzada fusio presencial.sav"
    # Process barometer merged from 2014
    if (kind == "barometer") {
      tmp <- tempfile()
      download.file(url.presential.barometer, tmp)
      file <- unzip(tmp, file.presential.barometer)
      d <- haven::read_spss(file)
      if (file.exists(file)) {
        unlink(file)
      }
    }
    # Process barometer merged until 2013
    if (kind == "barometer_until_2013") {
      # This must be fixed because the original file as of 211027 is not a zip file, but a RAR file
      download.file(url.phone.barometer, file.phone.barometer.rar)
      system("unrar e 2013_Microdades_anonimitzades_fusio_cine_telf.zip")
      file <- file.phone.barometer
      d <- haven::read_spss(file)
      names(d) <- toupper(names(d))
      # Add variable REO
      d <- d |>
        dplyr::mutate(REO, as.numeric(stringr::str_extract(BOP_NUM, "...$")))
      if (file.exists(file)) {
        unlink(file)
      }
      if (file.exists(file.phone.barometer.rar)) {
        unlink(file.phone.barometer.rar)
      }
    }
    # Arrange the barometer to process
    # Arrange factors
    if (!raw) { # Transform SPSS labels into proper R factors
      is_haven_labelled <- function(x) ifelse(length(which(class(x) %in%
                                                           "haven_labelled")) > 0,
                                              TRUE, FALSE)
      d <- d |>
        dplyr::mutate_if(is_haven_labelled, haven::as_factor, levels = "labels")
    }
    # Add complementary variables (date, ...)
    if (complementary_variables) {
      d <- d |>
        dplyr::mutate(Data = as.Date(paste(ifelse(is.na(DIA), 28, DIA),
                                           sprintf("%02d", MES),
                                           ANY, sep = "-")))
    }
    #
    # Filter by dates
    #
    if (!is.na(date_start)) {
      d <- d |>
        dplyr::filter(Data >= date_start)
    }
    if (!is.na(date_end)) {
      d <- d |>
        dplyr::filter(Data <= date_end)
    }
  } else {
    #
    # Serve only a single, untreated REO
    #
    if (is.character(reo)) {
      if (length(reo) == 1) {
        url.reo <- CEOmetadata()$`Enllac matriu de dades`[CEOmetadata()$REO == reo]
        if (!is.na(url.reo)) {
          d <- haven::read_spss(url.reo)
        } else {
          message(paste0("There is no dataset available for REO ", reo))
        }
      } else {
        message("'reo' must pass only a single REO.")
      }
    } else {
      stop("'reo' must be a character vector.")
    }
  }
  #
  return(d)
}

