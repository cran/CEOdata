## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
library(CEOdata)
library(tibble)
library(dplyr)
library(haven)
library(stringr)

example_path <- function(filename) {
  system.file("extdata", filename, package = "CEOdata", mustWork = TRUE)
}

acc_meta <- readRDS(example_path("accumulated_meta_example.rds"))
meta <- readRDS(example_path("REO_meta_example.rds"))

to_factor_tibble <- function(path) {
  haven::read_sav(path) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(
        where(~ inherits(.x, "haven_labelled")),
        haven::as_factor
      )
    )
}

to_raw_tibble <- function(path) {
  haven::read_sav(path) |>
    tibble::as_tibble()
}

d <- to_factor_tibble(example_path("BOP_presencial_example.sav"))
d_raw <- to_raw_tibble(example_path("BOP_presencial_example.sav"))
d1145 <- to_factor_tibble(example_path("d1145_example.sav"))
d1145_raw <- to_raw_tibble(example_path("d1145_example.sav"))

CEOaccumulated_meta <- function(series = NULL, active_only = FALSE) {
  out <- acc_meta
  if (!is.null(series)) {
    out <- out |>
      dplyr::filter(codi_serie %in% series)
  }
  if (isTRUE(active_only) && "estat" %in% names(out)) {
    estat_chr <- tolower(as.character(out$estat))
    out <- out[stringr::str_detect(estat_chr, "\\bactiva\\b") %in% TRUE, , drop = FALSE]
  }
  out
}

CEOmeta <- function(reo = NULL, search = NULL, date_start = NA, date_end = NA, ...) {
  out <- meta
  if (!is.null(reo)) {
    out <- out |>
      dplyr::filter(as.character(REO) %in% as.character(reo))
  } else if (!is.null(search)) {
    cols <- intersect(c("Titol enquesta", "Titol estudi", "Objectius", "Resum", "Descriptors"), names(out))
    if (length(cols) > 0) {
      pattern <- paste(search, collapse = "|")
      out <- out |>
        dplyr::mutate(dplyr::across(dplyr::all_of(cols), ~ tolower(as.character(.x)))) |>
        dplyr::filter(
          dplyr::if_any(
            dplyr::all_of(cols),
            ~ stringr::str_detect(.x, regex(pattern, ignore_case = TRUE))
          )
        )
    }
  }

  start <- suppressWarnings(as.Date(date_start))
  end <- suppressWarnings(as.Date(date_end))
  if (!is.na(start) && "Data d'alta al REO" %in% names(out)) {
    out <- out |>
      dplyr::filter(`Data d'alta al REO` >= start)
  }
  if (!is.na(end) && "Data d'alta al REO" %in% names(out)) {
    out <- out |>
      dplyr::filter(`Data d'alta al REO` <= end)
  }
  out
}

CEOdata <- function(series = "BOP_presencial", reo = NA, raw = FALSE) {
  if (!is.na(reo)) {
    return(if (isTRUE(raw)) d1145_raw else d1145)
  }
  if (!identical(series, "BOP_presencial")) {
    stop(
      "Offline vignette example includes only series = 'BOP_presencial'.",
      call. = FALSE
    )
  }
  if (isTRUE(raw)) d_raw else d
}

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
head(CEOaccumulated_meta())

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
head(unique(CEOaccumulated_meta()$codi_serie))

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
head(CEOaccumulated_meta(series = "BOP_presencial"))

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
d <- CEOdata()
head(d)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
d <- CEOdata(series = "BOP_presencial")
head(d)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
try(CEOdata(series = "Longitudinal"))

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
d_raw <- CEOdata(series = "BOP_presencial", raw = TRUE)
head(d_raw)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
meta <- CEOmeta()
head(meta)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
available <- CEOmeta() |> dplyr::filter(microdata_available)
head(available)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
specific_reo <- CEOmeta(reo = "1145")
head(specific_reo)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
d1145 <- CEOdata(reo = "1145")
head(d1145)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
d1145_raw <- CEOdata(reo = "1145", raw = TRUE)
head(d1145_raw)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
head(CEOsearch(d1145, keyword = "democràcia"))

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
head(CEOsearch(d1145, keyword = "Catalunya", where = "values"))

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
packageVersion("CEOdata")

