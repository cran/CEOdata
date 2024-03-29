% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CEOmeta.R
\encoding{UTF-8}
\name{CEOmeta}
\alias{CEOmeta}
\title{Import metadata from the "Centre d'Estudis d'Opinio"}
\usage{
CEOmeta(
  reo = NULL,
  search = NULL,
  date_start = NA,
  date_end = NA,
  browse = FALSE,
  browse_translate = NULL,
  browse_force = FALSE
)
}
\arguments{
\item{reo}{Character vector of length one that allows to get the metadata only of a specific REO (Registre d'Estudis d'Opinio, the internal register ID used by the CEO) to download. When not NULL it has precedence with the search, date_start and date_end arguments.}

\item{search}{Character vector with keywords to look for within several columns of the CEO metadata (title, summary, objectives and tags -descriptors-). Each element of the vector is strictly evaluated (all words are considered to be found in the format they appear, like in "AND"), while by using several elements in the vector the search works like an "OR" clause. Lower or upper cases are not considered.}

\item{date_start}{Character vector with a starting date ("YYYY-MM-DD") for the data.}

\item{date_end}{Character vector with an end date ("YYYY-MM-DD") for the data.}

\item{browse}{Logical value. When turned to TRUE, the browser opens the URLs of the required surveys. Only a maximum of 10 entries are opened.}

\item{browse_translate}{When opening the relevant entries in the browser (browse must be TRUE), use automatic translation to the language specified using Google Translate ('oc' for Occitan/Aranese, 'de' to German, 'en' to English, 'eu' to Basque, 'gl' for Galician or 'sp' to Spanish).}

\item{browse_force}{Logical value. When TRUE it overcomes the limitation of only opening a maximum of 10 URLs. Use it with caution.}
}
\value{
A tibble with the metadata of the surveys produced by the CEO.
}
\description{
Easy and convenient access to the metadata of the "Centre
d'Estudis d'Opinio", the Catalan institution for polling and public opinion.
It allows to search for specific terms to obtain the details of the datasets available
}
\examples{
\dontrun{
# Retrieve the metadata of the surveys ever produced by the CEO:
meta <- CEOmeta()
dim(meta)

# Search for specific terms in any of the metadata fields
# in this case, "internet".
CEOmeta(search = "internet")

# now for the combination of "Medi" AND "Ambient"
CEOmeta(search = "Medi ambient")

# now for the combination of ("Medi" AND "Ambient") OR "Municipi"
CEOmeta(search = c("Medi ambient", "Municipi"))

# Search for all registers starting in 2020
CEOmeta(date_start = "2020-01-01")

# Get the entry for a specific study (REO) and open its description in a browser
CEOmeta(reo = "746", browse = TRUE)
}
}
