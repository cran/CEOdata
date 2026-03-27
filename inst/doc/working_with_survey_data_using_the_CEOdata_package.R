## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
library(CEOdata)
library(knitr)
library(tibble)
library(dplyr)
library(haven)

example_path <- function(filename) {
  system.file("extdata", filename, package = "CEOdata", mustWork = TRUE)
}

d <- haven::read_sav(example_path("BOP_presencial_example.sav")) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    dplyr::across(
      where(~ inherits(.x, "haven_labelled")),
      haven::as_factor
    )
  )
d_available <- all(c("SEXE", "BOP_NUM") %in% names(d))
meta <- readRDS(example_path("REO_meta_example.rds"))
meta_tags_available <- all(c("Descriptors", "REO") %in% names(meta))
meta_fieldwork_available <- all(
  c("Dia inici treball de camp", "Dia final treball de camp", "REO", "microdata_available") %in% names(meta)
)

## ----message = FALSE, echo = TRUE, eval = FALSE-------------------------------
# library(CEOdata)
# d <- haven::read_sav("../data/BOP_presencial_example.sav")

## ----message = FALSE, warning = FALSE-----------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

## ----eval = d_available-------------------------------------------------------
d |>
  count(SEXE)

## ----prop-females, eval = d_available, fig.width = 8, fig.height = 4, fig.cap = 'Proportion of females in the different Barometers.'----
d |>
  group_by(BOP_NUM) |>
  summarize(propFemales = length(which(SEXE == "Femení")) / n()) |>
  ggplot(aes(x = BOP_NUM, y = propFemales, group = 1)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  expand_limits(y = c(0, 1))

## ----tags, eval = meta_tags_available, fig.width = 6, fig.height = 6, fig.cap = 'Prevalence of topics covered.'----
tags <- meta |>
  separate_rows(Descriptors, sep = ";") |>
  mutate(tag = factor(stringr::str_trim(Descriptors))) |>
  select(REO, tag)

tags |>
  group_by(tag) |>
  count() |>
  filter(n > 5) |>
  ggplot(aes(x = n, y = reorder(tag, n))) +
    geom_point() +
    ylab("Topic")

## ----fieldwork, eval = meta_fieldwork_available, fig.width = 8, fig.height = 10, fig.cap = 'Fieldwork periods.'----
meta |>
  filter(`Dia inici treball de camp` > "2018-01-01") |>
  ggplot(aes(xmin = `Dia inici treball de camp`,
             xmax = `Dia final treball de camp`,
             y = reorder(REO, `Dia final treball de camp`),
             color = microdata_available)) +
  geom_linerange() +
  xlab("Date") + ylab("Surveys with fieldwork") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

## ----eval = d_available-------------------------------------------------------
survey.data <- d |>
  mutate(Female = ifelse(SEXE == "Dona", 1, 0),
         Age = EDAT,
         # Pass NA correctly
         Income = ifelse(INGRESSOS_1_15 %in% c("No ho sap", "No contesta"), 
                         NA,
                         INGRESSOS_1_15),
         Date = DATA_FIN,
         # Reorganize factor labels
         `Place of birth` = factor(case_when(
            LLOC_NAIX == "Catalunya" ~ "Catalonia",
            LLOC_NAIX %in% c("No ho sap", "No contesta") ~ as.character(NA),
            TRUE ~ "Outside Catalonia")),
         # Convert into numerical (integer)
         `Interest in politics` = case_when(
            INTERES_POL == "Gens" ~ 0L,
            INTERES_POL == "Poc" ~ 1L,
            INTERES_POL == "Bastant" ~ 2L,
            INTERES_POL == "Molt" ~ 3L,
            TRUE ~ as.integer(NA)),
         # Convert into numeric (double) and properly address missing values
         `Satisfaction with democracy` = ifelse(
            SATIS_DEMOCRACIA %in% c("No ho sap", "No contesta"),
            NA,
            as.numeric(SATIS_DEMOCRACIA))) |>
  # Center income to the median
  mutate(Income = Income - median(Income, na.rm = TRUE)) |>
  # Pick only specific variables
  select(Date, Female, Age, Income,
         `Place of birth`, `Interest in politics`, 
         `Satisfaction with democracy`)



## ----eval = FALSE-------------------------------------------------------------
# save(survey.data, file = "my_cleaned_dataset.RData")

## ----eval = FALSE, echo = TRUE------------------------------------------------
# library(vtable)
# st(survey.data)

## ----eval = exists("survey.data"), echo = FALSE-------------------------------
vtable::st(survey.data, out = "kable")

## ----eval = FALSE, echo = TRUE------------------------------------------------
# library(compareGroups)
# createTable(compareGroups(Female ~ . -Date, data = survey.data))

## ----eval = exists("survey.data"), echo = FALSE-------------------------------
library(compareGroups)
createTable(compareGroups(Female ~ . -Date, data = survey.data))

