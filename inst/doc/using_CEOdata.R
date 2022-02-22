## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
library(CEOdata)

## ----message = FALSE----------------------------------------------------------
library(CEOdata)
d <- CEOdata()

## -----------------------------------------------------------------------------
dim(d)

## -----------------------------------------------------------------------------
d

## -----------------------------------------------------------------------------
names(d)[1:50]

## -----------------------------------------------------------------------------
d746 <- CEOdata(reo = "746")
d746

## -----------------------------------------------------------------------------
b2019 <- CEOdata(date_start = "2019-01-01", date_end = "2019-12-31")
b2019

## -----------------------------------------------------------------------------
tail(names(d))

## -----------------------------------------------------------------------------
CEOmeta()

## -----------------------------------------------------------------------------
CEOmeta(search = "Medi ambient")

## -----------------------------------------------------------------------------
CEOmeta(search = c("Medi ambient", "Municipi"))

## -----------------------------------------------------------------------------
CEOmeta(date_start = "2019-01-01", date_end = "2019-12-31")

## ---- eval = FALSE------------------------------------------------------------
#  CEOmeta(search = "Medi ambient a", browse = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  CEOmeta(search = "Medi ambient a", browse = TRUE, browse_translate = "de")

## ---- message = FALSE, warning = FALSE----------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

## -----------------------------------------------------------------------------
d |>
  count(SEXE)

## ----prop-females, fig.width = 8, fig.height = 4, fig.cap = 'Proportion of females in the different Barometers.'----
d |>
  group_by(BOP_NUM) |>
  summarize(propFemales = length(which(SEXE == "Dona")) / n()) |>
  ggplot(aes(x = BOP_NUM, y = propFemales, group = 1)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  expand_limits(y = c(0, 1))

## ----tags, fig.width = 6, fig.height = 6, fig.cap = 'Prevalence of topics covered.'----
tags <- CEOmeta() |>
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

## ----fieldwork, fig.width = 8, fig.height = 10, fig.cap = 'Fieldwork periods.'----
CEOmeta() |>
  filter(`Dia inici treball de camp` > "2018-01-01") |>
  ggplot(aes(xmin = `Dia inici treball de camp`,
             xmax = `Dia final treball de camp`,
             y = reorder(REO, `Dia final treball de camp`))) +
  geom_linerange() +
  xlab("Date") + ylab("Surveys with fieldwork") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

