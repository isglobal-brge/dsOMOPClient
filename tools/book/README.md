# Book build files

Readers of the tutorial book connect to an existing Opal server; they need none
of these files. The book's outputs are produced with a local DataSHIELD server
(DSLite) that holds the same resources as that Opal. Each chapter opens it in a
hidden setup chunk.

- `load-practice.R` loads the public synthetic GiBleed and Synthea27Nj datasets;
  it uses `load-numeric.R`.
- `connection.R` opens a resource on the local server with
  `book_connect("gibleed")`, `book_connect("numeric")` or
  `book_connect("questionnaire", ...)`; `book_switch()` and `book_disconnect()`
  close or replace it.
- `practice.R` supplies the local server for GiBleed and Synthea27Nj, with the
  Opal resource names `omop.gibleed` and `omop.synthea`, and their two-site splits.
- `fixture.R` supplies the synthetic questionnaire study: four groups of twelve
  people, education answers that change over time, visits, and deaths in 2023 and
  2024. `two_sites = TRUE` adds a second site with different answers and wording;
  `categories`, `drug_histories` and `formulas` add the teaching vocabulary,
  medicine supply histories, and serum creatinine with repeated same-day weights.
- `display.R` prints a data frame or matrix left at the end of a step as a table
  when the book is knitted. Its `book_*` display functions are no longer used by
  the chapters.
- `case-helpers.R` and `capstone.R` are kept for maintainer checks.
- `EunomiaDatasets-LICENSE.txt` is the licence for the public synthetic datasets.

The questionnaire medicine (950001) supports new-user and supply examples.
Questionnaire body weight (950002) and kilogram (9529) support numeric examples;
with the formula option, twelve people have two weights on the same day.
These local teaching concepts are separate from the public clinical datasets.
