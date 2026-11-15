# Tutorial helpers

Download the helper ZIP from Getting started and keep its `tools/book/` folder
inside your working directory.

- `load-practice.R` loads the public synthetic GiBleed and Synthea27Nj datasets;
  it uses `load-numeric.R`.
- `connection.R` opens a practice connection with `book_connect("gibleed")` or
  `book_connect("numeric")`. Use `book_switch(name)` to close it and open another
  resource, then set `conns <- practice$conns` from the returned object. Existing
  Opal settings survive re-sourcing; see the chapter's analyst login example.
- `practice.R` supplies the local practice server used by `book_connect()`.
- `fixture.R` supplies the synthetic questionnaire through
  `book_connect("questionnaire")` and describes its inputs with
  `book_fixture_structure()`. It has four groups of twelve people, education
  answers that change over time, visits, and deaths in 2023 and 2024. Optional
  `two_sites = TRUE` supplies different education levels and wording;
  `formulas = TRUE` adds serum creatinine and repeated same-day weights.
- `display.R` formats released results: `book_counts()` for one-way counts,
  `book_table2()` for two-way counts, `book_columns()` for selected column
  metadata, and `book_symbols()` for the names of assigned tables.
  `book_summary("D_people$age", datasources = conns)` includes valid and missing
  counts. To format a saved `ds.summary()` result, also pass the corresponding
  `ds.numNA()` result as `missing`. `book_group_comparison()` shows category
  counts and states the overall mean once on its own row. In a knitted report,
  such as this book, a data frame or matrix left at the end of a step prints as
  a table.
- `case-helpers.R` and `capstone.R` support the case studies and capstone.
- `EunomiaDatasets-LICENSE.txt` is the licence for the public synthetic datasets.

The questionnaire medicine (950001) supports new-user and supply examples.
Questionnaire body weight (950002) and kilogram (9529) support numeric examples;
with the formula option, twelve people have two weights on the same day.
These local teaching concepts are separate from the public clinical datasets.
