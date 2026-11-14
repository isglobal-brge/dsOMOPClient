# Learn dsOMOPClient

Build useful study tables while patient records stay on the server. This
book teaches the released **2.10.4 native recipe API**, one small step
at a time. Every worked lesson includes executed code, real synthetic
results and an exercise with its solution.

[Start with chapter 0
→](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md)

## Your path through the book

| Chapter                                                                                                        | What you will do                                          |
| -------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------- |
| [0. Getting started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md)                   | Install and open a practice session                       |
| [1. DataSHIELD and OMOP](https://isglobal-brge.github.io/dsOMOPClient/articles/book-01-mental-model.md)        | Understand what stays private                             |
| [2. Connecting](https://isglobal-brge.github.io/dsOMOPClient/articles/book-02-connect.md)                      | Attach a resource and use symbols                         |
| [3. Exploring the CDM](https://isglobal-brge.github.io/dsOMOPClient/articles/book-03-explore.md)               | Read schema and safe counts                               |
| [4. Vocabulary](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md)                   | Find concepts and build sets                              |
| [5. A first recipe](https://isglobal-brge.github.io/dsOMOPClient/articles/book-05-recipe.md)                   | Create education and cause columns                        |
| [6. Events, windows and reducers](https://isglobal-brge.github.io/dsOMOPClient/articles/book-06-events.md)     | Choose records, time and one-value summaries              |
| [7. Populations](https://isglobal-brge.github.io/dsOMOPClient/articles/book-07-populations.md)                 | Define eligibility, episodes, new users and matching      |
| [8. Output formats](https://isglobal-brge.github.io/dsOMOPClient/articles/book-08-outputs.md)                  | Choose shapes, censoring and joins                        |
| [9. Dates](https://isglobal-brge.github.io/dsOMOPClient/articles/book-09-dates.md)                             | Use parts, periods, anchored ages and partial dates       |
| [10. Concepts as categories](https://isglobal-brge.github.io/dsOMOPClient/articles/book-08-harmonise.md)       | Translate, recode, rename and harmonise two sites         |
| [11. Reusable studies and verbs](https://isglobal-brge.github.io/dsOMOPClient/articles/book-11-parameters.md)  | Bind parameters and use mutate/filter                     |
| [12. Privacy](https://isglobal-brge.github.io/dsOMOPClient/articles/book-12-privacy.md)                        | Understand final release and refused columns              |
| [13. Visit walkthrough](https://isglobal-brge.github.io/dsOMOPClient/articles/book-06-visits.md)               | Combine offsets, dates and event rows                     |
| [14. Verbal-autopsy capstone](https://isglobal-brge.github.io/dsOMOPClient/articles/book-07-verbal-autopsy.md) | Combine answers, age, visit periods and cause             |
| [15. Working with lab values](https://isglobal-brge.github.io/dsOMOPClient/articles/book-15-labs.md)           | Reduce, convert and pool synthetic lab values             |
| [16. Longitudinal data](https://isglobal-brge.github.io/dsOMOPClient/articles/book-16-longitudinal.md)         | Shape events, build risk sets and model repeated measures |

The main lessons use one DataSHIELD connection at a time. GiBleed
teaches the basics; Synthea27Nj supplies numeric labs. Both have
synthetic people. A separately labelled 24-person test fixture supplies
the colleague’s questionnaire examples. Chapter 10 includes a
two-connection vocabulary harmonisation demonstration. Chapter 15 pools
numeric summaries and a Gaussian model across disjoint synthetic sites.
[Reproduction and migration
notes](https://isglobal-brge.github.io/dsOMOPClient/articles/book-09-reproduce.md)
explain how the examples run and how older recipe conventions differ.
The visit walkthrough and capstone retain their original URLs so earlier
links continue to work.

## Learn with real execution

The displayed results come from DSLite running the matching released
server and client against the chapter’s stated synthetic dataset. The
verbal-autopsy example uses the released native follow-up recipe,
including exact age at death and calendar precision. The optional local
Opal/GiBleed instructions explain the production-shaped connection
route; GiBleed is a different dataset.

Read the book in order. Use the function reference when you need
argument details. The older reference guides are retained for existing
links and are not the book’s learning path. This is an OHDSI-inspired
theme, not an OHDSI endorsement.
