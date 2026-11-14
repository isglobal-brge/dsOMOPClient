# Learn dsOMOPClient

Build useful study tables while patient records stay on the server. This
book teaches the released **2.10.4 native recipe API**, one small step
at a time. Every worked lesson includes executed code, real synthetic
results and an exercise with its solution.

[Start with chapter 0
→](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md)

## Your path through the book

| Chapter                                                                                                       | What you will do                                                 |
| ------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------- |
| [0. Getting started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md)                  | Install the client and open a practice session                   |
| [1. DataSHIELD and OMOP](https://isglobal-brge.github.io/dsOMOPClient/articles/book-01-mental-model.md)       | Learn what you can see and what remains private                  |
| [2. Connecting](https://isglobal-brge.github.io/dsOMOPClient/articles/book-02-connect.md)                     | Attach a resource and use a session symbol                       |
| [3. Exploring the CDM](https://isglobal-brge.github.io/dsOMOPClient/articles/book-03-explore.md)              | Read tables, columns and safe counts                             |
| [4. Vocabulary](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md)                  | Find meanings and build concept sets                             |
| [5. A first recipe](https://isglobal-brge.github.io/dsOMOPClient/articles/book-05-recipe.md)                  | Create education and cause-of-death columns                      |
| [6. Visit dates](https://isglobal-brge.github.io/dsOMOPClient/articles/book-06-visits.md)                     | Use offsets, calendar dates and visit rows                       |
| [7. Verbal-autopsy capstone](https://isglobal-brge.github.io/dsOMOPClient/articles/book-07-verbal-autopsy.md) | Combine answers, completed age, visit periods and cause of death |

The main lessons use one synthetic DataSHIELD connection. A short
[two-vocabulary
appendix](https://isglobal-brge.github.io/dsOMOPClient/articles/book-08-harmonise.md)
demonstrates category harmonisation. [Reproduction and migration
notes](https://isglobal-brge.github.io/dsOMOPClient/articles/book-09-reproduce.md)
explain how the examples run and how older recipe conventions differ.

## Learn with real execution

The displayed results come from DSLite running the matching released
server and client against the package’s synthetic colleague CDM. The
verbal-autopsy example uses the released native follow-up recipe,
including exact age at death and calendar precision. The optional local
Opal/GiBleed instructions explain the production-shaped connection
route; GiBleed is a different dataset.

Read the book in order. Use the function reference when you need
argument details. The older reference guides are retained for existing
links and are not the book’s learning path. This is an OHDSI-inspired
theme, not an OHDSI endorsement.
