# Constructs that make a recipe need a v2 plan

Language-2 objects, and `parent_id` (whose v1 rendering silently ignored
inheritance, defect B-8). A recipe without them is v1-expressible: it
executes as its unchanged 2.10.1 plan (plan execution itself needs
dsOMOP 2.10.3 servers: the publication status methods).

## Usage

``` r
.recipe_needs_v2(recipe)
```
