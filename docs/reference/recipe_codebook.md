# Describe output tables without querying patient data

Describe output tables without querying patient data

## Usage

``` r
recipe_codebook(x, output = NULL, component = NULL)
```

## Arguments

  - x:
    
    A bound recipe or execution result carrying release metadata.

  - output:
    
    Optional exact output name.

  - component:
    
    Optional exact component name.

## Value

An ordered named list of codebook-1 records, with a columns data frame.

## Details

This is a pure client operation. Prospective records explicitly mark
unresolved schemas; published records use only admitted release
metadata.
