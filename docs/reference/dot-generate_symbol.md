# Generate a unique temporary symbol name

Creates a random symbol by appending six alphanumeric characters to the
given prefix, separated by a dot. Used to create unique server-side
variable names that avoid collisions across sessions.

## Usage

``` r
.generate_symbol(prefix = "dsO")
```

## Arguments

- prefix:

  Character; prefix for the generated symbol.

## Value

Character; a unique symbol string (e.g., `"dsO.aB3xZq"`).
