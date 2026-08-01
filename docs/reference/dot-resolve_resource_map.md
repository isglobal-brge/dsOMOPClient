# Resolve a resource argument into a per-server resource map

Accepts a single resource name (applied to every server), a named
list/vector mapping server name to resource (servers may hold the OMOP
resource at different locations), or an unnamed vector matched
positionally to the connected servers. Validates names against the
connected servers.

## Usage

``` r
.resolve_resource_map(resource, server_names, strict = TRUE)
```

## Arguments

- resource:

  Character scalar, named list/vector, or positional vector.

- server_names:

  Character; names of the connected servers.

- strict:

  Logical; error (vs warn) on unknown/missing server mappings.

## Value

Named list of resource names, one per server (NULL for unmapped).
