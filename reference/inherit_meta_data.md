# Inherit Meta Data for Extinct, Local, and Observed in a Taxonomy File

A taxonomy file can optionally contain some or all of the logical
columns "extinct", "local", and "observed". The value that is assigned
to some nodes can determine the value of other nodes. As an example, if
a species is marked as local this implies that also the genus, family,
etc. that the species belongs to are local.

## Usage

``` r
inherit_meta_data(file, delim = ",")
```

## Arguments

- file:

  path to the csv file

- delim:

  the delimiter used in the file

## Value

a `taxonomy_graph` with modified columns "local", "observed", and "rank"
(if present). The file given by `file` is overwritten as a side effect.

## Details

Inheritance is only applied to columns that are already present. No new
columns are added.

For the columns "local" and "observed", inheritance works upwards: if a
taxon is local or has been observed, the same applies to all it's
parents. Note that only the value set for leaf taxa is taken into
account.

For the columns "extinct", inheritance works downwards: if a taxon is
extinct, so are all taxa below it.
