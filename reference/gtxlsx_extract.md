# Look at the pieces of a built gt table

Runs gt's own build step and hands back the result as plain data frames
and lists: the rendered body, the column definitions, the stub, the row
groups, the spanners, the styles, the footnotes and the table options.

## Usage

``` r
gtxlsx_extract(x, context = "html")
```

## Arguments

- x:

  A \`gt_tbl\` object.

- context:

  Render context handed to gt's builder. \`"html"\` is what
  \[wb_add_gt()\] uses and the only value that has been exercised here.

## Value

A named list with the elements \`body\`, \`data\`, \`boxhead\`,
\`stub\`, \`groups_rows\`, \`row_groups\`, \`spanners\`, \`heading\`,
\`stubhead\`, \`styles\`, \`footnotes\`, \`source_notes\`, \`summary\`
and \`options\`.

## Details

This is the input \[wb_add_gt()\] works from. It is exported mainly so
you can see why a table came out the way it did, or check what a \`gt\`
feature leaves behind before it reaches the worksheet.

## Examples

``` r
library(gt)

tbl <- gt(data.frame(a = 1:2, b = c(1.5, 2.5)))
tbl <- fmt_number(tbl, columns = "b", decimals = 1)

g <- gtxlsx_extract(tbl)
g$body
#>   a   b
#> 1 1 1.5
#> 2 2 2.5
g$boxhead$var
#> [1] "a" "b"
```
