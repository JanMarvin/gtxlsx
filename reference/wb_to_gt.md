# Turn a worksheet range back into a gt table (experimental)

Reads a range of cells and builds a \`gt\` object from it: full width
merged rows at the top become the heading, partly merged rows above the
labels become spanners, full width merged rows at the bottom become
source notes, and per-cell fills, fonts and alignment are translated
into \`gt::tab_style()\` calls.

## Usage

``` r
wb_to_gt(
  wb,
  sheet = current_sheet(),
  dims = NULL,
  styles = TRUE,
  structure = TRUE,
  ...
)
```

## Arguments

- wb:

  A \`wbWorkbook\` object.

- sheet:

  The worksheet to read.

- dims:

  Range to read. Defaults to the used range of the sheet.

- styles:

  Translate cell styles into \`gt::tab_style()\` calls. This is done
  cell by cell, so it is slow on large ranges.

- structure:

  Read merged cells as heading, spanners and source notes. With
  \`FALSE\` the range is taken as a plain table.

- ...:

  Passed on to \[openxlsx2::wb_to_df()\].

## Value

A \`gt_tbl\` object.

## Please read this before using it

This function is a development toy, not a finished feature. It exists
because the reverse direction was interesting to try, and it has had
only light testing — a handful of sheets, no round trip guarantees.
Treat its output as a starting point you will edit, not as a faithful
copy, and expect the details to change or the function to be withdrawn.

A worksheet simply does not record most of what a \`gt\` table knows.
Row groups, the stub, footnote marks and number formats do not come
back: groups arrive as ordinary rows, the stub as a column named after
its letter, footnote marks glued to the text they mark, and
\`\$115,900\` as the bare number \`115900\` with no \`fmt_currency()\`
behind it. Column names are made unique, so repeated labels gain a
suffix.

## Examples

``` r
library(openxlsx2)

wb <- wb_workbook()$add_worksheet()
wb$add_data(x = data.frame(a = c("x", "y"), b = c(1, 2)))

tbl <- wb_to_gt(wb, dims = "A1:B3", styles = FALSE)
class(tbl)
#> [1] "gt_tbl" "list"  
```
