# Write an HTML table into a worksheet

Reads the first (or any) \`\<table\>\` of an HTML fragment or document
and writes it as cells. \`colspan\` and \`rowspan\` become merged
ranges, \`\<style\>\` rules and \`style=\` attributes become fills,
fonts, alignment and borders, and markup inside a cell becomes rich
text. Titles and notes that sit beside the table rather than inside it
are picked up as well.

## Usage

``` r
wb_add_html(
  wb,
  x,
  sheet = current_sheet(),
  dims = "A1",
  which = 1L,
  numeric = TRUE,
  col_widths = "auto",
  ignore_errors = TRUE,
  context = TRUE,
  ...
)
```

## Arguments

- wb:

  A \`wbWorkbook\` object.

- x:

  HTML: a string, a file path, or anything with an \`as.character()\`
  method that returns HTML.

- sheet:

  The worksheet to write to. Defaults to the current sheet.

- dims:

  Cell reference of the top left corner.

- which:

  Which table in the document to write, when there is more than one.

- numeric:

  Write cells as numbers where the text is plainly a number. Only symbol
  prefixes and suffixes such as \`\$\` or \` label like \`"458
  Speciale"\` stays text.

- col_widths:

  \`"auto"\` measures the rendered text, a numeric vector sets the
  widths directly, \`NULL\` leaves them alone.

- ignore_errors:

  Mark text cells that look numeric, so Excel does not flag them.

- context:

  Pick up block elements sitting beside the table — headings above it,
  notes below it — and write them as merged rows. Set to \`FALSE\` to
  write the table on its own.

- ...:

  Currently unused.

## Value

The workbook, invisibly.

## Details

This is the general path for tables that are already HTML, whatever
produced them. Old fashioned presentational markup is understood too:
\`bgcolor\`, \`align\`, \`valign\`, \`width\`, \`nowrap\` and \`\<table
border\>\`.

## How much CSS is understood

Enough for tables, not enough to call it a browser. Selectors are
matched on their rightmost part — tag, class and id — with the classes
of ancestor elements checked as well, which is what makes rules like
\`table.report td.total\` and \`thead td\` work. Nested rules, \`:is()\`
and \`!important\` are handled. What is not: \`:nth-child()\` and other
structural or state pseudo-classes are skipped rather than applied to
every cell, \`\>\` and \`+\` behave like a plain descendant combinator,
attribute selectors match on the tag alone, and stylesheets pulled in
with \`\<link\>\` are not fetched.

Properties with no spreadsheet equivalent — gradients, letter spacing,
rounded corners — are dropped. \`\<img\>\` and \`\<svg\>\` leave an
empty cell, and \`\<a href\>\` keeps its text but not the link.

## See also

\[wb_add_gt()\], which goes straight from a \`gt\` object and keeps more
of the structure.

## Examples

``` r
library(openxlsx2)

html <- paste0(
  "<style>th { background-color: #204060; color: white; }</style>",
  "<table><tr><th>Account</th><th>Change</th></tr>",
  "<tr><td>Cash</td><td>1,204.50</td></tr></table>"
)

wb <- wb_workbook()$add_worksheet()
wb <- wb_add_html(wb, html, dims = "A1")

wb_to_df(wb, col_names = FALSE)
#>         A      B
#> 1 Account Change
#> 2    Cash 1204.5
```
