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
  features = TRUE,
  freeze = FALSE,
  ...
)
```

## Arguments

- wb:

  A \`wbWorkbook\` object.

- x:

  HTML: a string, a file path, an already parsed document, or anything
  with an \`as.character()\` method that returns HTML. That includes
  what \`rvest\` and \`xml2\` hand back, so a scraped page or a single
  \`\<table\>\` node can be passed straight in.

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

  Pick up block elements sitting beside the table, such as a heading
  above it or a note below, and write them as merged rows. Set to
  \`FALSE\` to write the table on its own.

- features:

  What to write besides the values. \`TRUE\`, the default, is all of
  them; \`FALSE\` writes values only. Otherwise a character vector of
  any of \`"font"\`, \`"fill"\`, \`"border"\`, \`"numfmt"\`, \`"merge"\`
  and \`"link"\`. A page whose CSS or links go wrong in one respect can
  still be written in every other.

- freeze:

  Freeze panes so the header rows and any leading \`\<th\>\` column stay
  in view while scrolling. \`TRUE\` works them out from the table, a
  length-two vector \`c(row, col)\` freezes at a cell of your choosing,
  and \`FALSE\`, the default, leaves the sheet alone.

- ...:

  Currently unused.

## Value

The workbook, invisibly.

## Details

This is the general path for tables that are already HTML, whatever
produced them. \`openxlsx2\` is the only thing it needs; \`gt\` is a
suggestion and nothing on this path uses it.

Old fashioned presentational markup is understood too: \`bgcolor\`,
\`align\`, \`valign\`, \`width\`, \`nowrap\` and \`\<table border\>\`.

## How much CSS is understood

Enough for tables, not enough to call it a browser. A selector is
matched by walking its components against the cell and the elements
above it, so \`table.report td.total\`, \`thead td\` and \`div \> td\`
all mean what they say. Nested rules, \`:is()\`, custom properties and
\`!important\` are handled, as are the positional pseudo-classes
\`:first-child\`, \`:last-child\`, \`:only-child\` and \`:nth-child()\`.

An \`\<a href\>\` inside a cell becomes a hyperlink on that cell, and
the whole cell is what becomes clickable: a spreadsheet has no way to
link part of a cell's text. The first usable anchor is taken, since a
cell holds one target, and a link that only points at a fragment of the
source page is skipped. Either of those produces a warning naming how
many were dropped.

What is not: sibling combinators (\`+\`, \`~\`), state pseudo-classes
and \`::before\` cause a rule to be skipped rather than guessed at,
attribute selectors match on the tag alone, \`@media\` conditions are
ignored, and stylesheets pulled in with \`\<link\>\` are not fetched.

Properties with no spreadsheet equivalent, such as gradients, letter
spacing and rounded corners, are dropped. \`\<img\>\` and \`\<svg\>\`
leave an empty cell, and \`\<a href\>\` keeps its text but not the link.

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
