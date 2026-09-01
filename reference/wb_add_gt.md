# Write a gt table into a worksheet

Lays a \`gt\` table out as cells: the heading, the column spanners, the
column labels, the stub, the row groups, the body, any summary rows, the
footnotes and the source notes, one after another in a single
rectangular block starting at \`dims\`.

## Usage

``` r
wb_add_gt(
  wb,
  x,
  sheet = current_sheet(),
  dims = "A1",
  numeric = TRUE,
  col_widths = "auto",
  row_heights = NULL,
  ignore_errors = TRUE,
  ...
)
```

## Arguments

- wb:

  A \`wbWorkbook\` object, as returned by \[openxlsx2::wb_workbook()\].

- x:

  A \`gt_tbl\` object.

- sheet:

  The worksheet to write to. Defaults to the current sheet.

- dims:

  Cell reference of the top left corner of the table, for example
  \`"B2"\`.

- numeric:

  Write numbers as numbers where the displayed format can be reproduced.
  Set to \`FALSE\` to write every cell as text.

- col_widths:

  \`"auto"\` measures the rendered text and sizes the columns to fit it,
  a numeric vector sets the widths directly, and \`NULL\` leaves them
  alone. Widths set with \`gt::cols_width()\` always win.

- row_heights:

  \`NULL\`, the default, leaves Excel to size the rows. \`"gt"\` sets
  each row from the padding gt would have used around it, and a numeric
  vector sets the heights directly. Either of those also centres the
  text vertically, because Excel aligns to the bottom of a cell while gt
  pads above and below equally; without that the extra height would all
  appear as space above the text. Rows holding wrapped text keep Excel's
  own sizing, since a fixed height would clip them.

- ignore_errors:

  Mark text cells whose content looks like a number or a date, so Excel
  stops showing the green warning triangle on them.

- ...:

  Currently unused.

## Value

The workbook, invisibly. The input workbook is not modified; a clone is
returned, as elsewhere in \`openxlsx2\`.

## Details

Everything \`gt\` applies before rendering is already in place when the
cells are written, because \`gtxlsx\` reads the table gt has built
rather than repeating the work: every \`fmt\_\*()\` and \`sub\_\*()\`,
the \`cols_merge\_\*()\` family, \`text_transform()\`, \`data_color()\`,
\`summary_rows()\` and the footnote marks. Styling set with
\`gt::tab_style()\` and \`gt::tab_options()\` becomes fonts, fills,
alignment and borders; markup inside a cell (bold, italic, superscripts,
line breaks) becomes rich text.

Anything gt draws as a picture cannot be written to a cell.
\`gt::fmt_image()\` and \`gt::cols_nanoplot()\` leave the cell empty,
\`gt::fmt_icon()\` and \`gt::fmt_flag()\` fall back to their label text,
and \`gt::fmt_url()\` keeps the link text but not the hyperlink.

## Row striping

A striped table gets a fill on every body row, the striping colour on
one and \`table.background.color\` on the next. Excel leaves an unfilled
cell transparent, so filling only half the rows would show the banding
as detached blocks rather than a continuous column.

The colour comes from gt, and gt's default is white. On a worksheet with
a coloured background that white will cover the tint under the table.
Set \`table.background.color\` to match, or turn striping off, if that
matters.

## Numbers versus text

With \`numeric = TRUE\` a column is written as numbers whenever an Excel
number format can reproduce exactly what gt displays. \`\$1,234.50\`
becomes the value \`1234.5\` with the format \`"\$"#,##0.00\`, so the
sheet stays usable for arithmetic. Columns gt has scaled or suffixed
(\`1.2K\` for \`1200\`) cannot be reproduced that way and stay text;
those cells are marked so Excel does not flag them with its green
"number stored as text" indicator.

## See also

\[wb_add_html()\] for tables that are already HTML, and
\[gtxlsx_extract()\] to see the pieces \`wb_add_gt()\` works from.

## Examples

``` r
library(gt)
library(openxlsx2)

tbl <- gt(data.frame(item = c("Cash", "Debt"), amount = c(1204.5, -3910)))
tbl <- fmt_currency(tbl, columns = "amount", decimals = 2)
tbl <- tab_header(tbl, title = "Balance")

wb <- wb_workbook()$add_worksheet()
wb <- wb_add_gt(wb, tbl, dims = "B2")

wb_to_df(wb, col_names = FALSE)
#>         B      C
#> 2 Balance   <NA>
#> 3    item amount
#> 4    Cash 1204.5
#> 5    Debt  -3910
```
