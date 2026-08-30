# gtxlsx

Write **gt** tables — and plain HTML tables — into an **openxlsx2**
workbook.

`gt` renders beautiful tables for the web. `openxlsx2` writes real
spreadsheets. `gtxlsx` is the bit in between: it takes the table you
already built and lays it out as cells, keeping the heading, the column
spanners, the row groups, the stub, the summary rows, the footnotes and
the styling you set with
[`tab_style()`](https://gt.rstudio.com/reference/tab_style.html) and
[`tab_options()`](https://gt.rstudio.com/reference/tab_options.html).

Numbers stay numbers. If gt shows `$1,234.50`, the cell holds `1234.5`
with the number format `"$"#,##0.00` attached, so the sheet is still
something you can compute with.

## Maintenance

I am probably the wrong person to be building this. I do not use `gt`
myself, so I am likely to miss changes on that side and to have no feel
for what its users actually need. This would be in better hands with
someone who works with `gt` day to day, and I would happily hand it
over.

## Installation

``` r

# install.packages("pak")
pak::pak("JanMarvin/gtxlsx")
```

## A gt table

``` r

library(gt)
library(openxlsx2)
library(gtxlsx)

tbl <- gt(head(gtcars[, c("mfr", "model", "year", "msrp")])) |>
  tab_header(title = md("The Cars of **gtcars**")) |>
  fmt_currency(columns = msrp, decimals = 0) |>
  tab_style(
    style = list(cell_fill(color = "lightblue"), cell_text(weight = "bold")),
    locations = cells_body(columns = msrp, rows = msrp > 100000)
  )

wb <- wb_workbook()$add_worksheet(grid_lines = FALSE)
wb <- wb_add_gt(wb, tbl, dims = "B2")

if (interactive()) wb$open()
```

The title lands as a merged, bold, larger row; `**gtcars**` keeps its
bold run inside the cell; the expensive cars get the blue fill; and
`msrp` is a numeric column formatted as currency.

## An HTML table

Anything that produces an HTML `<table>` can go the same way. Inline
`<style>` rules and `style=` attributes are read, `colspan` and
`rowspan` become merged ranges, and markup inside a cell becomes rich
text.

``` r

library(openxlsx2)
library(gtxlsx)

html <- '
<style>
  th { background-color: #204060; color: white; }
  td.num { text-align: right; }
</style>
<table>
  <tr><th>Account</th><th>Change</th></tr>
  <tr><td>Cash</td><td class="num">1,204.50</td></tr>
  <tr><td><b>Net</b></td><td class="num">-2,705.50</td></tr>
</table>'

wb <- wb_workbook()$add_worksheet()
wb <- wb_add_html(wb, html, dims = "A1")
```

## What is covered

Everything `gt` applies before rendering comes through, because `gtxlsx`
reads the table gt has already built: every `fmt_*()` and `sub_*()`, the
`cols_merge_*()` family,
[`text_transform()`](https://gt.rstudio.com/reference/text_transform.html),
[`data_color()`](https://gt.rstudio.com/reference/data_color.html),
[`summary_rows()`](https://gt.rstudio.com/reference/summary_rows.html),
spanners, footnote marks,
[`opt_stylize()`](https://gt.rstudio.com/reference/opt_stylize.html) and
the rest of
[`tab_options()`](https://gt.rstudio.com/reference/tab_options.html).

What cannot survive a spreadsheet does not: anything gt draws as a
picture.
[`fmt_image()`](https://gt.rstudio.com/reference/fmt_image.html) and
[`cols_nanoplot()`](https://gt.rstudio.com/reference/cols_nanoplot.html)
leave the cell empty,
[`fmt_icon()`](https://gt.rstudio.com/reference/fmt_icon.html) and
[`fmt_flag()`](https://gt.rstudio.com/reference/fmt_flag.html) fall back
to their label text, and
[`fmt_url()`](https://gt.rstudio.com/reference/fmt_url.html) keeps the
link text but not the link.

[`wb_to_gt()`](reference/wb_to_gt.md) goes the other way, from a
worksheet range back to a `gt` object. It is a development toy rather
than a finished feature — see its help page before relying on it.

## See also

`inst/examples/` holds three scripts: a gallery of gt tables covering
summaries, colours, merged columns, nested spanners and theming; a set
of HTML dialects from several table packages; and a coverage probe that
runs most of the `gt` API through the writer.
