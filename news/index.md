# Changelog

## gtxlsx (development version)

- [`fmt_url()`](https://gt.rstudio.com/reference/fmt_url.html) and
  [`fmt_email()`](https://gt.rstudio.com/reference/fmt_email.html)
  produce real hyperlinks, and an `<a href>` in any HTML table does the
  same. A link that only points at a fragment of the source page is
  skipped, since it means nothing in a workbook.

- Where a cell holds several anchors, the first usable one is taken. A
  footnote marker pointing at a fragment of its own page no longer hides
  the real link beside it.

- A `<tr>` with no cells is a spacer the page uses for layout, and no
  longer lands as a blank row in the sheet.

- New `features` argument on [`wb_add_gt()`](../reference/wb_add_gt.md)
  and [`wb_add_html()`](../reference/wb_add_html.md) decides what is
  written besides the values: any of `"font"`, `"fill"`, `"border"`,
  `"numfmt"`, `"merge"` and `"link"`. `FALSE` writes values only, so a
  table that goes wrong in one respect can still be written in every
  other.

- New `freeze` argument keeps the heading and the stub in view while
  scrolling. `TRUE` works the split out from the table, `c(row, col)`
  puts it where you say.

- Links that cannot be written are reported: a warning names how many
  were dropped because a cell already had one, and how many pointed only
  into the source page.

- [`wb_add_html()`](../reference/wb_add_html.md) accepts anything with
  an [`as.character()`](https://rdrr.io/r/base/character.html) method
  that returns HTML, so what `rvest` and `xml2` hand back can be passed
  straight in: a whole page or a single `<table>` node.

## gtxlsx 0.3.0

- `gt` and `markdown` moved to Suggests, and `xml2` is gone.
  [`wb_add_html()`](../reference/wb_add_html.md) now needs nothing but
  `openxlsx2`, so using it no longer pulls in `gt`’s dependency tree.

- HTML is read by a parser in the package rather than by `xml2`. It
  handles markup an XML parser rejects: `<br>`, unquoted attribute
  values, unclosed `<td>`. Output for `gt`, `lt`, `pivottabler`,
  `kable`, `htmlTable` and `tinytable` is unchanged.

- CSS selectors are matched by walking their component chain, so
  `div > td` and `div td` mean different things. Sibling combinators are
  skipped rather than guessed at.

- [`wb_add_gt()`](../reference/wb_add_gt.md) takes a
  [`gt_group()`](https://gt.rstudio.com/reference/gt_group.html) or
  [`gt_split()`](https://gt.rstudio.com/reference/gt_split.html) object
  and writes the tables one after another, spaced by `gap`.

- Works with `gt` 1.1.0 through 1.3.0.9000. On a multi-column stub, gt
  records
  [`cells_stub()`](https://gt.rstudio.com/reference/cells_stub.html)
  styles under a different location name; that is handled now. Values
  printed without a leading zero, as `drop_leading_zero` gives, stay
  numbers.

- Minimum R version is 3.6.0. Nothing in the package needed 4.1.

## gtxlsx 0.2.0

- Fixed border and fill handling. A border spanning several rows reached
  only the last of them, and striped tables left the plain rows
  unfilled, which showed as detached blocks rather than banding.

- The CSS reader handles nested rules,
  [`var()`](https://rdrr.io/r/stats/cor.html) custom properties and
  positional pseudo-classes such as `:not(:last-child)`, so `lt` tables
  keep their borders.

- New `row_heights` argument, off by default, sets row heights from gt’s
  padding and centres the text to match.

## gtxlsx 0.1.1

- Group headings and summary labels are no longer forced bold; the
  weight follows the `gt` option.

- Spanners get the bottom border `gt` draws under them, and column
  labels get their vertical lines under `opt_table_lines("all")`.

## gtxlsx 0.1.0

- [`wb_add_gt()`](../reference/wb_add_gt.md),
  [`wb_add_html()`](../reference/wb_add_html.md),
  [`wb_add_lt()`](../reference/wb_add_lt.md) and
  [`wb_to_gt()`](../reference/wb_to_gt.md).
