# How gtxlsx works

This is the internals document: what the package does with a table between
`wb_add_gt(wb, tbl)` and a cell landing on a worksheet. The user-facing
description is in `README.md` and the help pages.

## The shape of the problem

`gt` and `openxlsx2` disagree about almost everything.

A gt table is a document. It has a heading, a stub, row groups, spanners
stacked several levels deep, footnotes with marks, summary rows interleaved
with data rows. Its styling is CSS: cascading, inherited, expressed in
pixels and percentages, with borders on four sides of every element.

A worksheet is a grid. Every cell is at a row and a column, holds one value,
and points at one style record. There is no nesting, no cascade, and no
inheritance. Merged ranges are the only structure available.

So the work splits in three:

1. **Flatten** the document into a grid — decide which sheet row and column
   every piece of the table occupies.
2. **Translate** CSS into the small set of things a cell style can hold:
   font, fill, alignment, four border sides, a number format.
3. **Write** the result without doing it one cell at a time, because that is
   slow.

The same three problems appear again for plain HTML input, which is why
`wb_add_html()` shares most of the machinery.

`gt` is only needed for the gt entry points; `wb_add_html()` depends on
nothing but `openxlsx2`. That is why `gt` and `markdown` sit in Suggests and
every call into them goes through `need_gt()` or a `requireNamespace()` check.

## Reading the gt table

`gtxlsx_extract()` (`R/extract.R`) is the only place that touches gt's
internals. It calls `gt:::build_data()` through `gt_build_data()` in
`R/utils.R`, then returns the built components as plain data frames.

Calling gt's builder rather than reimplementing it is the single decision
that makes the package feasible. After `build_data()` the body already has
every `fmt_*()` and `sub_*()` applied, `cols_merge()` patterns expanded,
`text_transform()` run, rows reordered into their groups, and footnote marks
attached. A probe over 79 gt functions (`inst/examples/coverage.R`) passes 78
— not because each was handled, but because almost none of them had to be.

The cost is a dependency on an unexported function. `gt_build_data()` checks
it is there and fails with an explanatory message if not, and a test asserts
that every component the package reads is still present, so a gt release that
moves things breaks the suite rather than the output.

### Whose problem that is

Ours, and it should stay ours. `build_data()` is internal, gt owes it no
stability, and the component test is a tripwire for us rather than a claim on
gt. If a release moves it, the fix belongs here.

This is also why the package sits on r-universe rather than CRAN. On CRAN, gt
would see gtxlsx in its reverse dependency checks: their maintainer would get
a red result they did not cause and cannot fix without asking us to change
something. That is the real cost of reaching into another package's
internals, and it is worth more than the packaging convenience of being on
CRAN. Off CRAN, gt never sees us and owes us nothing.

Two things follow. Bugs found in gt are reported as gt bugs, demonstrated
through gt's own output, with no mention of this package. And nothing is asked
of gt that only makes sense because of the internal access. The clean way out,
if it ever comes, is gt exporting an accessor for a built table, which would
turn this from taking something into using something.

`gtxlsx_theme()` reduces gt's ~200 `tab_options()` entries to the two dozen
values the writer needs: fonts and sizes per region, fills, weights, padding,
striping. Two details worth knowing:

- Font sizes are CSS pixels; Excel wants points. `css_pt()` converts, so gt's
  default `16px` becomes `12pt` rather than `16pt`.
- gt does not store a text colour per region. It computes one from that
  region's background with a luminance rule in `gt_colors.scss`. `luminance()`
  reproduces it, which is why a dark column-label background gets white text.

## Laying out the grid

`gtxlsx_plan()` (`R/layout.R`) decides where everything goes, before anything
is written. It returns row numbers for the title, subtitle, each spanner
level, the column labels, every body row, every group heading, every summary
block, the footnotes and the source notes, plus a `col_of()` function mapping
a gt variable to a sheet column.

Two things it gets right that are easy to get wrong:

- Spanner levels are renumbered against the *visible* columns only, after
  hidden columns are dropped, and rendered top-down so level 1 sits directly
  above the labels.
- Body rows are placed by walking `_groups_rows`, so a group heading row, its
  data rows and its summary rows are interleaved in the right order.
  `body_row[i]` maps the i-th row of the built body to a sheet row.

Because the plan exists before any writing, styles and footnotes can be
resolved to cell references directly. An earlier draft wrote rows in the wrong
order and then renumbered `sheet_data$cc` afterwards; planning first removes
the need.

## The cell buffer

Nothing is written to the workbook during layout. The writers push records
into a buffer (`R/cells.R`):

```r
put_cell(cc, row, col, text, style = st, sig = key, ...)
```

A record holds its position, its text or numeric value, and either its own
style attributes or — more usually — a reference to a shared style list plus
a `sig` identifying it. Cells in a column mostly look alike, so passing the
style by reference keeps the records small and lets the renderer group them
without deriving a signature per cell.

`merge_records()` then collapses records that target the same cell, later
writes winning. This is what lets the writers work in passes: the body writer
lays down text and column styling, `gtxlsx_apply_styles()` comes along after
with `tab_style()` overrides, and the merge sorts out the result.

`render_cells()` does the actual writing, in one pass per concern:

- Values, batched. Numeric and plain-text cells are sorted into contiguous
  column runs and written with one `add_data()` call each. Cells containing
  markup are written individually as `fmt_txt` rich strings.
- Fonts, fills, alignment and number formats, grouped by identical style so
  each distinct combination costs one call. A 9,000-cell sheet typically ends
  up with two fonts and two fills.

`group_by_sig()` builds the grouping key from a record's `sig` plus anything
set directly on it. Both halves matter: without the `sig` the key has to be
rebuilt from ten fields per cell; without the overrides, a `data_color()` fill
set per cell after the fact would be collapsed onto the whole column.

## Numbers

Writing `$1,234.50` as text produces a spreadsheet you cannot compute with, so
`infer_numfmt()` (`R/numfmt.R`) tries to recover the number and an Excel format
that displays it the same way.

It parses gt's rendered strings for a prefix, sign, integer part, grouping
mark, decimals and suffix; requires all of them to agree across the column;
builds a format such as `"$"#,##0.00` or `0.0%`; and then **checks the result
against gt's own values** before committing. The printed text must be a valid
rounding of the stored number, within half a unit of the last shown decimal.

Anything that fails the check stays text: a column gt has scaled (`1.2K` for
`1200`), inconsistent decimal counts, cells containing markup. Those cells get
an `ignoredError` entry so Excel does not flag them with its green
"number stored as text" triangle.

The HTML path has no source values to check against, so `text_as_numbers()` is
stricter: only symbol affixes such as `$` or `%` convert, which keeps a label
like `458 Speciale` from becoming the number 458.

## Text with markup

gt renders cell contents as small HTML fragments — `<em>`, `<sup>`, `<br>`,
`<span style>`, entities. `html_runs()` (`R/html.R`) tokenises these with a
style stack and returns a list of runs, which `runs_to_fmt()` turns into
openxlsx2 rich text.

Cells with no markup are written as plain strings on purpose. A cell holding
rich text ignores the cell-level font, so writing everything as `fmt_txt`
would silently break `tab_style()` fonts. Only cells that actually need runs
get them.

## Styling

Two sources have to be merged, and they work differently.

`tab_style()` entries arrive as `_styles` rows with a location, and
`style_targets()` (`R/styles.R`) maps each location — `data`, `stub`,
`columns_columns`, `columns_groups`, `row_groups`, `title`, `subtitle`,
`stubhead`, `summary_cells`, `grand_summary_cells`, `footnotes`,
`source_notes` — onto cells. `style_attrs()` converts the style object to
record attributes.

`tab_options()` never appears in `_styles`. It drives gt's stylesheet, so the
package reads the option values directly and applies them per region:
backgrounds, weights, sizes, and the border rules in `gtxlsx_borders()`. This
split is worth remembering when something looks wrong — a missing spanner
border or a wrongly bold group heading comes from the options side, and the
tests for the two sides are different (see below).

Borders need care for a reason that is not obvious. openxlsx2 treats a range
as a block, so `add_border(dims = "A4:A5", bottom_border = ...)` puts the
border on the bottom edge of the block — A5 only. `apply_borders()` therefore
collapses identical border requests into runs **across a row** for top and
bottom, and **down a column** for left and right, so every cell in the range
sits on the edge being drawn.

One deliberate departure from gt: a striped table gets a fill on every body
row, including the unstriped ones. Excel leaves an unfilled cell transparent,
and alternating filled and transparent rows reads as detached blocks rather
than banding.

## Reading HTML

`wb_add_html()` (`R/html_table.R`) solves the same three problems for input
that is already HTML. It is the general path — `wb_add_lt()` is a thin wrapper
that asks lt to bake its table to static HTML first.

**The markup.** `R/html_parse.R` reads the HTML. openxlsx2 ships an XML
parser, but HTML is not XML: `<br>`, unquoted attribute values and unclosed
`<td>` are all legal HTML and all rejected by an XML reader. Rather than add a
second XML library, the tags are scanned directly into a flat store of
elements, each holding its tag, attributes, child indices, parent index and
the span of source between its tags. Void elements close themselves, an
unclosed `<td>`, `<tr>`, `<p>` or `<li>` is closed by its successor, and stray
end tags are ignored. Roughly 170 lines, no dependencies.

**The stylesheet.** `walk_css()` parses `<style>` with a selector stack, so
nested rules and `&` references resolve, `:is()` expands to separate
selectors, and `own_decls()` separates a rule's own declarations from its
nested blocks by matching braces. Custom properties are resolved by
`resolve_vars()`, so `--bd: 1.5px solid #888` followed by
`border-top: var(--bd)` works.

**Matching.** A selector is kept as an ordered chain of components, each with
the combinator preceding it, and matched by walking the chain right to left
against the cell's ancestors: a child combinator must match the next level up,
a descendant combinator may skip any number. Order matters, so
`tfoot thead td` does not match a cell that has both above it in the wrong
sequence. Positional pseudo-classes
(`:first-child`, `:last-child`, `:only-child`, `:nth-child()`, negated or not)
are evaluated against each cell's position in its row and each row's position
in its section. Anything else — state pseudo-classes, `::before`, `:has()` —
causes the rule to be **skipped rather than applied**, because a rule applied
to every cell is worse than a rule dropped.

**The grid.** `html_grid()` walks the table's own rows in rendered order
(thead, tbody, tfoot, regardless of source order), resolving `colspan` and
`rowspan` against an occupancy matrix so cells land in the column they
actually occupy. `rowspan="0"` runs to the end of its section, per spec.

**The cascade**, in increasing precedence: browser defaults for `<th>`, the
table rule's inherited properties, `<colgroup>`, the section, the `<tr>`, the
cell's own rules and attributes, and finally a single wrapper chain
(`<td><p><span>text`) if the content sits inside one. Presentational
attributes — `bgcolor`, `align`, `valign`, `width`, `nowrap`,
`<table border>` — are translated too, since plenty of real HTML still uses
them. The whole resolved record is cached on a key built from the cell's tag,
class, style attribute, child count, ancestor chain and position, so a uniform
table resolves the cascade once.

## Several tables at once

A `gt_group`, from `gt_group()` or `gt_split()`, holds its tables as rows of a
tibble. `write_gt_group()` pulls each one back out with `gt::grp_pull()`,
plans it to learn how tall it is, writes it, and starts the next below with
`gap` blank rows in between. Each table keeps its own layout and styling; only
the starting row is shared.

## The reverse direction

`wb_to_gt()` reads a range back into a `gt` object: full-width merged rows at
the top become the heading, partly merged rows above the labels become
spanners, trailing merged rows become source notes, and cell styles become
`tab_style()` calls.

It is a development toy and its help page says so. A worksheet does not record
row groups, the stub, footnote marks or number formats, so none of those come
back.

## Testing

Three layers, because they catch different things.

*Unit tests* cover the pure functions — colour and length conversion, the
inline tokeniser, number format inference, the cell buffer.

*Fidelity tests* (`test-fidelity.R`) walk gt's `_styles` table, resolve each
entry to cells through the layout, and compare the workbook against what gt
declared. Later styles win over earlier ones, as in gt.

*Stylesheet tests* (`test-css-fidelity.R`) cover what `_styles` cannot see.
They render the table with `as_raw_html()`, parse the CSS with the package's
own reader, and compare each region — title, spanner, labels, group heading,
stub, body, summary, notes — against the cells it was written to. This is the
layer that catches `tab_options()` regressions: a forced bold, a missing
spanner border, absent column-label vertical rules.

The useful discipline is checking that a new test *fails* when the bug it was
written for is reintroduced. Several tests here looked fine and were inert
until that was done.

## Performance

Writing 9,000 cells takes roughly 3 seconds, against about 0.27s for
`openxlsx2::wb_add_data()` on the same data — the floor, since that does none
of this work.

The gap closed from an initial 113 seconds through a handful of fixes, all of
which are easy to reintroduce:

- Growing a list one element at a time is quadratic in R. It appeared in
  `merge_records()` and again in `html_grid()`.
- One `add_border()` call per cell was the largest single cost. Grouping
  identical borders into ranges removed most of it.
- Per-cell regex work on length-1 strings is slow. Text extraction, stripping
  and number parsing are vectorised over all cells at once.
- Resolving the CSS cascade per cell is slow. It is cached on a signature that
  a uniform table hits every time.

What did *not* help, each measured: caching the declaration lookup separately,
handing style calls ranges instead of ref lists, and replacing the per-cell
record lists with parallel vectors. Allocating 9,000 small lists costs about
4 milliseconds; it was never the problem.

Beyond this the remaining time is spread thinly across `render_cells()` and
openxlsx2's own per-cell style assignment, with no hotspot left to remove.
Going faster means not iterating per cell at all, which is a different
program.

## gt versions

gt moves, and the package reads its internals, so a few things are worth
knowing about the range it has to cope with. The suite has been run against
gt 1.1.0 and 1.3.0.9000. What has been checked against gt's changelog:

- **Multi-column stubs** (gt 1.2.0, 1.3.0). `cells_stub()` targets every
  column of the stub, and on a multi-column stub gt records the style under
  the location name `stub_column` rather than `stub`, one row per column.
  Both names are handled; missing the new one meant a stub style silently
  reached no cell at all.
- **`drop_leading_zero`** (development). A value printed as `.75` is still
  written as a number; the Excel format uses `#` rather than `0` so the
  leading zero stays suppressed.
- **`gt_group()` and `gt_split()`**. gt counts the tables in a group with
  `nrow(x$gt_tbls)` and that has not changed, but the count here tolerates the
  container becoming a plain list rather than returning `NULL` and silently
  writing nothing. Note that on gt 1.3.0.9000 `gt_split()` leaves every table
  after the first with `_stub_df$rownum_i` pointing past its own data, so its
  body builds as `NA` and gt's own HTML for it is empty; the sheet reproduces
  that faithfully, which is why the test compares against the built body
  rather than the data the table was given.
- **`stub.separate`** (gt 1.3.0) and **`table.no_data_message`**
  (development) are options this package does not read; both degrade to
  nothing rather than an error, because option lookups return `NA` when the
  option is absent.
- **`summary_columns()`** and **`row_order()`** need no handling: they change
  the built body, which is read as it comes.

The safety net is the component test in `test-extract.R`, which asserts that
every field read out of a built gt table is still present. A release that
moves one of them fails there rather than producing a wrong sheet.

## Known limits

- **Graphics.** `fmt_image()` and `cols_nanoplot()` leave the cell empty;
  `fmt_icon()` and `fmt_flag()` fall back to their label text; `fmt_url()`
  keeps the link text but not the hyperlink.
- **CSS.** Sibling combinators, attribute selectors, external stylesheets and
  `@media` conditions are approximated or ignored.
- **Not carried over.** `tab_caption()` (which gt does not render either),
  `opt_css()`, and the interactive parts of `opt_interactive()`. The last two
  describe a browser and a JavaScript widget; neither has a sheet equivalent
  worth inventing.
- **Row heights** are opt-in and derived from padding with an assumed line
  height; they are an approximation of the browser's `line-height: normal`.
