suppressWarnings(suppressMessages({library(openxlsx2); library(gtxlsx)}))
d <- data.frame(name = c("Alice","Bob","Cha"), score = c(85.4, 92.1, 77.5),
                city = c("Berlin","Paris","Rome"), stringsAsFactors = FALSE)

wiki <- '<table class="wikitable sortable" style="text-align:center">
<caption>Largest cities</caption>
<colgroup><col style="width:120px"><col><col span="2" style="text-align:right"></colgroup>
<tbody>
<tr style="background:#eaecf0"><th scope="col">City</th><th>Country</th>
<th>Pop.<sup class="reference">[1]</sup></th><th>Area&nbsp;(km<sup>2</sup>)</th></tr>
<tr><td style="background:#fee">Tokyo</td><td>Japan</td><td align="right">37,400,068</td><td align="right">2,194</td></tr>
<tr><td>Delhi</td><td>India</td><td align="right">28,514,000</td><td align="right">1,484</td></tr>
<tr><td colspan="2"><i>subtotal</i></td><td align="right">65,914,068</td><td align="right">3,678</td></tr>
</tbody></table>'

legacy <- '<TABLE BORDER=2 CELLPADDING=4 BGCOLOR="#FFFFF0">
<TR BGCOLOR="#336699"><TH ALIGN=LEFT><FONT COLOR="white">Item</FONT></TH><TH>Qty</TH></TR>
<TR><TD NOWRAP>Widget &amp; bolt</TD><TD ALIGN=RIGHT>12</TD></TR>
<TR><TD BGCOLOR="#ffe4e1"><B>Gadget</B></TD><TD ALIGN=RIGHT>7</TD></TR>
</TABLE>'

tricky <- '<table>
<caption>tfoot first, rowspan 0, nested table</caption>
<tfoot><tr><td colspan="3">footer row</td></tr></tfoot>
<thead><tr><th rowspan="2">Key</th><th colspan="2">Pair</th></tr>
<tr><th>a</th><th>b</th></tr></thead>
<tbody>
<tr><td rowspan="0">spans down</td><td>1</td><td>2</td></tr>
<tr><td>3</td><td><table><tr><td>nested</td></tr></table></td></tr>
</tbody></table>'

styled <- '<style>
table.rep { font-family: Georgia, serif; font-size: 14px; color: #222; }
table.rep td, table.rep th { border-bottom: 1px solid #ccc; padding: 4px; }
table.rep thead th { background-color: #204060; color: #fff; text-align: left; }
table.rep tbody tr:nth-child(even) td { background-color: #f5f7fa; }
table.rep .num { text-align: right; font-variant-numeric: tabular-nums; }
table.rep .neg { color: #b00020; font-weight: 700; }
</style>
<table class="rep"><thead><tr><th>Account</th><th class="num">Change</th></tr></thead>
<tbody><tr><td>Cash</td><td class="num">1,204.50</td></tr>
<tr><td>Debt</td><td class="num neg">-3,910.00</td></tr>
<tr><td>Net</td><td class="num">-2,705.50</td></tr></tbody></table>'

pivot <- NULL
if (requireNamespace("pivottabler", quietly = TRUE)) {
  set.seed(1)
  pv <- data.frame(
    toc = rep(c("Arriva", "Virgin", "CrossCountry"), each = 4),
    cat = rep(c("Express", "Ordinary"), 6),
    n = sample(50:500, 12)
  )
  pt <- pivottabler::PivotTable$new()
  pt$addData(pv)
  pt$addColumnDataGroups("cat")
  pt$addRowDataGroups("toc")
  pt$defineCalculation(calculationName = "Total", summariseExpression = "sum(n)")
  pt$evaluatePivot()
  pivot <- paste0("<style>", pt$getCss(), "</style>", as.character(pt$getHtml()))
}

cases <- list(
  kable      = knitr::kable(d, format = "html"),
  xtable     = paste(capture.output(print(xtable::xtable(d), type = "html")), collapse = "\n"),
  tinytable  = as.character(tinytable::save_tt(tinytable::tt(d), "html")),
  htmlTable  = htmlTable::htmlTable(as.matrix(d)),
  wikipedia  = wiki,
  legacy     = legacy,
  tricky     = tricky,
  stylesheet = styled,
  pivot      = pivot
)
cases <- cases[!vapply(cases, is.null, logical(1))]

wb <- wb_workbook()
for (nm in names(cases)) {
  wb$add_worksheet(nm, grid_lines = FALSE)
  ok <- tryCatch({ wb <- wb_add_html(wb, as.character(cases[[nm]]), sheet = nm, dims = "B2"); "ok" },
                 error = function(e) conditionMessage(e))
  cat(sprintf("%-11s %s\n", nm, ok))
}
wb$save("dialects.xlsx")
for (nm in names(cases)) {
  cat("=====", nm, "=====\n")
  print(tryCatch(wb_to_df(wb, sheet = nm, col_names = FALSE), error = function(e) "empty"))
}
