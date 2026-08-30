library(pivottabler)
library(openxlsx2)
library(gtxlsx)

# pivottabler builds a complete HTML table plus a stylesheet, so wb_add_html()
# can take it as is. Each example below mirrors one from the package vignettes.
pt_html <- function(pt) {
  paste0("<style>", pt$getCss(), "</style>", as.character(pt$getHtml()))
}

add <- function(wb, name, pt, dims = "B2") {
  wb$add_worksheet(name, grid_lines = FALSE)
  wb_add_html(wb, pt_html(pt), sheet = name, dims = dims)
}

wb <- wb_workbook()

# 1 -- two levels of column groups and row groups, with totals ----------------
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addColumnDataGroups("PowerType")
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName = "TotalTrains", summariseExpression = "n()")
pt$evaluatePivot()
wb <- add(wb, "nested", pt)

# 2 -- several calculations side by side --------------------------------------
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName = "TotalTrains", caption = "Trains",
                     summariseExpression = "n()")
pt$defineCalculation(calculationName = "MaxSpeed", caption = "Max Speed",
                     summariseExpression = "max(SchedSpeedMPH, na.rm=TRUE)")
pt$defineCalculation(calculationName = "MeanSpeed", caption = "Mean Speed",
                     summariseExpression = "mean(SchedSpeedMPH, na.rm=TRUE)",
                     format = "%.1f")
pt$evaluatePivot()
wb <- add(wb, "calcs", pt)

# 3 -- outline layout: row groups become headings and subtotal rows -----------
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups(
  "TOC",
  outlineBefore = list(groupStyleDeclarations = list(color = "blue")),
  outlineAfter = list(isEmpty = FALSE, mergeSpace = "dataGroupsOnly",
                      caption = "Total ({value})",
                      groupStyleDeclarations = list("font-style" = "italic")),
  outlineTotal = list(groupStyleDeclarations = list(color = "blue"),
                      cellStyleDeclarations = list(color = "blue"))
)
pt$addRowDataGroups("PowerType", addTotal = FALSE)
pt$defineCalculation(calculationName = "TotalTrains", summariseExpression = "n()")
pt$evaluatePivot()
wb <- add(wb, "outline", pt)

# 4 -- a coloured theme --------------------------------------------------------
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName = "TotalTrains", summariseExpression = "n()")
pt$theme <- list(
  fontName = "Verdana, Arial",
  headerBackgroundColor = "rgb(0, 102, 204)",
  headerColor = "rgb(255, 255, 255)",
  cellBackgroundColor = "rgb(255, 255, 255)",
  cellColor = "rgb(0, 0, 0)",
  totalBackgroundColor = "rgb(240, 240, 240)",
  totalColor = "rgb(0, 0, 0)",
  borderColor = "rgb(64, 64, 64)"
)
pt$evaluatePivot()
wb <- add(wb, "theme", pt)

# 5 -- conditional formatting on found cells ----------------------------------
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName = "TotalTrains", summariseExpression = "n()")
pt$evaluatePivot()
high <- pt$findCells(minValue = 10000, totals = "exclude")
pt$setStyling(cells = high,
              declarations = list("background-color" = "#C6EFCE", color = "#006100"))
low <- pt$findCells(maxValue = 3000, totals = "exclude")
pt$setStyling(cells = low,
              declarations = list("background-color" = "#FFC7CE", color = "#9C0006"))
pt$setStyling(rowNumbers = 1, columnNumbers = 1:3,
              declarations = list("font-weight" = "bold"))
wb <- add(wb, "conditional", pt)

# 6 -- a deeper hierarchy on both axes ----------------------------------------
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addColumnDataGroups("PowerType", addTotal = FALSE)
pt$addRowDataGroups("TOC")
pt$addRowDataGroups("SchedSpeedMPH", addTotal = FALSE)
pt$defineCalculation(calculationName = "TotalTrains", summariseExpression = "n()")
pt$evaluatePivot()
wb <- add(wb, "hierarchy", pt)

wb$save("pivottabler.xlsx")
if (interactive()) wb$open()
