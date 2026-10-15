# gt writes its date and time styles as CLDR patterns, which map onto spreadsheet
# date formats almost one to one. A column is only written as dates when the
# printed text is exactly what such a pattern gives for the stored values, the
# same round trip that guards numbers.

date_styles <- c(
  "y-MM-dd", "EEEE, MMMM d, y", "EEE, MMM d, y", "EEEE d MMMM y", "MMMM d, y",
  "MMM d, y", "d MMM y", "d MMMM y", "d MMMM", "d MMM", "y", "MMMM", "dd",
  "y/MM/dd", "yy/MM/dd"
)
# styles that name a whole day; the rest cannot be told from other text
date_styles_full <- date_styles[c(1:8, 14L)]
time_styles <- c("HH:mm:ss", "HH:mm", "h:mm:ss a", "h:mm a", "h a")

en_months <- c("January", "February", "March", "April", "May", "June", "July",
               "August", "September", "October", "November", "December")
en_days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
             "Saturday")

cldr_tokens <- function(pattern) {
  regmatches(pattern, gregexpr("'[^']*'|[A-Za-z]+|[^A-Za-z']+", pattern))[[1L]]
}

cldr_field <- c(y = "year", yy = "year2", MM = "month2", MMM = "monabb",
                MMMM = "monname", d = "day", dd = "day2", EEE = "dayabb",
                EEEE = "dayname", HH = "hour2", h = "hour12", mm = "min2",
                ss = "sec2", a = "ampm")

# lt is a POSIXlt in UTC; NULL when the pattern holds a token not handled here
cldr_render <- function(pattern, lt) {
  hour12 <- ifelse(lt$hour %% 12L == 0L, 12L, lt$hour %% 12L)
  field <- list(
    year = as.character(lt$year + 1900L),
    year2 = sprintf("%02d", (lt$year + 1900L) %% 100L),
    month2 = sprintf("%02d", lt$mon + 1L),
    monabb = substr(en_months[lt$mon + 1L], 1L, 3L),
    monname = en_months[lt$mon + 1L],
    day = as.character(lt$mday),
    day2 = sprintf("%02d", lt$mday),
    dayabb = substr(en_days[lt$wday + 1L], 1L, 3L),
    dayname = en_days[lt$wday + 1L],
    hour2 = sprintf("%02d", lt$hour),
    hour12 = as.character(hour12),
    min2 = sprintf("%02d", lt$min),
    sec2 = sprintf("%02d", as.integer(lt$sec)),
    ampm = ifelse(lt$hour < 12L, "AM", "PM")
  )
  out <- rep("", length(lt$year))
  for (tok in cldr_tokens(pattern)) {
    piece <- if (grepl("^'", tok)) {
      gsub("'", "", tok, fixed = TRUE)
    } else if (!grepl("^[A-Za-z]", tok)) {
      tok
    } else {
      field[[cldr_field[tok]]]
    }
    if (is.null(piece)) return(NULL)
    out <- paste0(out, piece)
  }
  out
}

cldr_excel <- function(pattern) {
  code <- c(year = "yyyy", year2 = "yy", month2 = "mm", monabb = "mmm",
            monname = "mmmm", day = "d", day2 = "dd", dayabb = "ddd",
            dayname = "dddd", hour2 = "hh", hour12 = "h", min2 = "mm",
            sec2 = "ss", ampm = "AM/PM")
  paste(vapply(cldr_tokens(pattern), function(tok) {
    if (grepl("^'", tok)) {
      paste0("\"", gsub("'", "", tok, fixed = TRUE), "\"")
    } else if (!grepl("^[A-Za-z]", tok)) {
      tok
    } else {
      unname(code[cldr_field[tok]])
    }
  }, character(1L)), collapse = "")
}

date_norm <- function(x) gsub("[\u00a0\u202f]", " ", x, perl = TRUE)

excel_serial <- function(lt) {
  days <- as.numeric(as.POSIXct(strftime(lt, "%Y-%m-%d", tz = "UTC"), tz = "UTC")) / 86400
  days + 25569 + (lt$hour * 3600 + lt$min * 60 + lt$sec) / 86400
}

iso_pat <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}([ T][0-9]{2}:[0-9]{2}(:[0-9]{2})?)?$"

# the clock time as stored, as a POSIXlt in UTC, or NULL for anything that is
# not a date
as_clock <- function(values) {
  if (inherits(values, "Date")) {
    return(as.POSIXlt(as.POSIXct(values), tz = "UTC"))
  }
  if (inherits(values, "POSIXct")) {
    tz <- attr(values, "tzone")
    if (is.null(tz) || !nzchar(tz[1L])) tz <- "UTC"
    clock <- format(values, "%Y-%m-%d %H:%M:%S", tz = tz[1L])
    return(as.POSIXlt(clock, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"))
  }
  if (is.character(values)) {
    v <- values[!is.na(values)]
    if (!length(v) || !all(grepl(iso_pat, v))) return(NULL)
    v <- sub("T", " ", values, fixed = TRUE)
    v <- ifelse(nchar(v) == 10L, paste(v, "00:00:00"), v)
    v <- ifelse(nchar(v) == 16L, paste0(v, ":00"), v)
    return(as.POSIXlt(v, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"))
  }
  NULL
}

#' @return `NULL` unless every printed value is what one date or time style
#'   gives for the stored value, otherwise a list with `values` as spreadsheet
#'   serials and `numfmt`.
#' @noRd
infer_datefmt <- function(strings, values) {
  if (any(is_rich(strings))) return(NULL)
  lt <- as_clock(values)
  if (is.null(lt)) return(NULL)
  strings <- date_norm(html_strip(strings))
  keep <- !is.na(strings) & nzchar(strings) & !is.na(lt$year)
  if (!any(keep)) return(NULL)
  lt_keep <- lt[keep]

  candidates <- c(date_styles,
                  paste(rep(date_styles[1:8], each = length(time_styles)),
                        time_styles))
  for (pattern in candidates) {
    shown <- cldr_render(pattern, lt_keep)
    if (!is.null(shown) && identical(shown, strings[keep])) {
      out <- rep(NA_real_, length(strings))
      out[keep] <- excel_serial(lt_keep)
      return(list(values = out, numfmt = cldr_excel(pattern)))
    }
  }
  NULL
}

# For HTML there are no stored values, so a cell is read back by the shape of
# each whole-day style and accepted when that style reproduces the text.
text_as_dates <- function(x) {
  n <- length(x)
  out <- list(value = rep(NA_real_, n), numfmt = rep(NA_character_, n))
  left <- which(!is.na(x) & nzchar(x) & grepl("[0-9]", x))
  if (!length(left)) return(out)
  txt <- date_norm(trimws(x))

  candidates <- c(date_styles_full,
                  paste(rep(date_styles_full[1:8], each = length(time_styles)),
                        time_styles))
  for (pattern in candidates) {
    if (!length(left)) break
    read <- cldr_read(pattern, txt[left])
    if (is.null(read)) next
    hit <- !is.na(read$year)
    if (!any(hit)) next
    lt <- as.POSIXlt(sprintf("%04d-%02d-%02d %02d:%02d:%02d", read$year[hit],
                             read$month[hit], read$day[hit], read$hour[hit],
                             read$min[hit], read$sec[hit]),
                     tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    real <- !is.na(lt$year)
    ok <- logical(length(lt$year))
    ok[real] <- cldr_render(pattern, lt[real]) == txt[left][hit][real]
    idx <- left[hit][ok]
    out$value[idx] <- excel_serial(lt[ok])
    out$numfmt[idx] <- cldr_excel(pattern)
    left <- setdiff(left, idx)
  }
  out
}

read_rx <- list(
  year = "([0-9]{4})",
  month2 = "([0-9]{2})", day2 = "([0-9]{2})", hour2 = "([0-9]{2})",
  min2 = "([0-9]{2})", sec2 = "([0-9]{2})",
  monabb = paste0("(", paste(substr(en_months, 1L, 3L), collapse = "|"), ")"),
  monname = paste0("(", paste(en_months, collapse = "|"), ")"),
  day = "([0-9]{1,2})", hour12 = "([0-9]{1,2})",
  dayabb = paste0("(", paste(substr(en_days, 1L, 3L), collapse = "|"), ")"),
  dayname = paste0("(", paste(en_days, collapse = "|"), ")"),
  ampm = "(AM|PM)"
)

cldr_read <- function(pattern, txt) {
  pieces <- character()
  kinds <- character()
  for (tok in cldr_tokens(pattern)) {
    if (grepl("^'", tok)) {
      pieces <- c(pieces, gsub("([][{}()*+?.\\\\^$|])", "\\\\\\1",
                               gsub("'", "", tok, fixed = TRUE)))
    } else if (!grepl("^[A-Za-z]", tok)) {
      pieces <- c(pieces, gsub("([][{}()*+?.\\\\^$|])", "\\\\\\1", tok))
    } else {
      f <- cldr_field[[tok]]
      if (is.null(read_rx[[f]])) return(NULL)
      pieces <- c(pieces, read_rx[[f]])
      kinds <- c(kinds, f)
    }
  }
  m <- regmatches(txt, regexec(paste0("^", paste(pieces, collapse = ""), "$"), txt))
  hit <- lengths(m) > 0L
  n <- length(txt)
  field <- function(kind) {
    j <- match(kind, kinds)
    if (is.na(j)) return(NULL)
    v <- rep(NA_character_, n)
    v[hit] <- vapply(m[hit], function(z) z[[j + 1L]], character(1L))
    v
  }
  month <- if (!is.null(f1 <- field("month2"))) {
    as.integer(f1)
  } else if (!is.null(f2 <- field("monabb"))) {
    match(f2, substr(en_months, 1L, 3L))
  } else {
    match(field("monname"), en_months)
  }
  hour <- if (!is.null(f3 <- field("hour2"))) {
    as.integer(f3)
  } else if (!is.null(f4 <- field("hour12"))) {
    (as.integer(f4) %% 12L) + ifelse(field("ampm") == "PM", 12L, 0L)
  } else {
    rep(0L, n)
  }
  min <- field("min2")
  sec <- field("sec2")
  day <- if (!is.null(f5 <- field("day2"))) as.integer(f5) else as.integer(field("day"))
  list(year = as.integer(field("year")), month = month, day = day, hour = hour,
       min = if (is.null(min)) rep(0L, n) else as.integer(min),
       sec = if (is.null(sec)) rep(0L, n) else as.integer(sec))
}
