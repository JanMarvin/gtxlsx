css_hex <- function(x) {
  x <- trimws(tolower(x))
  if (!nzchar(x) || x %in% c("transparent", "none", "inherit", "initial", "currentcolor")) {
    return(NULL)
  }
  if (grepl("^#[0-9a-f]{8}$", x)) {
    return(toupper(paste0(substr(x, 8, 9), substr(x, 2, 7))))
  }
  if (grepl("^#[0-9a-f]{6}$", x)) {
    return(toupper(paste0("FF", substring(x, 2))))
  }
  if (grepl("^#[0-9a-f]{3,4}$", x)) {
    ch <- strsplit(substring(x, 2), "")[[1]]
    ch <- paste0(ch, ch)
    if (length(ch) == 4L) return(toupper(paste0(ch[4], ch[1], ch[2], ch[3])))
    return(toupper(paste0("FF", paste0(ch, collapse = ""))))
  }
  m <- regmatches(x, regexec("^rgba?\\(([^)]*)\\)$", x))[[1]]
  if (length(m) == 2L) {
    v <- strsplit(m[2L], "[,/ ]+")[[1]]
    v <- v[nzchar(v)]
    pct <- grepl("%$", v)
    num <- suppressWarnings(as.numeric(sub("%$", "", v)))
    num[pct] <- num[pct] * 255 / 100
    num <- num[!is.na(num)]
    if (length(num) >= 3L) {
      a <- if (length(num) >= 4L) num[4L] else 1
      if (a <= 1) a <- a * 255
      return(sprintf("%02X%02X%02X%02X", as.integer(round(a)),
                     as.integer(round(num[1L])), as.integer(round(num[2L])),
                     as.integer(round(num[3L]))))
    }
    return(NULL)
  }
  m <- regmatches(x, regexec("^hsla?\\(([^)]*)\\)$", x))[[1]]
  if (length(m) == 2L) {
    v <- strsplit(m[2L], "[,/ ]+")[[1]]
    v <- suppressWarnings(as.numeric(sub("%$", "", v[nzchar(v)])))
    v <- v[!is.na(v)]
    if (length(v) >= 3L) {
      rgb <- grDevices::hsv(((v[1L] %% 360) / 360), 0, 0)
      out <- hsl_rgb(v[1L], v[2L] / 100, v[3L] / 100)
      a <- if (length(v) >= 4L) v[4L] else 1
      if (a <= 1) a <- a * 255
      return(sprintf("%02X%02X%02X%02X", as.integer(round(a)), out[1L], out[2L], out[3L]))
    }
    return(NULL)
  }
  # R's colour names are the X11 set; several basic CSS names disagree
  css_named <- c(green = "008000", purple = "800080", gray = "808080",
                 grey = "808080", maroon = "800000", olive = "808000",
                 teal = "008080", silver = "C0C0C0", lime = "00FF00",
                 aqua = "00FFFF", fuchsia = "FF00FF", navy = "000080",
                 rebeccapurple = "663399")
  if (x %in% names(css_named)) return(paste0("FF", css_named[[x]]))
  rgb <- try(grDevices::col2rgb(x, alpha = TRUE), silent = TRUE)
  if (inherits(rgb, "try-error")) return(NULL)
  sprintf("%02X%02X%02X%02X", rgb[4L], rgb[1L], rgb[2L], rgb[3L])
}

css_color <- function(x, default = NULL) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return(default)
  css_hex(as.character(x)) %||% default
}

wbc <- function(hex) {
  if (is.null(hex) || is.na(hex)) return(NULL)
  openxlsx2::wb_color(hex = hex)
}

css_px <- function(x, base = 16) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return(NA_real_)
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(tolower(as.character(x)))
  num <- suppressWarnings(as.numeric(sub("^(-?[0-9.]+).*$", "\\1", x)))
  if (is.na(num)) {
    kw <- c(`xx-small` = 0.6, `x-small` = 0.75, small = 0.89, medium = 1,
            large = 1.2, `x-large` = 1.5, `xx-large` = 2, thin = 1 / base,
            thick = 5 / base)
    if (!x %in% names(kw)) return(NA_real_)
    return(unname(kw[x]) * base)
  }
  if (grepl("%$", x)) return(base * num / 100)
  if (grepl("px$", x)) return(num)
  if (grepl("pt$", x)) return(num / 0.75)
  if (grepl("(r?em)$", x)) return(base * num)
  if (grepl("in$", x)) return(num * 96)
  if (grepl("cm$", x)) return(num * 37.7953)
  if (grepl("mm$", x)) return(num * 3.77953)
  num
}

css_pt <- function(x, base = 16, default = NULL) {
  px <- css_px(x, base = base)
  if (is.na(px)) return(default)
  round(px * 0.75, 1)
}

css_border <- function(style, width = NULL, base = 16) {
  if (is.null(style) || length(style) != 1L || is.na(style)) return(NULL)
  style <- trimws(tolower(as.character(style)))
  if (style %in% c("none", "hidden", "")) return("none")
  w <- css_px(width, base = base)
  if (is.na(w)) w <- 1
  switch(
    style,
    solid = if (w >= 3) "thick" else if (w >= 2) "medium" else if (w < 1) "hair" else "thin",
    double = "double",
    dashed = if (w >= 2) "mediumDashed" else "dashed",
    dotted = "dotted",
    groove = ,
    ridge = ,
    inset = ,
    outset = "thin",
    "thin"
  )
}

css_align <- function(x, default = NULL) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return(default)
  switch(tolower(x), left = "left", right = "right", center = "center",
         justify = "justify", default)
}

css_valign <- function(x, default = NULL) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return(default)
  switch(tolower(x), top = "top", middle = "center", bottom = "bottom", default)
}

# CSS px -> Excel column width (characters), calibrated on the default font
px_to_width <- function(px) {
  if (is.na(px)) return(NA_real_)
  round(max(px - 5, 0) / 7, 2)
}

hsl_rgb <- function(h, s, l) {
  h <- (h %% 360) / 60
  c0 <- (1 - abs(2 * l - 1)) * s
  x0 <- c0 * (1 - abs(h %% 2 - 1))
  rgb <- switch(as.character(floor(h)),
                "0" = c(c0, x0, 0), "1" = c(x0, c0, 0), "2" = c(0, c0, x0),
                "3" = c(0, x0, c0), "4" = c(x0, 0, c0), c(c0, 0, x0))
  as.integer(round((rgb + (l - c0 / 2)) * 255))
}

apply_transform <- function(x, how) {
  if (is.null(how) || is.na(x)) return(x)
  switch(tolower(trimws(how)),
         uppercase = toupper(x),
         lowercase = tolower(x),
         capitalize = gsub("\\b([a-z])", "\\U\\1", x, perl = TRUE),
         x)
}

# CSS rotation is counter-clockwise; Excel takes 1-90 counter-clockwise and
# 91-180 as clockwise degrees minus 90
css_rotation <- function(x) {
  if (is.null(x) || is.na(x)) return(NULL)
  m <- regmatches(x, regexec("rotate\\(\\s*(-?[0-9.]+)deg", x))[[1]]
  if (length(m) != 2L) return(NULL)
  rot <- (-as.numeric(m[2L])) %% 360
  if (rot <= 90) as.integer(round(rot))
  else if (rot >= 270) as.integer(round(90 + (360 - rot)))
  else NULL
}

parse_css_decls <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(list())
  parts <- strsplit(x, ";", fixed = TRUE)[[1]]
  parts <- parts[grepl(":", parts, fixed = TRUE)]
  if (!length(parts)) return(list())
  key <- trimws(tolower(sub(":.*$", "", parts)))
  val <- trimws(sub("^[^:]*:", "", parts))
  imp <- grepl("!\\s*important", val, perl = TRUE)
  val <- trimws(sub("!\\s*important", "", val, perl = TRUE))
  out <- as.list(val)
  names(out) <- key
  attr(out, "important") <- key[imp]
  out
}
