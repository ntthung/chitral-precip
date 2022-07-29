update_geom_defaults('line', list(size = 0.2))
update_geom_defaults('hline', list(size = 0.2))
update_geom_defaults('vline', list(size = 0.2))

# Labeling
skip_label <- function(n) {
  function(x) {
    idx <- seq_along(x)
    x[idx %% n != 1] <- ' '
    x 
  }
}

monthLab <- function(x) {
  sapply(x, function (m) {
    char <- substr(m, 4, 4)
    mon <- substr(m, 1, 1)
    fcase(char == 'p', paste0('<span style="color:firebrick">', mon, '</span>'),
          char == 'n', paste0('<span style="color:darkorange">', mon, '</span>'),
          default = mon)
  })
}

# Converting dplR objects to data.table

rwl_to_dt <- function(rwl, var.name = 'core', val.name = 'rw') {
  years <- as.integer(row.names(rwl))
  DT <- as.data.table(rwl)
  DT[, year := years]
  DT <- melt(DT, id.vars = 'year', variable.name = var.name, value.name = val.name)
  DT[!is.na(get(val.name))]
}

crn_to_dt <- function(crn) {
  DT <- as.data.table(crn, keep.rownames = 'year')
  DT[, year := as.integer(year)]
  DT[]
}

# Other utilities

lapplyrbind <- function(x, fun, ..., id = NULL) rbindlist(lapply(x, fun, ...), idcol = id)

abs_range <- function(x) {
  absMax <- max(abs(x))
  c(-absMax, absMax)
}

standardize <- function(x, ...) (x - mean(x, ...)) / sd(x, ...)

roundDT <- function(DT, digits = 2, type = 'numeric') {
  DT[, lapply(.SD, 
              function(x) {
                if (is.numeric(x)) {
                  rd <- round(x, digits)
                  if (type == 'numeric') rd else sprintf(glue("%.{digits}f"), rd)
                } else x
              })]
}

`%ni%` <- Negate(`%in%`)