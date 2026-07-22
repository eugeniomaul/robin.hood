## Internal helpers for s_summarize.

## Stata-matching moment statistics (base R, no dependencies).
## Stata summarize, detail uses the BIASED skewness (m3 / m2^1.5) and
## NON-EXCESS kurtosis (m4 / m2^2, ~3 for a normal). Variance/SD use n-1.
.s_moments <- function(x){
  x <- x[!is.na(x)]
  n <- length(x)
  if(n == 0) return(list(n=0, mean=NA, sd=NA, var=NA, se=NA,
                         skewness=NA, kurtosis=NA, min=NA, max=NA))
  mu  <- mean(x)
  m2  <- mean((x - mu)^2)              # population 2nd moment (for skew/kurt)
  m3  <- mean((x - mu)^3)
  m4  <- mean((x - mu)^4)
  v   <- if(n > 1) sum((x - mu)^2)/(n - 1) else NA   # sample variance (Stata)
  sdv <- if(n > 1) sqrt(v) else NA
  list(
    n        = n,
    mean     = mu,
    sd       = sdv,
    var      = v,
    se       = if(n > 1) sdv/sqrt(n) else NA,
    skewness = if(m2 > 0) m3 / (m2^(3/2)) else NA,
    kurtosis = if(m2 > 0) m4 / (m2^2) else NA,
    min      = min(x),
    max      = max(x)
  )
}

## Compute a single named statistic for a numeric vector.
## 'stat' is one of the accepted keywords. Returns a numeric scalar.
.s_one_stat <- function(x, stat){
  xx <- x[!is.na(x)]
  n  <- length(xx)
  pctl <- function(p) if(n >= 1) as.numeric(quantile(xx, p/100, type = 6)) else NA
  ## type=6 matches Stata's default percentile definition most closely.
  m <- .s_moments(x)
  switch(stat,
    n         = m$n,
    mean      = m$mean,
    sd        = m$sd,
    variance  = m$var,
    se        = m$se,
    min       = m$min,
    max       = m$max,
    median    = pctl(50),
    skewness  = m$skewness,
    kurtosis  = m$kurtosis,
    shapiro.test = if(n > 2 && n < 5000)
                     tryCatch(shapiro.test(xx)$p.value, error = function(e) NA) else NA,
    p1 = pctl(1),  p5 = pctl(5),  p10 = pctl(10), p25 = pctl(25),
    p50 = pctl(50), p75 = pctl(75), p90 = pctl(90), p95 = pctl(95), p99 = pctl(99),
    NA
  )
}

## Expand the user's statistics vector, resolving the 'q' shortcut (p25,p50,p75).
.s_expand_stats <- function(statistics){
  out <- character(0)
  for(s in statistics){
    if(s == "q") out <- c(out, "p25", "p50", "p75") else out <- c(out, s)
  }
  ## de-duplicate but preserve order
  out[!duplicated(out)]
}

## Pretty column headers for each statistic keyword.
.s_stat_label <- function(stat){
  labs <- c(n="Obs", mean="Mean", sd="Std. Dev.", variance="Variance",
            se="Std. Err.", min="Min", max="Max", median="Median",
            skewness="Skewness", kurtosis="Kurtosis", shapiro.test="Shapiro p",
            p1="1%", p5="5%", p10="10%", p25="25%", p50="50%",
            p75="75%", p90="90%", p95="95%", p99="99%")
  if(stat %in% names(labs)) labs[[stat]] else stat
}

## Format one statistic's value, honoring per-stat decimal overrides.
## n is integer; shapiro.test gets >=4 dp; everything else uses cont.digits.
.s_fmt_stat <- function(value, stat, cont.digits){
  if(is.na(value)) return("")
  if(stat == "n") return(as.character(round(value)))
  d <- if(stat == "shapiro.test") max(4, cont.digits) else cont.digits
  format(round(value, d), nsmall = d, trim = TRUE)
}