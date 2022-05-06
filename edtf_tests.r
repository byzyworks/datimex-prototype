source("edtf_resolve_v0.1.r")

options(warn=-1)

exps <- c(
  "@",
  "+@",
  "-@",
  "@ + 1D",
  "@ - 1D",
  "@ + d1",
  "@ + #1",
  "@ + #6d13",
  "@ + #6d13 + 1D + #6d13",
  "y2020m8d14H0M0S0 + #6d13",
  "@ - y2021m1d1H0M0S0",
  "1s",
  "2s + 2s",
  "2s2s",
  "y1970m1d1H0M0S0",
  "y1970m1d1H0M0S0z",
  "@ + 1Yu",
  "@ + 1Yv",
  "@ + y2025 + y2020",
  "@ + #1d1 + #2d2 + #3d3",
  "y2020",
  "y2020y2021m12d6H0M0S0",
  "2D - d2",
  "3Mu",
  "3Mv",
  "mary had a little lamb"
)

desc <- c(
  "now",
  "now",
  "now (in vector form)",
  "now plus 1 day",
  "now minus 1 day",
  "once it's day 1 of the month",
  "once it's Sunday",
  "once it's Friday the 13th",
  "once it's Friday the 13th again",
  "once it's Friday the 13th again",
  "difference between now and 01/01/2021",
  "1 second",
  "2 seconds plus 2 seconds",
  "2 seconds plus 2 seconds",
  "January 1st, 1970, 12:00:00 AM (localtime)",
  "January 1st, 1970, 12:00:00 AM (UTC)",
  "now plus 1 (forced) calendar year",
  "now plus 1 (forced) real year (60*60*24*365 seconds)",
  "once it's 2025, then backtrack until it's 2020",
  "once it's Sunday the 1st, then Monday the 2nd, then March the 3rd",
  "any time in 2020 (multiple points in time)",
  "a date in which it is both 2020 and 2021 (no such point in time exists)",
  "2 days minus the 2nd day of the month (what even)",
  "3 (forced) calendar months (not relative to anything, but needs to be to measure time period)",
  "3 (forced) real months (2629800*3 seconds)",
  "not a datimex"
)

sols <- rep(NA_character_, length(exps))
type <- rep(NA_character_, length(exps))
text <- rep(NA_character_, length(exps))
for (i in 1:length(exps)) {
  sols[[i]] <- edtf_resolve(exps[[i]])
  
  if (!is.na(sols[[i]])) {
    start <- substr(sols[[i]], 1, 1)
    
    if (start == "X") {
      type[[i]] <- "no solution"
      next
    } else if (start == "t") {
      type[[i]] <- "point"
    } else {
      type[[i]] <- "vector"
      next
    }
  } else {
    type[[i]] <- "invalid"
    next
  }
  
  text[[i]] <- as.character(as_datetime(as.numeric(substr(sols[[i]], 2, nchar(sols[[i]]))), tz = Sys.timezone()))
}

out <- data.frame(datimex = exps, output = sols, type = type, datetime = text, description = desc)
print(out, right = FALSE)