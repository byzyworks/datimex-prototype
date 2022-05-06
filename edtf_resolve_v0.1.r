library("lubridate")
library("stringr")
library("dplyr")

###############################################################################################################################
#
#  EDTF (Expressional Date-Time Format)/DaTimEx Engine, v0.1 Alpha
# 
# The following implements a procedural language that is capable of performing a number of advanced operations on (traditional)
# datetime expressions, including arithmetic operations as if to treat datetime in a mathematical way, and a "datimex" is to
# "2+2" what a traditional datetime expression is to "4". However, as performing arithmetic is nothing new to the way a lot of
# APIs and libraries for various programming languages already implement objects for datetime, the purpose of the EDT format is
# to both expand on that functionality using a simplified, abstracted interface, and to provide it in such a way that it can be
# implemented "universally" into any application that wants to use datimexes to their advantage, which is very much akin to
# using "regular expressions" for parsing character strings, and as such is also where the namesake comes from. The purpose of
# datimex, or at least the original intended purpose, is or was to create a datetime format that could be relative to the
# current time, and thus, change with respect to time itself, in order to create repeating alarms without the awkward
# limitations of existing time-management applications and schedulers.
#
# One of these unique functionalities that datimexes have, for instance, is this ability to resolve "ambiguous" or "incomplete"
# datetime expressions based on other given (or implicit) information like a mode of operation (<future-forward, past-backward>
# search, etc.) and a "base" (like the present) from which to search from. A common example of this would be a datimex
# representing the question "when is the next Friday the 13th?". For that, the expression is:
#
#	"@ + #6d13"
#
# Where "@" is the current system time, "+" is the "relate" operator (with respect to the format which the arguments are in,
# which can be of two types: points (points in time) or vectors (lengths of time), to which "relate" is invoked between two
# points), and "#6d13" is a string not just representing Friday the 13th, but any Friday the 13th given nothing else as a
# condition (hence the ambiguity, and why an unambiguous base to relate to is needed). Here are some other examples you can try
# out:
#
#	"@"                 : now (point)
#	"@ + m12d25"        : next Christmas (point)
#	"y2020m12d25H0M0S0" : Christmas at midnight (point)
#	"@ + 2h"            : now plus 2 hours (point + vector = point)
#	"@ - 2h15m"         : now minus 2 hours and 15 minutes (point - vector = point)
#	"1d"                : 1 day (vector)
#	"-@"                : difference in seconds between now and January 1st, 1970 12:00:00 AM (vector)
#	"@ - t0"            : difference in seconds between now and January 1st, 1970 12:00:00 AM (point - point = vector)
# 
# (Note this is only as of v0.1 Alpha; there is greater deal of functionality planned, including things like logical operators
# and paranthetical notation.
#
# As of this, the following implementation is done in R, and using R Studio with a robust library for (traditional) datetime
# expressions called Lubridate, which has been integral to easing many of the headaches related to manipulating datetime (such
# as how a vector like "two hours" can mean two different things when dealing with daylight savings). The only function that is
# intended for end users is one at the end called "edtf_resolve()", which takes a datimex in the form of a string, and outputs
# a totally simplified datimex that is either in the form of the letter "t" proceeded by the number of seconds since the Unix
# epoch (January 1st, 1970, 12:00:00 AM) (that is, a point) or a number of seconds relative to no point in time at all (that
# is, a vector), which proceeds nothing but is proceeded by the letter "s". To put it simply:
#
#	edtf_resolve(<point expression>) -> <timestamp>
#	(example: edtf_resolve("y1970m1d1H0M0S0z + S5") -> "t5")
#	
#	OR...
#	
#	edtf_resolve(<vector expression>) -> <number of seconds>
#	(example: edtf_resolve("1m - 30s") -> "30s"
#
# You may get an idea of how this works by running a separate script called "edtf_test.r". This will test a number of
# expressions against the engine by default, and then display the results in a human-readable format. Try it, and see how the
# time at which the program is run changes the output of edtf_resolve() for all expressions that base off the "@" symbol for
# now.
#
# All other function, which are marked beginning with periods are at least informally considered to be internal functions.
#
###############################################################################################################################

###############################################################################################################################
# These are the types of outputs that may result from parsing or from an operation. These include points, which are points in
# time, or vectors, which are lengths of time. They mainly follow the rules of "points" and "vectors" in terms of affine
# spaces. For instance, a point plus a vector is valid, as is a vector plus a vector, but not a point plus a point, so "+"
# represents a different operation in that case. Note that a "contradiction" right now is only a catch-all for any expression
# that can't be solved. In the future, it may be useful to have a separate type for multi-solution returns, as distinguished
# from no-solution returns.
###############################################################################################################################
time_types <- list(
  contradiction = 0L,
  point         = 1L,
  vector        = 2L
)

###############################################################################################################################
# The elements of a point are all those elements which are required to "complete" the point, and as such, a complete point must
# have all of these defined. A point which is complete has only one possible solution, whether it is a particular point in time
# or no point in time (because of a contradiction). In order for edtf_resolve() to output a point in time, a complete
# expression has to be obtained at the very end, which can be built over the entire expression. Given the conflation between
# "completeness" and "resolution" as well, most point operations also require one or two complete points in order to work.
###############################################################################################################################
point_elem_types <- list(
  year   = "y",
  month  = "m",
  day    = "d",
  hour   = "H",
  minute = "M",
  second = "S"
)

###############################################################################################################################
# For everything else that still has to be true for the expression to spit out a point in time, but is not necessary to
# specify, there are the point's conditions. Another way to think of it is that the point's elements are its primary
# conditions, and these are its secondary conditions. So far, only one condition (specifying the day of the week) is
# implemented, but Lubridate makes it possible for some others to be implemented, like specifying the epidemiological week. In
# reality, the possibilities regarding conditions are endless as long as there are means of calculating and obtaining those
# conditions. You can even have a condition for Chinese New Year, as an example, using the same methodology these are solved
# for by.
###############################################################################################################################
point_cond_types <- list(
  wday = "#"
)

###############################################################################################################################
# Special point element types are ones which contain enough information for multiple primary conditions, or that do not need to
# be solved with regards to. The "null" element is a condensed form to represent a contradiction with. The "now" element is one
# which expands to the current local time of your system, which changes every time the expression is evaluated. It is arguably
# the most useful of all the elements to perform operations on. The "stmp" element (short for "timestamp") is for giving a Unix
# epoch timestamp, and is what a fully-resolved point expression will output. The "utc" element tells the engine to interpret
# the given primary conditions as being with respect to UTC time, rather than local time, as is the default.
###############################################################################################################################
point_spec_types <- list(
  null = "X",
  now  = "@",
  stmp = "t",
  utc  = "z"
)

###############################################################################################################################
# Vector elements do not represent conditions like their point equivalents, but simply offsets of different sizes. Because of
# phenomena like daylight savings time, "two hours", for instance, can mean two different things, and not all vectors are
# treated the same for that reason. For instance, if at 2:00 AM, the clock sets forward an hour, and the difference between 1
# AM and 3 AM is "calendrically" 2 hours, but in reality, only 1 hour in terms of actual time. If a vector contains units as
# large as days or larger, then by default, the vector is considered "unstable" - that is, the length of time changes depending
# on the preceding base/reference point (in time). Such vectors are fundamentally calendrical, and thus use Lubridate's
# "period" types. On the other hand, "stable" vectors are non-calendrical, use Lubridate's "duration" types, and can be
# translated to a number of seconds directly without a preceding base/reference point.
###############################################################################################################################
vector_elem_types <- list(
  years   = "Y",
  months  = "M",
  weeks   = "W",
  days    = "D",
  hours   = "h",
  minutes = "m",
  seconds = "s"
)

###############################################################################################################################
# Special vector elements function similarly to special point elements, allowing us to set certain state elements as well if we
# so desire. The "void" element is the vector equivalent to a contradiction or the point-specific "null" element. This usually
# appears when trying to resolve an explicitly unstable vector without a point of reference. The "unstable" element is what we
# can actually use to force the vector to be unstable, even if no unit larger than hours are used. On the contrary, the
# "stable" element does the opposite by forcing the vector be stable, even if units larger than hours are used.
###############################################################################################################################
vector_spec_types <- list(
  void     = "X",
  unstable = "u",
  stable   = "v"
)

###############################################################################################################################
# These are shortcuts for regular expressions that are commonly repeated or at least particularly useful. They're used to
# distinguish points from vectors, as well as to validate the received datimexes in general as actual datimexes.
###############################################################################################################################
point_char_regex  <- paste("[", paste(c(point_elem_types, point_cond_types, point_spec_types), collapse=""), "]", sep = "")
point_elem_regex  <- paste("[", paste(point_elem_types, collapse=""), "]", sep = "")
point_cond_regex  <- paste("[", paste(point_cond_types, collapse=""), "]", sep = "")
point_spec_regex  <- paste("[", paste(point_spec_types, collapse=""), "]", sep = "")
point_sep_regex   <- paste("((?<=\\d)|(?<=", point_char_regex, "))(?=", point_char_regex, ")", sep = "")
vector_char_regex <- paste("[", paste(c(vector_elem_types, vector_spec_types), collapse=""), "]", sep = "")
vector_elem_regex <- paste("[", paste(vector_elem_types, collapse=""), "]", sep = "")
vector_spec_regex <- paste("[", paste(vector_spec_types, collapse=""), "]", sep = "")
vector_sep_regex  <- paste("(?<=", vector_char_regex, ")((?=", vector_char_regex, ")|(?=\\d))", sep = "")
validation_regex  <- paste("^([\\+-]?(((\\d+", vector_elem_regex, ")|", vector_spec_regex, ")+|((", point_elem_regex, "\\d+)|(", point_cond_regex, "\\d+)|", point_spec_regex, ")+)([\\+-]|$))+", sep = "")

###############################################################################################################################
# Defines the contradiction "class" and returns a new instance of it (Yeah, I know that they're actually lists).
#
# @return A new datimex contradiction object (list)
###############################################################################################################################
.edtf_contradiction <- function() {
  new <- list(
    tag = time_types$contradiction
  )
  
  return(new)
}

###############################################################################################################################
# Defines the point "class" and returns a new instance of it. If a template/another point is given, create the new instance as
# a copy of that point. Notice the pseudo-object-oriented way things have been done. Here, the "classes" are defined inside of
# a function which also acts as the constructor for an object of said "class". I became aware of S3 and S4 classes perhaps a
# little too late, but this has served my purposes for now at least.
#
# @param  template Another datimex point object (list) which to copy
#
# @return A new point datimex point object (list)
###############################################################################################################################
.edtf_point <- function(template = NULL) {
  new <- list(
    tag   = time_types$point,
    elems = list(
      year   = NA,
      month  = NA,
      day    = NA,
      hour   = NA,
      minute = NA,
      second = NA
    ),
    force = list(
      year   = NA,
      month  = NA,
      day    = NA,
      hour   = NA,
      minute = NA,
      second = NA
    ),
    conds = list(
      wday = NA
    ),
    utc = FALSE
  )
  
  if (!is.null(template)) {
    for (i in names(point_elem_types)) {
      new$elems[[i]] <- template$elems[[i]]
      new$force[[i]] <- template$force[[i]]
    }
    for (i in names(point_cond_types)) {
      new$conds[[i]] <- template$conds[[i]]
    }
    new$utc <- template$utc
  }
  
  return(new)
}

###############################################################################################################################
# Defines the vector "class" and returns a new instance of it. If a template/another vector is given, create the new instance
# as a copy of that vector.
#
# @param  template Another datimex vector object (list) which to copy
#
# @return A new datimex vector object (list)
###############################################################################################################################
.edtf_vector <- function(template = NULL) {
  new <- list(
    tag   = time_types$vector,
    elems = list(
      years   = 0,
      months  = 0,
      weeks   = 0,
      days    = 0,
      hours   = 0,
      minutes = 0,
      seconds = 0
    ),
    explicitly_unstable = FALSE,
    implicitly_stable   = TRUE,
    explicitly_stable   = FALSE
  )
  
  if (!is.null(template)) {
    for (i in names(vector_elem_types)) {
      new$elems[[i]] <- template$elems[[i]]
    }
    new$stable  <- template$stable
  }
  
  return(new)
}

###############################################################################################################################
# Helper function to determine if a point is complete, and all of its primary conditions are filled in.
#
# @param point The datimex point object (list) being determined for its completeness
#
# @return A boolean/logical representing whether that datimex point object is complete or not
###############################################################################################################################
.edtf_point_is_complete <- function(point) {
  for (i in 1:length(point_elem_types)) {
    if (is.na(point$elems[[i]])) {
      return(FALSE)
    }
  }
  return(TRUE)
}

###############################################################################################################################
# Helper function to determine if a vector is stable or not, which is determined off of multiple stored conditions. The reason
# for the multiple conditions is so that the engine can change separately the "implicit" stability (to influence its actual
# stability) of the vector even after parsing depending on the circumstances. In that sense, storing the implicit and explicit
# states as one variable could potentially be losing information since it wouldn't know whether the stability was set by the
# machine (can be changed) or by the user (cannot be changed).
# 
# @param vector The datimex vector object (list) being determined for its stability
#
# @return A boolean/logical representing whether that datimex vector object is stable or not
###############################################################################################################################
.edtf_vector_is_stable <- function(vector) {
  if (vector$explicitly_unstable | !(vector$implicitly_stable | vector$explicitly_stable)) {
    return(FALSE)
  }
  return(TRUE)
}

###############################################################################################################################
# Helper function that fills in a point with the information contained in a Lubridate datetime object (summary). This is the
# inverse of .edtf_point_summary()
#
# @param point   The datimex point object (list) apply the datetime summary to
# @param summary A Lubridate datetime object that is to be applied to the datimex point object
#
# @return The argument datimex point object (list) with the summary argument applied to it 
###############################################################################################################################
.edtf_point_reflect <- function(point, summary) {
  for (i in names(point_elem_types)) {
    point$elems[[i]] <- as.numeric(do.call(i, as.list(summary)))
  }
  
  return(point)
}

###############################################################################################################################
# Helper function that checks for a point if there are any secondary conditions at all that would have to be solved for.
#
# @param point The datimex point object (list) being determined to have secondary conditions
#
# @return A boolean/logical representing whether there are secondary conditions or not
###############################################################################################################################
.edtf_point_cond_is_used <- function(point) {
  for (i in names(point_cond_types)) {
    if (!is.na(point$conds[[i]])) {
      return(TRUE)
    }
  }
  return(FALSE)
}

###############################################################################################################################
# Helper function that checks if a point's secondary conditions are in conflict with the point's primary conditions. The point
# is assumed to be complete before it is judged on this basis, but this factor may change at some point.
#
# @param point The datimex point object (list) being determined to have compatible primary and secondary conditions
#
# @return A boolean/logical representing whether the primary and secondary conditions are compatible or not
###############################################################################################################################
.edtf_point_cond_is_compat <- function(point) {
  if (.edtf_point_is_complete(point)) {
    summary <- .edtf_point_summary(point)
    for (i in names(point_cond_types)) {
      if (!is.na(point$conds[[i]])) {
        actual <- do.call(i, as.list(summary))
        cond   <- point$conds[[i]]
        
        if (actual != cond) {
          return(FALSE)
        }
      }
    }
  }
  
  return(TRUE)
}

###############################################################################################################################
# Helper function that checks if a point is not only complete, but that none of the primary conditions can either change
# (because they're set by the user).
#
# @param point The datimex point object (list) being determined as to whether it's static or not
#
# @return A boolean/logical representing whether the datimex point object is static or not
###############################################################################################################################
.edtf_point_is_static <- function(point) {
  for (i in 1:length(point_elem_types)) {
    if (is.na(point$force[[i]])) {
      return(FALSE)
    }
  }
  return(TRUE)
}

###############################################################################################################################
# Helper function to compute the smallest unit used by a point. This would be used in the future for allowing some point
# expressions to not need to be as specific or granular as using seconds all the time. At the moment, however, it is not used
# for anything at all, though I had the motive to make it while attempting (at far too late) to find a way of solving for leap
# days.
#
# @param point The datimex point object (list) that needs to be computed for its time resolution
#
# @return An integer representing the time resolution of the argument datimex point object
###############################################################################################################################
.edtf_point_resolution <- function(point) {
  for (i in 1:length(point_elem_types)) {
    if (!is.na(point$force[[i]])) {
      res <- i
    }
  }
  return(res)
}

###############################################################################################################################
# This function tries to determine if all of the primary and secondary conditions for a point are true at once, thus making
# that point a candidate of choice for .edtf_point_cond_solve(). In other words, this is the function that
# .edtf_point_cond_solve() tries to optimize for.
#
# @param point A datimex point object (list) that needs to be determined if all conditions contained within are met
#
# @return A boolean/logical representing whether all conditions of the argument are abided by for the time it is set to
###############################################################################################################################
.edtf_point_cond_is_true <- function(point) {
  if (!.edtf_point_is_complete(point)) {
    return(FALSE)
  }
  
  all <- TRUE
  
  for (i in 1:length(point_elem_types)) {
    if (!is.na(point$force[[i]])) {
      if (point$force[[i]] != point$elems[[i]]) {
        all <- all & FALSE
      }
    }
  }
  
  summary <- .edtf_point_summary(point)
  for (i in names(point_cond_types)) {
    if (!is.na(point$conds[[i]])) {
      actual <- do.call(i, as.list(summary))
      cond   <- point$conds[[i]]
      
      if (actual != cond) {
        all <- all & FALSE
      }
    }
  }
  
  return(all)
}

###############################################################################################################################
# The methodology for solving a point expression based on any present conditions is done here. More specifically, after solving
# for the primary conditions individually, like month and day, the conditions are solved separately here. This is normally done
# in a brute-force manner where just the original solution is continually incremented (or decremented) by some determined
# value, and tested until the function above outputs TRUE. This is done either until "iter", the number of iterations, is
# reached, or until surpassing the datetime object given as "limit". If the engine can't solve for the condition within those
# limits, it will simply output a contradiction.
#
# @param point   The datimex point object (list) which to solve for its secondary conditions 
# @param limit   A Lubridate datetime object at which to stop testing for solutions
# @param iter    An integer representing the number of times after which to stop testing for solutions
# @param forward A boolean/logical to represent whether the function should search the relative future (TRUE) or relative past
#
# @return The datimex point object (list) with a time that agrees with its primary and secondary conditions, or a contradiction
###############################################################################################################################
.edtf_point_cond_solve <- function(point, limit = NULL, iter = 500L, forward = TRUE) {
  if (!.edtf_point_is_complete(point)) {
    warning("Incomplete/multi-valued points are not solveable for their conditions.")
    return(.edtf_contradiction())
  }
  
  if (!.edtf_point_cond_is_used(point)) {
    return(point)
  }
  
  unit <- length(vector_elem_types)
  
  # Adjust the granularity of x based on if the weekday condition is active.
  # For any additional conditions, add more similar to the form this is given in.
  if (!is.na(point$conds$wday)) {
    cond_unit <- case_when(
      is.na(point$force$day)   ~ 3L,
      is.na(point$force$month) ~ 2L,
      is.na(point$force$year)  ~ 1L,
      TRUE                     ~ 0L
    )
    unit <- min(unit, cond_unit)
  }
  
  if (unit == 0L) {
    warning("There are no solutions for the year given.")
    return(.edtf_contradiction())
  }
  
  if (.edtf_point_is_static(point)) {
    if (.edtf_point_cond_is_true(point)) {
      return(point)
    } else {
      return(.edtf_contradiction())
    }
  }
  
  init_summary <- .edtf_point_summary(point)
  
  if ((unit >= 3) & (!is.na(point$conds$wday))) {
    diff <- point$conds$wday - wday(init_summary)
    if (forward) {
      if (diff < 0) {
        init_summary <- init_summary + weeks(1)
        init_summary <- floor_date(init_summary + days(diff), unit = "day")
      } else if (diff > 0) {
        init_summary <- floor_date(init_summary + days(diff), unit = "day")
      }
    } else {
      if (diff > 0) {
        init_summary <- init_summary - weeks(1)
        init_summary <- ceiling_date(init_summary + days(diff), unit = "day") - seconds(1)
      } else if (diff < 0) {
        init_summary <- ceiling_date(init_summary + days(diff), unit = "day") - seconds(1)
      }
    }
  }
  
  unit <- names(vector_elem_types)[[unit]]
  
  point <- .edtf_point_reflect(point, init_summary)
  if (.edtf_point_cond_is_true(point)) {
    return(point)
  }
  
  for (i in 1:iter) {
    if (forward) {
      summary <- init_summary + do.call(unit, as.list(i))
    } else {
      summary <- init_summary - do.call(unit, as.list(i))
    }
    
    if (!is.null(limit)) {
      if ((forward & (summary > limit)) | (!forward & (summary < limit))) {
        iter <- i
        break
      }
    }
    
    point <- .edtf_point_reflect(point, summary)
    if (.edtf_point_cond_is_true(point)) {
      return(point)
    }
  }
  
  warning("No suitable solutions could be found with the given conditions and number of iterations.")
  return(.edtf_contradiction())
}

###############################################################################################################################
# Helper function to determine the relative positioning of one point (p2) compared to another (p1). If p2 is in the past
# relative to p1, the function outputs -1. If p2 is in the future relative to p1, the function outputs 1. Otherwise, if p2 is
# in the present of p1 (its given primary condition match p1's), the function output 0. Unlike the point difference operation,
# this does not require that both points be complete, only p1.
#
# @param p1 The datimex point object (list) to subtract argument p2 from
# @param p2 The datimex point object (list) to be subtracted from argument p1
#
# @return A datimex vector object (list) representing the difference between the two arguments in seconds
###############################################################################################################################
.edtf_operate_point_compare <- function(p1, p2) {
  if (!.edtf_point_is_complete(p1)) {
    warning("At least point p1 must be complete/resolvable for another to be compared with it.")
    return(NULL)
  }
  
  if (is.na(p2$elems[[1]])) {
    return(1L)
  }
  
  for (i in 1:length(point_elem_types)) {
    if (!is.na(p2$elems[[i]])) {
      if (p2$elems[[i]] < p1$elems[[i]]) {
        return(-1L)
      } else if (p2$elems[[i]] > p1$elems[[i]]) {
        return(1L)
      }
    }
  }
  return(0L)
}

###############################################################################################################################
# The process of relation is a fundamental concept to the idea of datimexes. Relation is effectively used to turn an incomplete
# point, called the extension, into a complete one when measured against a point that's already complete, called the base. You
# can think of relation as solving a datimex for a certain point in time using both the point's elements and implicit
# conditions that (at least for now) are built into the operation. To explain it, let's take an example of an incomplete point
# "y2020". It is impossible to know what point in time this is referring to, assuming a time resolution smaller than years,
# because there is nothing to tell what month, day, etc. is desired in the output. It is a statement that represents all points
# of time in 2020, but which one should be output? Relation is telling the engine "Here's a particular point in time. Try to
# find the soonest point in time where these conditions are true, and if not in the future, try the most recent past." Those
# conditions are of course, simply the extension itself. Note that in the future, the relation operation could be modified to,
# for instance, try a past-first search, or to move towards the present, but the usefulness of that is limited without the
# planned OR operation.
#
# This function is the main driver for solving relation, which it does by trying different methods of solving. There are two
# already which this function uses. One is for solving the primary conditions that is done heuristically in this function, and
# one is for solving the secondary conditions that it calls .edtf_point_cond_solve() for. The heuristic way works by going down
# the list of primary conditions (from years to seconds) and checking to see when the extension's is less than the base's. If
# it is less, it rounds the previous unit in whichever way it's determined that it should (future = ceiling, past = floor - 1
# second, present = don't). Note that rounding is done either way if in any case, the extension is not already in the present
# completely. It then freely sets the rest of the primary conditions.
#
# This works in most cases, but is problematic in ones such as when attempting to find the next leap day from a year that is
# not the one directly before the leap year. That requires an additional method of solving that I have not gotten to
# implementing successfully here.
#
# @param base      A complete datimex point object (list) that the extension argument is solved relative to
# @param extension A datimex point object (list), either complete or incomplete (latter preferred), to be solved for
#
# @return A datimex point object (list) representing the solution to this operation
###############################################################################################################################
.edtf_operate_point_relate <- function(base, extension) {
  if (!.edtf_point_is_complete(base) & !.edtf_point_is_complete(extension)) {
    warning("An incomplete/multi-valued point cannot be related to another incomplete/multi-valued point.")
    return(.edtf_contradiction())
  }
  
  if (.edtf_point_is_complete(extension)) {
    return(extension)
  }
  
  position <- .edtf_operate_point_compare(base, extension)
  if (position == 0) {
    return(base)
  }
  
  rounded <- FALSE
  
  for (i in 1:length(point_elem_types)) {
    if (!is.na(extension$elems[[i]])) {
      if (!rounded) {
        summary <- .edtf_point_summary(base)
        if (is.na(summary)) {
          warning("The date represented by the expression is invalid.")
          return(.edtf_contradiction())
        }
        
        elem_diff <- extension$elems[[i]] - base$elems[[i]]
        
        if (position > 0) {
          if (elem_diff > 0) {
            if (i == 1) {
              base <- .edtf_point_reflect(base, ceiling_date(summary, unit = names(point_elem_types)[[i]]))
            } else {
              base <- .edtf_point_reflect(base, ceiling_date(summary, unit = names(point_elem_types)[[i]]))
            }
            rounded <- TRUE
          } else if (elem_diff < 0) {
            if (i == 1) {
              base <- .edtf_point_reflect(base, ceiling_date(summary, unit = names(point_elem_types)[[i - 1]]))
            } else {
              base <- .edtf_point_reflect(base, ceiling_date(summary, unit = names(point_elem_types)[[i - 1]]))
            }
            rounded <- TRUE
          }
        } else {
          if (elem_diff != 0) {
            base <- .edtf_point_reflect(base, floor_date(summary, unit = names(point_elem_types)[[i]]) - seconds(1))
            rounded <- TRUE
          }
        }
      }
      
      base$elems[[i]] <- extension$elems[[i]]
    }
  }
  
  for (i in 1:length(point_elem_types)) {
    base$force[[i]] <- extension$force[[i]]
  }
  for (i in 1:length(point_cond_types)) {
    base$conds[[i]] <- extension$conds[[i]]
  }
  
  if (.edtf_point_cond_is_used(base)) {
    if (position > 0) {
      base <- .edtf_point_cond_solve(base)
    } else {
      base <- .edtf_point_cond_solve(base, forward = FALSE)
    }
  }
  
  return(base)
}

###############################################################################################################################
# This operation tries to combine the elements of a vector into a point. This is relatively self-explanatory. For instance,
# with a point representing now, and a vector of 1 second, the result is just now + 1 second. The result of this operation is
# thus a point. The behavior varies with respect to stable and unstable vectors, but that is mostly handled by
# .edtf_vector_summary().
#
# @param point  A datimex point object (list) which to offset by the argument datimex vector object
# @param vector A datimex vector object (list) which to add to the argument datimex point object
#
# @return The argument datimex point object (list) after it is offset by the datimex vector object
###############################################################################################################################
.edtf_operate_point_offset <- function(point, vector) {
  temp <- .edtf_point_summary(point) + .edtf_vector_summary(vector, point = point)
  
  for (i in names(point_elem_types)) {
    point$elems[[i]] <- as.numeric(do.call(i, as.list(temp)))
  }
  
  return(point)
}

###############################################################################################################################
# In case a vector is ever subtracted or preceded with a minus sign, this function will simply be what negates its individual
# elements, and thus, the entire vector itself.
#
# @param vector A datimex vector object (list) to negate
#
# @return The argument datimex vector object (list) after it is negated
###############################################################################################################################
.edtf_vector_negate <- function(vector) {
  for (i in names(vector_elem_types)) {
    vector$elems[[i]] <- -vector$elems[[i]]
  }
  
  return(vector)
}

###############################################################################################################################
# This operation tries to add two vectors together, which it does simply by adding their elements together. The result of this
# operation is another vector. Remember that the stability of a vector affects the length of time that the same elements in
# each may represent. If the two vectors are forced to be of different types, they cannot be added together for this reason.
# Otherwise, the vector with the explicitly given, if at all given, stability element determines the stability of the final
# vector. Still otherwise, some more work needs to be done around how the final vector's stability is determined implicitly,
# since right now, it's just the first vector's stability that wins out.
#
# @param v1 A datimex vector object (list) to add to argument v2
# @param v2 A datimex vector object (list) to add to argument v1
#
# @return A datimex vector object (list) that is the sum of both argument datimex vector objects
###############################################################################################################################
.edtf_operate_vector_add <- function(v1, v2) {
  if ((v1$explicitly_stable & v2$explicitly_unstable) | (v1$explicitly_unstable & v2$explicitly_stable)) {
    warning("Stable and unstable vectors cannot be added together.")
    return(.edtf_contradiction())
  }
  
  for (i in names(v1$elems)) {
    v1$elems[[i]] <- v1$elems[[i]] + v2$elems[[i]]
  }
  
  if (v2$explicitly_stable) {
    v1$explicitly_stable <- TRUE
  } else if (v2$explicitly_unstable) {
    v1$explicitly_unstable <- TRUE
  }
  
  return(v1)
}

###############################################################################################################################
# This operation tries to subtract one point from another, resulting in the difference in seconds between the points in time
# they represent. The result of this operation is obviously a vector, which is technically stable. In the future, I would try
# to make it possible to create both stable and unstable vectors from this operation, but unstable vectors would almost
# certainly be computed in a different manner.
#
# @param p1 A datimex point object (list) to subtract the argument p2 from
# @param p2 A datimex point object (list) to subtract from the argument p1
#
# @return A datimex vector object (list) that represents the difference between the two argument datimex point objects
###############################################################################################################################
.edtf_operate_point_differ <- function(p1, p2) {
  if (!.edtf_point_is_complete(p1) | !.edtf_point_is_complete(p2)) {
    warning("Both points need to be complete/resolvable to have their differences taken.")
    return(.edtf_contradiction())
  }
  
  vector <- .edtf_vector()
  
  vector$explicitly_stable <- TRUE
  
  vector$elems$seconds <- as.numeric(.edtf_point_summary(p1)) - as.numeric(.edtf_point_summary(p2))
  
  if (is.na(vector$elems$seconds)) {
    return(.edtf_contradiction())
  }
  
  return(vector)
}

###############################################################################################################################
# The role of this function is to try to translate a point in the form that it is used by the engine, including its individual
# elements, into a Lubridate datetime object. This is useful because we need to use a datetime object in order to perform date
# arithmetic; doing the arithmetic on the individual primary conditions (correctly) would be as difficult as re-implementing
# Lubridate itself. This is the inverse of .edtf_point_reflect().
#
# @param point The datimex point object (list) to be summarized as a particular datetime
#
# @return A Lubridate datetime object that represents a particular the argument datimex point object
###############################################################################################################################
.edtf_point_summary <- function(point) {
  summary <- ymd_hms(paste(point$elems, collapse = " "), tz = Sys.timezone())
  if (point$utc) {
    summary <- force_tz(summary, tzone = "UTC")
  }
  
  return(summary)
}

###############################################################################################################################
# To attempt to determine the number of seconds that a vector and all of its elements represent, you would use this function.
# If the vector is stable, it's as simple as adding the vector elements together, translated to their appropriate number of
# seconds. If the vector is unstable, then a Lubridate datetime object has to be created from a reference point where the
# elements are added one by one, and a final difference in seconds is taken (that is an actual representation of time).
#
# @param vector The datimex vector object (list) to be summarized as a number of (real) seconds
# @param point  A datimex point object (list) the is used as a reference point for the argument vector if it is unstable
#
# @return A Lubridate duration object representing the number of seconds that the argument datimex vector object represents
###############################################################################################################################
.edtf_vector_summary <- function(vector, point = NULL) {
  if (.edtf_vector_is_stable(vector)) {
    summary <- dseconds(0)
    
    for (i in names(vector_elem_types)) {
      summary <- summary + do.call(paste("d", i, sep = ""), as.list(vector$elems[[i]]))
    }
  } else {
    if (!is.null(point)) {
      summary <- dseconds(0)
      
      nowtime <- .edtf_point_summary(point)
      if (is.na(nowtime)) {
        return(.edtf_contradiction())
      }
      
      for (i in names(vector_elem_types)) {
        nexttime <- nowtime + do.call(i, as.list(vector$elems[[i]]))
        summary  <- summary + dseconds(as.numeric(nexttime) - as.numeric(nowtime))
        nowtime  <- nexttime
      }
    }
  }
  
  return(summary)
}

###############################################################################################################################
# This function tries to determine the operation to use on two datimex-generated objects based on whether they're points or
# vectors, and what symbol was used between them. At the moment, the only symbols are "+", and "-". In the future, I have plans
# to add future symbols representing additional operations such as skip/backtrack (for points) or multiply (for vectors) ("*"),
# OR (","), ranged OR (":"), the logical so-called "hard operators" ("&, |, ?"), and possibly more.
#
# @param a  A datimex object (list) that is the first argument in the operation
# @param b  A datimex object (list) that is the second argument in the operation
# @param op A character as a symbol from the datimex that represents the operation to be performed
#
# @return A datimex object (list) that is the final result of the operation performed starting here
###############################################################################################################################
.edtf_operate <- function(a, b, op) {
  if ((a$tag == time_types$contradiction) | (b$tag == time_types$contradiction)) {
    return(.edtf_contradiction())
  }
  
  if (op == "+") {
    if (a$tag == time_types$point) {
      if (b$tag == time_types$point) {
        a <- .edtf_operate_point_relate(a, b)
      } else {
        a <- .edtf_operate_point_offset(a, b)
      }
    } else {
      if (b$tag == time_types$point) {
        a <- .edtf_operate_point_offset(b, a)
      } else {
        a <- .edtf_operate_vector_add(a, b)
      }
    }
  } else if (op == "-") {
    if (a$tag == time_types$point) {
      if (b$tag == time_types$point) {
        a <- .edtf_operate_point_differ(a, b)
      } else {
        b <- .edtf_vector_negate(b)
        a <- .edtf_operate_point_offset(a, b)
      }
    } else {
      if (b$tag == time_types$point) {
        warning("A point cannot be negated or subtracted from a vector.")
        return(.edtf_contradiction())
      } else {
        b <- .edtf_vector_negate(b)
        a <- .edtf_operate_vector_add(a, b)
      }
    }
  }
  
  return(a)
}

###############################################################################################################################
# This is the main function that fills a point object once it has determined if a datimex (full expression or sub-expression)
# represents a point. Mainly, it just looks through the individual elements inside the datimex and creates a point object based
# on what it finds. It thus also determines the value of individual elements for the numbers which are placed next to them in
# the datimex, which in a point follows the element's identifying character.
#
# @param exp A string/character-formatted datimex expression representing a datimex point 
#
# @return A datimex point object (list) representing the datimex point expression provided as an argument
###############################################################################################################################
.edtf_parse_point <- function(exp) {
  point <- .edtf_point()
  
  subexp_elems <- unlist(strsplit(exp, point_sep_regex, perl = TRUE))
  
  for (i in 1:length(subexp_elems)) {
    key <- substr(subexp_elems[[i]], 1, 1)
    val <- substr(subexp_elems[[i]], 2, nchar(subexp_elems[[i]]))
    
    if (key == point_spec_types$null) {
      return(.edtf_contradiction())
    } else if (key == point_spec_types$utc) {
      point$utc <- TRUE
    } else if (key == point_spec_types$stmp) {
      for (j in 1:length(point_elem_types)) {
        if (!is.na(point$elems[[j]])) {
          warning("One or more point elements are contradictory; no solution is possible.")
          return(.edtf_contradiction())
        }
      }
      
      temp <- as_datetime(as.numeric(val))
      for (i in 1:length(point_elem_types)) {
        point$elems[[i]] <- as.numeric(do.call(names(point_elem_types)[[i]], as.list(temp)))
        point$force[[i]] <- point$elems[[i]]
      }
    } else if (key == point_spec_types$now) {
      for (j in 1:length(point_elem_types)) {
        if (!is.na(point$elems[[j]])) {
          warning("One or more point elements are contradictory; no solution is possible.")
          return(.edtf_contradiction())
        }
      }
      
      temp <- now()
      for (i in 1:length(point_elem_types)) {
        point$elems[[i]] <- as.numeric(do.call(names(point_elem_types)[[i]], as.list(temp)))
        point$force[[i]] <- point$elems[[i]]
      }
    } else if (key == point_cond_types$wday) {
      point$conds$wday <- as.integer(val)
    } else {
      idx <- NULL
      idx <- case_when(
        key == point_elem_types$year   ~ 1L,
        key == point_elem_types$month  ~ 2L,
        key == point_elem_types$day    ~ 3L,
        key == point_elem_types$hour   ~ 4L,
        key == point_elem_types$minute ~ 5L,
        key == point_elem_types$second ~ 6L
      )
      
      if (!is.null(idx)) {
        if (!is.na(point$elems[[idx]])) {
          warning("One or more point elements are contradictory; no solution is possible.")
          return(.edtf_contradiction())
        }
        
        point$elems[[idx]] <- as.numeric(val)
        point$force[[idx]] <- point$elems[[idx]]
      }
    }
  }
  
  if (!.edtf_point_cond_is_compat(point)) {
    warning("One or more point elements are contradictory; no solution is possible.")
    return(.edtf_contradiction())
  }
  
  return(point)
}

###############################################################################################################################
# This is the main function that fills a vector object once it has determined if a datimex (full expression or sub-expression)
# represents a vector. Mainly, it just looks through the individual elements inside the datimex and creates a vector object
# based on what it finds. It thus also determines the value of individual elements for the numbers which are placed next to
# them in the datimex, which in a vector precedes the element's identifying character.
#
# @param exp A string/character-formatted datimex expression representing a datimex vector 
#
# @return A datimex vector object (list) representing the datimex vector expression provided as an argument
###############################################################################################################################
.edtf_parse_vector <- function(exp) {
  vector <- .edtf_vector()
  
  subexp_elems <- unlist(strsplit(exp, vector_sep_regex, perl = TRUE))
  
  for (j in 1:length(subexp_elems)) {
    key <- substr(subexp_elems[[j]], nchar(subexp_elems[[j]]), nchar(subexp_elems[[j]]))
    val <- substr(subexp_elems[[j]], 1, nchar(subexp_elems[[j]]) - 1)
    
    if (key == vector_spec_types$void) {
      return(.edtf_contradiction())
    } else if (key == vector_spec_types$stable) {
      vector$explicitly_stable <- TRUE
      if (vector$explicitly_unstable) {
        warning("A vector was declared stable and unstable simultaneously.")
        return(.edtf_contradiction())
      }
    } else if (key == vector_spec_types$unstable) {
      vector$explicitly_unstable <- TRUE
      if (vector$explicitly_stable) {
        warning("A vector was declared stable and unstable simultaneously.")
        return(.edtf_contradiction())
      }
    } else {
      idx <- NULL
      idx <- case_when(
        key == vector_elem_types$years   ~ "years",
        key == vector_elem_types$months  ~ "months",
        key == vector_elem_types$weeks   ~ "weeks",
        key == vector_elem_types$days    ~ "days",
        key == vector_elem_types$hours   ~ "hours",
        key == vector_elem_types$minutes ~ "minutes",
        key == vector_elem_types$seconds ~ "seconds"
      )
      
      if (!is.null(idx)) {
        vector$elems[[idx]] <- vector$elems[[idx]] + as.numeric(val)
      }
      
      if (str_detect(key, "[YMWD]")) {
        vector$implicitly_stable <- FALSE
      }
    }
  }
  
  return(vector)
}

###############################################################################################################################
# This is a recursive function that outputs an object representing the datimex given to it. Mainly right now, it just goes
# through the expression's sub-expressions (separated by operators) in a sequential, left-to-right manner. In the base case,
# it will try to determine if that expression or sub-expression represents a point or vector, and depending on which, it will
# hand that off to its respective parsing function. Otherwise, it asks .edtf_operate() to resolve two expressions into one
# (left-to-right) until it can get down to just one. The recursiveness is partly to prepare for adding paranthetical notation
# in the future, so I avoided using a loop for that reason.
#
# @param exp Any string/character-formatted datimex expression
#
# @return A datimex object (list) representing the (fully-reduced) datimex expression provided as an argument
###############################################################################################################################
.edtf_parse <- function(exp) {
  subexps <- unlist(strsplit(exp, "[\\+-]"))
  
  if (length(subexps) == 1) {
    if (str_detect(substr(subexps[[1]], 1, 1), point_char_regex)) {
      base <- .edtf_parse_point(subexps[[1]])
    } else {
      base <- .edtf_parse_vector(subexps[[1]])
    }
  } else {
    ops <- str_replace_all(exp, "[^\\+-]+", "")
    
    for (i in 2:length(subexps)) {
      if (i == 2) {
        base <- .edtf_parse(subexps[[i]])
        
        if (substr(ops, 1, 1) == "-") {
          if (base$tag == time_types$point) {
            base <- .edtf_operate(base, .edtf_parse("t0"), op = "-")
          } else if (base$tag == time_types$vector) {
            base <- .edtf_operate(.edtf_parse("0s"), base, op = "-")
          }
        }
      } else {
        modifier <- .edtf_parse(subexps[[i]])
        base <- .edtf_operate(base, modifier, op = substr(ops, i - 1, i - 1))
      }
    }
    
    return(base)
  }
}

###############################################################################################################################
# This is the first function that a datimex is put through before it is actually parsed as one. It is responsible for clearing
# out whitespaces, redundant operators, and ultimately, by determining if the datimex is actually formatted as a datimex at
# all. This allows the parsing functions that come later to assume what they're receiving is always a datimex.
#
# @param exp Any string/character-formatted datimex expression
#
# @return A datimex object that is correctly formatted for parsing, or nothing if the argument is not a datimex expression
###############################################################################################################################
.edtf_screen <- function(exp) {
  if (!is.character(exp)) {
    warning("The argument provided is not string-formatted.")
    return(.edtf_contradiction())
  }
  
  exp <- str_replace_all(exp, "\\s", "")
  
  exp <- str_replace_all(exp, "--", "+")
  exp <- str_replace_all(exp, "\\++", "+")
  exp <- str_replace_all(exp, "-\\+|\\+-", "-")
  exp <- str_replace_all(exp, "[\\+-]$", "")
  
  if (!str_detect(substr(exp, 1, 1), "^[\\+-]")) {
    exp <- paste("+", exp, sep = "")
  }
  
  if (nchar(str_replace_all(exp, validation_regex, "")) != 0L) {
    warning("The argument provided as a string is not a valid expression.")
    return(NA_character_)
  }
  
  return(exp)
}

###############################################################################################################################
# This function is run at the very end to transform the final point or vector into a massively simplified datimex that can be
# thought of as a timestamp or a similarly simple vector representation. This can even be fed back into the algorithm, if need
# be, through edtf_resolve(). Since it is basically a de-parsing function, in the future, the output might be diversified to
# try and account for incomplete datimexes (where it might return a still more simplified version of said datimex, if it can't
# solve the datimex). There may also be possible arguments the user could give to control the output, such as a "no vector"
# option or just an option to force a particular datetime format that isn't another datimex.
#
# @param timeval Any datimex object (list) to be de-parsed
#
# @return A string/character-formatted datimex expression representing the argument datimex object
###############################################################################################################################
.edtf_format <- function(timeval) {
  if (timeval$tag == time_types$contradiction) {
    return(point_spec_types$null)
  }
  if (timeval$tag == time_types$point) {
    summary <- .edtf_point_summary(timeval)
    if (is.na(summary)) {
      warning("The date represented by the expression is invalid.")
      return(point_spec_types$null)
    }
    return(paste(point_spec_types$stmp, as.numeric(summary), sep = ""))
  }
  if (timeval$tag == time_types$vector) {
    timeval$implicitly_stable <- TRUE
    if (timeval$explicitly_unstable) {
      return(vector_spec_types$void)
    }
    return(paste(as.numeric(.edtf_vector_summary(timeval)), vector_elem_types$seconds, sep = ""))
  }
}

###############################################################################################################################
# The granddaddy of them all, the equivalent to main(), and the only one that users should interface with (no dot at the
# beginning). I mentioned above that I may want to allow the user to give optional arguments to this function in the future,
# mainly to control the output. However, this function is indeed pretty simplistic, otherwise. Just for right now, and while
# there aren't any optional argument, it can only take a datimex-formatted string, and in the end, output a datimex-formatted
# string. Note that when I say "in the future", I'm generally referring to when after this algorithm is adapted to a separate,
# compiled program.
#
# @param exp Any string/character-formatted datimex expression
#
# @return A string/character-formatted datimex expression that can be treated as a Unix timestamp or a difference in seconds
###############################################################################################################################
edtf_resolve <- function(exp) {
  exp <- .edtf_screen(exp)
  if (is.na(exp)) {
    return(NA_character_)
  }
  timeval <- .edtf_parse(exp)
  return(.edtf_format(timeval))
}
