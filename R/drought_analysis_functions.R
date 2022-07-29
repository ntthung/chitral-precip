# Classify each year
classify_events <- function(x) {
  N <- length(x)
  events <- rep(0, N)
  if (x[1] < 0 && x[2] < 0) {
    events[1] <- events[2] <- -1
  } else if (x[1] > 0 && x[2] > 0) {
    events[1] <- events[2] <- 1
  }
  for (i in 2:(N-1)) {
    if (events[i-1] == -1) {             # In drought
      if (x[i] < 0) {                    # This year is dry: still in drought
        events[i] <- -1
      } else {                           # This year is wet
        if (x[i+1] < 0) {                # Next year back to dry: still in drought
          events[i] <- -1
        } else {                         # Next year also wet: turn to pluvial
          events[i] <- 1
        }
      }
    } else {                             # Not in drought
      if (events[i-1] == 1) {            # In pluvial
        if (x[i] > 0) {                  # This year is wet: still in pluvial
          events[i] <- 1
        } else {                         # This year is dry
          if (x[i+1] > 0) {              # Next year back to wet: still in pluvial
            events[i] <- 1
          } else {                       # Next year is also dry: turn to drought
            events[i] <- -1
          }
        }
      } else {                           # In neutral
        if (x[i] < 0 && x[i+1] < 0) {    # Start a drought
          events[i] <- -1
        } else {
          if (x[i] > 0 && x[i+1] > 0) {  # Start a pluvial
            events[i] <- 1
          }                              # Otherwise stay neutral
        }
      }
    }
  }
  if (events[N-1] == -1 && x[N] < 0) {
    events[N] <- -1
  } else {
    if (events[N-1] == 1 && x[N] > 0) {
      events[N] <- 1
    }
  }
  events
}

# Make a data.table of each events, consisting of timing, duration, and peak
get_timing <- function(x, events) {
  
  N <- length(events)
  
  # Get start and end of sequences
  events <- c(0, events)
  droughtStt <- which(sapply(2:N, \(k) events[k-1] != -1 && events[k] == -1))
  pluvialStt <- which(sapply(2:N, \(k) events[k-1] !=  1 && events[k] ==  1))
  events <- c(events[-1], 0)
  droughtEnd <- which(sapply(1:N, \(k) events[k] == -1 && events[k+1] != -1))
  pluvialEnd <- which(sapply(1:N, \(k) events[k] ==  1 && events[k+1] !=  1))
  
  rbind(
    data.table(
      type = 'drought',
      id = 1:length(droughtStt),
      start = as.integer(droughtStt),
      final = as.integer(droughtEnd),
      dur = droughtEnd - droughtStt + 1,
      peak = sapply(1:length(droughtStt), \(k)  min(x[droughtStt[k]:droughtEnd[k]]))
    ),
    data.table(
      type = 'pluvial',
      id = 1:length(pluvialStt),
      start = as.integer(pluvialStt),
      final = as.integer(pluvialEnd),
      dur   = pluvialEnd - pluvialStt + 1,
      peak = sapply(1:length(pluvialStt), \(k)  max(x[pluvialStt[k]:pluvialEnd[k]]))
    )
  )
}

