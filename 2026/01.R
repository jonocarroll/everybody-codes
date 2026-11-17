example <- c(3, 11, 12, 16, 24, 25)
n_needed <- function(x) {
  closest <- ceiling(sqrt(x))
  closest^2 - x
}
sum(n_needed(example))

# d <- as.integer(strsplit(readLines("2026/data/01_01.csv"), ",")[[1]])
# ans <- sum(n_needed(d))

library(reticulate)
py_require("everybody-codes-data")
ecd <- import("ecd")

inputs <- ecd$get_inputs(quest = 1L, event = 2026L)
vals <- as.integer(strsplit(inputs[[1]], ",")[[1]])
ans <- sum(n_needed(vals))
ans

resp <- ecd$submit(quest = 1L, event = 2026L, part = 1L, answer = ans)
resp$status
resp$json()

inputs2 <- ecd$get_inputs(quest = 1L, event = 2026L)
vals <- as.integer(strsplit(inputs2[[2]], ",")[[1]])

n_needed_sq <- function(x) {
  guess <- 2
  while (TRUE) {
    guess <- guess + 1
    tot <- guess^2 - (guess - 2)^2
    if (tot >= x) break
  }
  tot - x
}
sapply(example, n_needed_sq)
sum(sapply(example, n_needed_sq))
ans <- sum(sapply(vals, n_needed_sq))
ans

resp <- ecd$submit(quest = 1L, event = 2026L, part = 2L, answer = ans)
resp$status

inputs3 <- ecd$get_inputs(quest = 1L, event = 2026L)
vals <- as.integer(strsplit(inputs3[[3]], ",")[[1]])

n_needed_hole <- function(x) {
  guess <- 1
  while (TRUE) {
    guess <- guess + 2
    tot <- guess^2 - 1
    if (tot >= x) break
  }
  tot - x
}
sapply(example, n_needed_hole)
sum(sapply(example, n_needed_hole))
ans <- sum(sapply(vals, n_needed_hole))
ans

resp <- ecd$submit(quest = 1L, event = 2026L, part = 3L, answer = ans)
resp$status

sq <- function(x) x^2
border <- function(x) x^2 - (x - 2)^2
hole <- function(x) x^2 - 1

n_needed <- function(x, f, a = 0, b = 1) {
  guess <- a
  tot <- 0
  while (tot < x) {
    guess <- guess + b
    tot <- f(guess)
  }
  tot - x
}

sum(sapply(example, n_needed, sq)) == 11
sum(sapply(example, n_needed, border, a = 2)) == 9
sum(sapply(example, n_needed, hole, a = 1, b = 2)) == 61

vals1 <- as.integer(strsplit(inputs3[[1]], ",")[[1]])
vals2 <- as.integer(strsplit(inputs3[[2]], ",")[[1]])
vals3 <- as.integer(strsplit(inputs3[[3]], ",")[[1]])

sum(sapply(vals1, n_needed, sq)) == 1299
sum(sapply(vals2, n_needed, border, a = 2)) == 315
sum(sapply(vals3, n_needed, hole, a = 1, b = 2)) == 652741
