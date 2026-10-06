# Every data set of the package is a function. The tests call each of them
# once and check what a question or an exercise relies on: a data frame (or
# a labelled vector), a story in the label of the data set, and a label for
# every variable where the function sets them.

generators <- function() {

  ns <- asNamespace("umbra")
  nms <- setdiff(sort(getNamespaceExports(ns)),
                 c("rndPairs", "rndWord", "twoSamp"))

  stats::setNames(lapply(nms, get, envir = ns), nms)
}


# arguments for the generators that have none by default
bare <- function(f) {

  fm <- formals(f)
  req <- names(fm)[vapply(fm, function(z)
    is.symbol(z) && !nzchar(as.character(z)), logical(1))]

  # retouren() carries two unused compatibility arguments
  if(identical(req, "n")) list(n = 100) else list()
}


test_that("every generator runs and returns labelled data", {

  set.seed(1)

  for(nm in names(generators())) {

    f <- generators()[[nm]]
    res <- do.call(f, bare(f))

    expect_true(is.data.frame(res) || is.numeric(res), info = nm)
    expect_gt(NROW(res), 0)

    story <- attr(res, "label")
    expect_true(is.character(story) && length(story) == 1L && nzchar(story),
                info = nm)
  }
})


test_that("the generators leave the seed to the caller", {

  # design rules 8.3: no set.seed() inside, so the same seed gives the
  # same data and two calls in a row do not
  for(nm in c("choco", "reisekunden", "schnee", "alpvieh")) {

    f <- generators()[[nm]]

    set.seed(7); a <- f()
    set.seed(7); b <- f()
    d <- f()

    expect_identical(a, b, info = nm)
    expect_false(identical(a, d), info = nm)
  }

  # ... and none of them takes a 'seed' argument, exported or not
  ns <- asNamespace("umbra")
  hasSeed <- vapply(ls(ns, all.names = TRUE), function(nm) {
    f <- get(nm, envir = ns)
    is.function(f) && "seed" %in% names(formals(f))
  }, logical(1))

  expect_identical(names(hasSeed)[hasSeed], character(0))
})


test_that("the variable labels match the columns", {

  set.seed(1)

  d <- akku()
  expect_identical(lengths(lapply(d, attr, "label")),
                   c(id = 1L, akkutyp = 1L, zyklen = 1L))

  d <- reisekunden(50)
  expect_true(all(lengths(lapply(d, attr, "label")) == 1L))
  expect_match(attr(d, "label"), "Reiseb")

  # the label of the data set used to be lost in the final shuffle
  expect_match(attr(retouren(), "label"), "Garantief")

  # used to return the story instead of the data
  expect_s3_class(verzoegerung(), "data.frame")
})


test_that("the helpers taken over from the suite are called as it defines them", {

  set.seed(1)

  # mGsub(): text first
  expect_match(attr(hotdog(), "label"), "<em>m\u00e4nnlich</em>, <em>weiblich</em>")
  expect_match(attr(vegi(), "label"), "Grossverteiler")
  expect_false(grepl("&level_", attr(vegi(), "label"), fixed = TRUE))

  # num() for the codes of a factor, winsorize() with 'limits'
  d <- schulnote(200)
  expect_identical(nrow(d), 200L)
  expect_true(all(d$note >= 1 & d$note <= 6))

  expect_identical(nrow(fitness(60)), 60L)
  expect_identical(nrow(kredit(60)), 60L)
})


test_that("rndWord() and rndPairs() build what they are asked for", {

  set.seed(1)

  w <- rndWord(size = 5, length = 4)
  expect_length(w, 5L)
  expect_true(all(nchar(w) == 4L))
  expect_true(all(grepl("^[A-Z]+$", w)))
  expect_identical(rndWord(0, 3), character(0))
  expect_match(rndWord(1, 6, x = 0:9), "^[0-9]{6}$")

  d <- rndPairs(n = 2000, r = 0.7)
  expect_equal(cor(d$x, d$y), 0.7, tolerance = 0.05)

  d <- rndPairs(n = 400, r = 0.5, prop = c(0.25, 0.5, 0.25))
  expect_equal(as.vector(prop.table(table(d$x))), c(0.25, 0.5, 0.25))
})
