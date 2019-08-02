
question <- function(...)
{
  Call <- match.call()
  Call[[1]] <- quote(code.chunk)
  Call2 <- Call
  Call$chunk.opts <- "r eval=FALSE"
  Call2$chunk.opts <- "r"
  Call$co <- Call2$co <- NULL
  list(
    "##",
    eval(Call, parent.frame()),
    "##",
    eval(Call2, parent.frame())
  )
}


library(arsenal)
write2(list(
  yaml(
    title = "aRRRRRgh",
    subtitle = "When R Doesn't Do What You Expect",
    author = "Ethan Heinzen",
    date = "2019-08-01"
  ),
  "<style>div.font10 pre {font-size: 10px; line-height: 16px;}</style>",
  code.chunk(
    chunk.opts = "r setup, include=FALSE",
    knitr::opts_chunk$set(echo=TRUE, message=TRUE, warning=TRUE, error=TRUE, tidy = FALSE)
  ),
  "## Inspiration",
  paste("This talk was inspired by a talk by Hadley Wickham at rstudio::conf 2019 about the `vctrs` package:",
        "https://resources.rstudio.com/rstudio-conf-2019/vctrs-tools-for-making-size-and-type-consistent-functions"),
  paste("which was itself inspired by a talk about Ruby and Javascript of the same nature:",
  "https://www.destroyallsoftware.com/talks/wat."),
  "A few examples were also inspired by The R Inferno: https://www.burns-stat.com/pages/Tutor/R_inferno.pdf.",
  "# What do you expect the following commands to do?",
  question(c(factor("A"), factor("B"))),
  question(unlist(list(factor("A"), factor("B")))),
  question(c(Sys.Date(), Sys.time()), c(Sys.time(), Sys.Date())),
  question(c(Sys.Date(), NULL), c(NULL, Sys.Date())),
  question(
    now <- Sys.time(),
    ten.s.ago <- now - 10,
    one.hr.ago <- now - 3600,
    c(as.numeric(now - ten.s.ago), as.numeric(now - one.hr.ago))
  ),
  question(is.integer(1), is.integer(1:1)),
  question(0.3 == 0.1*3),
  question(sum(rep(1, each=100*(1-0.8)))),
  question(as.integer(100*10.1), as.integer(100*10.2)),
  question(as.character(1e5)),
  question(round(2.5) - round(1.5)),
  question(0^0, sum(), prod()),
  question(seq(0:5)),
  question(mean(1, 2, 3), sum(1, 2, 3)),
  question(100 %in% 100L*(1:5)),
  "##\n\n```{r eval=FALSE}\n!TRUE + TRUE + (!FALSE + FALSE)\n(!TRUE + TRUE) + !FALSE + FALSE\n```",
  "##\n\n```{r}           \n!TRUE + TRUE + (!FALSE + FALSE)\n(!TRUE + TRUE) + !FALSE + FALSE\n```",
  # question(FALSE && TRUE == FALSE),
  # question(!FALSE %in% TRUE, !"a" %in% "b"),
  question(if(1 * 0 && 1 / 0) 1 else 2, if(1 / 0 && 1 * 0) 1 else 2),
  # question(if(FALSE) stop("Throw an error") else "No error"),
  # question(a <- c(TRUE, FALSE), if(a && TRUE) 1 else 2, if(a & TRUE) 1 else 2),
  question(
    abc <- c("A", "B", "C"),
    ifelse(c(TRUE, FALSE, NA), factor(abc), abc)
  ),
  question(if(50 < "7") 1 else 2),
  question(m <- matrix(1, nrow = 2, ncol = 2), m[2, 1] <- 2, m[2, 1], m[i = 2, j = 1], m[j = 1, i = 2]),
  question(lst <- list(a = 1, b = 2), lst[], lst[c()]),
  question(i1 <- c(TRUE, TRUE, FALSE), i2 <- c(TRUE, FALSE, TRUE),
           data.frame(a = 1:3, b = 2:4)[i1 & i2, ], data.frame(a = 1:3, b = 2:4)[i1 && i2, ]),
  question(tmp <- factor(c("b", "a"), levels = c("b", "a")), c(a = 1, b = 2)[tmp],
           tmp <- factor(c("b", "a")), levels(tmp) <- c("b", "a"), c(a = 1, b = 2)[tmp]),
  "##\n\n```{r eval=FALSE}\nL <- list('`a`' = 1, `b` = 2)\nL$`a`\nL$`b`\n```",
  "##\n\n```{r}           \nL <- list('`a`' = 1, `b` = 2)\nL$`a`\nL$`b`\n```",
  question(list(a = 1, b = 2, a = 3)$a),
  question(list(aa = 1, ab = 2)$a, list(aa = 1)$a),
  question(identical(NA, c("", NA)[2])),
  question(
    n_col <- function(x) {
      out <- 0
      for(i in 1:ncol(x)) out <- out + 1
      out
    },
    d <- data.frame(x = 1),
    d$y <- data.frame(z = 2),
    n_col(d),
    n_col(d$y)
  ),
  question(
    m <- matrix(1:4, nrow = 2),
    n_col(m),
    n_col(m[, 1]),
    n_col(m[, 0])
  ),
  question(
    x <- 1:5,
    y <- c(x, 0),
    z <- x + y,
    z[length(z)]
  ),
  # question(
  #   "",
  #   myvec <- 1:10,
  #   length(myvec) <- 3,
  #   myvec
  # ),
  question(
    fun2 <- function(..., a) a + 1,
    fun1 <- function(arg1, ...) fun2(...),
    fun1(a = 3)
  ),

  "##\n\n```{r eval=FALSE}\nx <- y <- 1\nwhile(x <------- y) y <- y - 1\nx\n```",
  "##\n\n```{r}           \nx <- y <- 1\nwhile(x <------- y) y <- y - 1\nx\n```",
  "##\n\n```{r eval=FALSE}\n1 +-+-+ 1\n```",
  "##\n\n```{r}           \n1 +-+-+ 1\n```",
  question(
    e <- environment(),
    trash <- Map(function(x, y) assign(x, y, e), LETTERS, (1:26)-20),
    A <- NA_real_,
    sum(vapply(LETTERS, get, 0, e), na.rm = T),
    sum(vapply(LETTERS, get, 0, e), na.rm = F)
  ),
  # question(
  #   "",
  #   x <- 1:10,
  #   class(x) <- c("myclass", class(x)),
  #   x[1] <- "a",
  #   is.numeric(x),
  #   is.integer(x)
  # )
  question(
    f <- function() {x <- "super secret PHI"; a ~ b},
    (form <- f()),
    environment(form)$x
  ),
  "## Session Info",
  "<div class='font10'>",
  verbatim(sessionInfo()),
  "</div>"
), file = "aRRRRRgh.html", output_format = rmarkdown::ioslides_presentation())
