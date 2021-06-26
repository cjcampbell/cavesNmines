library(litsearchr)

mysearchterms <- list(
    c(
      "Lasiurus",
      "Lasionycteris",
      "red bat",
      "yellow bat",
      "hoary bat",
      "Aeorestes",
      "silver-haired bat",
      # NEW:
      "Atalapha"
      ),
    c(
      "cave", "mine", "grotto", "quarry", "underground", "subterranean", "cavern", "tunnel","pit",
      # French
      "grotte", "mienne", "grotte", "carrière", "souterraine", "caverne", "tunnel", "fosse",
      # Spanish
      "cueva", "mina", "gruta", "cantera", "subterráneo", "caverna", "túnel", "pozo"
    )
  )

my_search <-
  litsearchr::write_search(
    groupdata = mysearchterms,
    #languages = c("English", "Spanish", "French"),
    languages = "English",
    stemming = TRUE,
    closure = "none",
    exactphrase = TRUE,
    writesearch = FALSE,
    verbose = TRUE,
    directory = file.path(wd$bin, "litSearchResults")
  )

my_search

# gold_standard <-
#   c(
#     "A Record of a Silver-Haired Bat in a Cave",
#     "A West Virginia cave record for the silver-haired bat",
#     "Hoary Bat Skull in an Indiana Cave"
#   )
# 
# title_search <- litsearchr::write_title_search(titles=gold_standard)
# 
# retrieved_articles <-
#   litsearchr::import_results(directory = wd$bin, verbose = TRUE)
# retrieved_articles <- litsearchr::remove_duplicates(retrieved_articles, field="title", method="string_osa")
# retrieved_articles$title
