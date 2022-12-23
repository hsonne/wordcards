author <- list(
  name = "Hauke Sonnenberg", 
  orcid = "0000-0001-9134-2871",
  url = "https://github.com/hsonne"
)

description <- list(
  name = "wordcards", 
  title = "Create Word or Syllable Cards from Text", 
  desc  = paste(
    "This package provides functions to analyse the frequency of words or", 
    "syllables in a given text. It provides a function to create a PDF file", 
    "with all words or syllables printed on cards, together with their", 
    "frequencies. The idea is to use these cards to help children learn to",
    "read."
  )
)

kwb.pkgbuild::use_pkg(
  author, 
  description, 
  version = "0.0.0.9000", 
  stage = "experimental"
)
