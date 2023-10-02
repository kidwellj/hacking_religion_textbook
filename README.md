# Theology, Religious Studies, and Data Science

An open textbook introducing data science to religious studies.

You can view a live demo of the book here: [https://kidwellj.github.io/hacking_religion_textbook/intro.html]

The course here has been compiled with [quarto](https://quarto.org/), and so the live instance of the course is compiled from openly accessible resources located in this repository. If you're interested in doing something similar, there are a number of other options, some of which have lamentably turned commercial, including:  [bookdown](https://github.com/rstudio/bookdown), [gitbook](https://docs.gitbook.com/), [mkdocs](https://www.mkdocs.org/), [readthedocs](https://readthedocs.org) which technically uses [Sphinx](http://www.sphinx-doc.org/en/master/) or [daux](https://daux.io/).

Directory structure includes:
* `README.md` this README file displayed on Github
* `_book` a folder containing the compiled book in html and .pdf formats
* `references.bib` a bibliography of items used for the course in [BibTeX format](http://www.bibtex.org/Format/)
* `preface.qmd` Preface
* `intro.qmd` Introduction and overview to book
* `chapter_1.qmd` Chapter 1: working with UK Census religion data
* `chapter_2.qmd` Chapter 2: survey data
* `chapter_3.qmd` Chapter 3: geospatial data"
* `chapter_4.qmd` Chapter 4: data scraping, corpus analysis and wordclouds
* `chapter_5.qmd` What's next?

# How to produce books from this repository:

1. clone this repository using `git clone https://github.com/kidwellj/hacking_religion_textbook.git` ([install git](https://git-scm.com/downloads) if you haven't already)
2. [install quarto](https://quarto.org/docs/get-started/)
3. change to the `hacking_religion` subdirectory and run `quarto preview`
4. alternatively you can render a copy of the book using `quarto render`

# Copyright

Content here, unless otherwise indicated are copyright by Jeremy H. Kidwell. Please re-use them as they are covered by Creative Commons Attribution 4.0 International Licence (CC BY 4.0).
