# Theology, Religious Studies, and Data Science

An open textbook introducing data science to religious studies.

The course here has been compiled with [quarto](https://quarto.org/), and so the live instance of the course is compiled from openly accessible resources located in this repository. If you're interested in doing something similar, there are a number of other options, some of which have lamentably turned commercial, including:  [bookdown](https://github.com/rstudio/bookdown), [gitbook](https://docs.gitbook.com/), [mkdocs](https://www.mkdocs.org/), [readthedocs](https://readthedocs.org) which technically uses [Sphinx](http://www.sphinx-doc.org/en/master/) or [daux](https://daux.io/).

Directory structure includes:
* `README.md` this README file displayed on Github
* `docs` a folder containing the compiled book in html, .pdf and epub formats
* `course.bib` a bibliography of items used for the course in [BibTeX format](http://www.bibtex.org/Format/)
* `index.Rmd` Contains initialization settings, and preface content
* `01-Overview.Rmd` Introduction and overview to course
* `02-Session1.Rmd` First chapter: "what is data?"
* `03-Session2.Rmd` Second chapter: "copyright, licenses, and data as property"
* `04-Session3.Rmd` Third chapter "exploring confidentiality and privacy"
* `05-Session4.Rmd` Fourth and closing chapter: "how do we decide what to do?"

# How to produce books from this repository:

1. clone this repository using `git clone https://github.com/kidwellj/hacking_religion_textbook.git` ([install git](https://git-scm.com/downloads) if you haven't already)
2. [install quarto](https://quarto.org/docs/get-started/)
3. change to the `hacking_religion` subdirectory and run `quarto preview`
4. alternatively you can render a copy of the book using `quarto render`

# Copyright

Content here, unless otherwise indicated are copyright by Jeremy H. Kidwell. Please re-use them as they are covered by Creative Commons Attribution 4.0 International Licence (CC BY 4.0).
