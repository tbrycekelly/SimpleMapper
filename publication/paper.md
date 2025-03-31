---
title: 'SimpleMapper: A Simple Alternative for Mapping Data in R'
tags:
  - R
  - geographic
  - mapping
  - oceanography
authors:
  - name: Thomas Bryce Kelly
    orcid: 0000-0000-0000-0000
    corresponding: true # (This is how to denote the corresponding author)
    affiliation: "1" # (Multiple affiliations must be quoted)
  - name: Laura M. Whitmore
    affiliation: "2"
    orcid: 0000-0000-0000-0000
affiliations:
 - name: College of Fisheries and Ocean Science, University of Alaska Fairbanks, Fairbanks, AK, USA
   index: 1
 - name: International Arctic Research Center, University of Alaska Fairbanks, Fairbanks, AK, USA
   index: 2
date: 20 February 2025
bibliography: paper.bib

---

# Summary



# Statement of need

While many mapping libraries exist for the `R` programming language (e.g. [@]), the packages require significant training or knowledge of geographic information systems (GIS) to use as intended. Such rigor is essential for precise measuremernts and independent replicability, yet many applications in earth science are far less stringent (e.g., planning out the location of study sites, indicating where samples were collected, or applying data overlayers to provide spatial context). `SimpleMapper` was designed with the following goals: (1) easy to learn by non-specialists, (2) readily adaptable by scientists (not software developers), and (3) be minimially relient on software dependencies. Below we will first describe some uses of the package for scientific studies, then we will elaborate on the three design criteria for `SimpleMapper`, including an overview of implementation.

# Case-studies



# Design criteria and implementation

## 1. Easy to learn


## 2. Readily Adaptable


## 3. Minimal relience


# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

If you want to cite a software repository URL (e.g. something on GitHub without a preferred
citation) then you can do it with the example BibTeX entry below for @fidgit.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

# Figures

Figures can be included like this:
![Caption for example figure.\label{fig:example}](figure.png)
and referenced from text using \autoref{fig:example}.

Figure sizes can be customized by adding an optional second parameter:
![Caption for example figure.](figure.png){ width=20% }

# Acknowledgements



# References