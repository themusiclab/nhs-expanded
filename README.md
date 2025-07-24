# The Expanded Natural History of Song Discography, a global corpus of vocal music

This is the repository for "The Expanded Natural History of Song Discography, a global corpus of vocal music" (Bertolo et al., 2025, *Open Mind*). You can read the paper at https://direct.mit.edu/opmi/article/doi/10.1162/opmi.a.4/131815 and access the corpus at https://doi.org/10.5281/zenodo.8237500.

## Anatomy of the repo 
`NHS2-manuscript.Rmd` contains all writing, analyses, and bibliography for the generation of the final manuscript file `NHS2-manuscript.pdf`. `themusiclab.bib` contains relevant bibtex keys, and `apa-auto_cofirsts.csl` specifies APA style citation formatting. 

`/data` contains corpus metadata (`metadata.csv`) and acoustic features extracted from the corpus audio files (`audio_Extraction_raw_median_all.csv`).

`/analysis` contains code for audio feature extraction (`featureExtraction.m`), LASSO permutation tests (`LASSO_permtest.Rmd`), and ICC computation (`nhs2-ICC.Rmd`).

`/results` contains results from LASSO and ICC analyses.

`/viz` contains .R files for the dynamic generation of figures 2 through 5, whose output is stored in this same folder as `.png` files that `NHS2-manuscript.Rmd` reads in. 

## Assistance

If you need help with any materials associated with this project, please contact Mila Bertolo (mila.bertolo@mail.mcgill.ca), Marty Snarskis (msna962@aucklanduni.ac.nz), and Samuel Mehr (sam@auckland.ac.nz).

[![DOI](https://zenodo.org/badge/939009168.svg)](https://doi.org/10.5281/zenodo.15717353)
