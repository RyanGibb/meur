$-if(tags)$
Tags: $for(tags)$[$tag$](/$lowerTag$.md)$sep$, $endfor$.
$-endif$
$-if(doi)$
DOI: [https://doi.org/$doi$]($doi$).
$-endif$

$mdParsed$

[PDF](/papers/$name$.pdf)$if(url)$ | [URL]($url$)$endif$ | [BibTeX](/papers/$name$.bib)
$-if(mdAbstract)$

$mdAbstract$
$-endif$
