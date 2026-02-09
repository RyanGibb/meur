$-if(tags)$
Tags: $for(tags)$[$tag$](/$lowerTag$.md)$sep$, $endfor$.
$-endif$
$-if(doi)$
DOI: [https://doi.org/$doi$]($doi$).
$-endif$

$mdParsed$
$-if(video)$

[Video]($video$)
$-endif$

[Slides](/talks/slides/$name$.html)$if(url)$ | [URL]($url$)$endif$ | [BibTeX](/talks/$name$.bib)
$-if(mdAbstract)$

$mdAbstract$
$-endif$
