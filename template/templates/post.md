$-if(published)$
Published $published$.
$-endif$
$-if(updated)$
Last updated $updated$.
$-endif$
$-if(tags)$
Tags: $for(tags)$[$tag$](/$lowerTag$.md)$sep$, $endfor$.
$-endif$
$-if(doi)$
DOI: [https://doi.org/$doi$]($doi$).
$-endif$

$body$
