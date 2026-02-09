$-if(mdPrev)$
Previous: $mdPrev$
$-endif$
$-if(mdNext)$
Next: $mdNext$
$-endif$
$-if(tags)$
Tags: $for(tags)$[$tag$](/$lowerTag$.md)$sep$, $endfor$.
$-endif$

$body$
