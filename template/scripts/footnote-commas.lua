
LIST = false

RESET = function(i)
  LIST = false
  return i
end

local filter = {
  traverse = 'topdown',

  Note = function (n)
    if LIST then
      n = pandoc.Span({ pandoc.Superscript(pandoc.Str ", "), n }, {class = "footnote-ref-wrapper"})
    end
    LIST = true
    return n,
    false -- stop traversal hereThe traversal order of filters can be selected by setting the key traverse to either 'topdown' or 'typewise'; the default is 'typewise'.
  end,

  Para = RESET,
  Str = RESET,
  Space = RESET,
  Header = RESET,
  Figure = RESET,
}
return {filter}
