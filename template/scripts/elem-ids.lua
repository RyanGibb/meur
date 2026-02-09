-- should also be possible to use a hash as the id
-- although this will change the id even with minor changes (e.g. typo)
-- lua hides hashing impelmentation, but look at https://github.com/lunarmodules/md5
para_counter = 0
image_counter = 0
header = ""

local filter = {
  -- required so images are processed with the correct `header`
  traverse = 'topdown',

  -- keep track of the header we're in
  Header = function (h)
    prev_header = header
    header = h.identifier .. "-"
    if header ~= prev_header then
      image_counter = 0
	    para_counter = 0
    end
  end,

  -- need to add span because we can't add arbitrary attributes https://github.com/jgm/pandoc/issues/684
  Para = function (para)
    -- don't wrap image if it's the only content of paragraph as it will break captioning
    -- (https://pandoc.org/MANUAL.html#images)
    if para.content[2] == nil and para.content[1].t == "Image" then
      return para
    end
    local para_id = pandoc.Para(pandoc.Span(para.content, {id = header .. "para-" .. para_counter, class = nil}))
    para_counter = para_counter + 1
    return para_id
  end,

  Image = function (image)
    image.identifier = header .. "img-" .. image_counter
    image_counter = image_counter + 1
    return image
  end,

  --Str = function (str)
  --  local str_id = pandoc.Span(str, {id = header .. "str-" .. counter, class = nil})
  --  counter=counter+1
  --  return str_id
  --end,
}
return {filter}
