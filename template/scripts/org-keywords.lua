local keywords_map = {
  ["TODO"] = "O",
  ["MOVE"] = ">",
  ["DONE"] = "X",
  ["KILL"] = "~",
  ["PROJ"] = "#",
}

local function process_first_inline(content_list)
  if content_list and #content_list > 0 then
    local first_inline = content_list[1]
    local keyword_text = nil
    local inner_str_element = nil
    -- Check if the first element is a Span containing a Str (Pandoc keyword)
    if first_inline.t == "Span" and
       first_inline.content and
       #first_inline.content == 1 and
       first_inline.content[1].t == "Str" then
      inner_str_element = first_inline.content[1]
      keyword_text = inner_str_element.text
      -- If we found a keyword we want to replace
      if keyword_text and keywords_map[keyword_text] then
        local replacement_symbol = keywords_map[keyword_text]
        -- Modify the text of the inner Str element
        inner_str_element.text = replacement_symbol
      end
    end
  end
end

function Header (header)
  -- Modify content in place
  process_first_inline(header.content)
  return header
end

function Para (para)
  process_first_inline(para.content)
  return para
end
