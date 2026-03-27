-- custom-boxes.lua
-- Converts ::: promptbox, ::: insightbox, ::: alertbox, ::: answer
-- into \begin{...} ... \end{...} for LaTeX output,
-- and leaves them as styled <div>s for HTML.

local envs = {
  ["promptbox"]  = true,
  ["insightbox"] = true,
  ["alertbox"]   = true,
  ["answer"]     = true
}

function Div(el)
  for cls, _ in pairs(envs) do
    if el.classes:includes(cls) then
      if FORMAT:match("latex") or FORMAT:match("pdf") then
        local open  = pandoc.RawBlock("latex", "\\begin{" .. cls .. "}")
        local close = pandoc.RawBlock("latex", "\\end{" .. cls .. "}")
        local blocks = pandoc.List({open})
        blocks:extend(el.content)
        blocks:insert(close)
        return blocks
      else
        -- HTML: Pandoc already wraps in <div class="cls">, nothing to do
        return nil
      end
    end
  end
end
