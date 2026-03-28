-- custom-boxes.lua
local envs = {
  ["promptbox"]    = true,
  ["insightbox"]   = true,
  ["alertbox"]     = true,
  ["answer"]       = true,
  ["responsebox"]  = true
}

-- Environments where code blocks need the Shaded fix
local code_envs = {
  ["responsebox"] = true,
  ["promptbox"]   = true
}

function Div(el)
  for cls, _ in pairs(envs) do
    if el.classes:includes(cls) then
      if FORMAT:match("latex") or FORMAT:match("pdf") then
        local blocks = pandoc.List({})

        if code_envs[cls] then
          blocks:insert(pandoc.RawBlock("latex",
            "\\begingroup\n" ..
            "\\renewenvironment{Shaded}" ..
            "{\\begin{tcolorbox}[enhanced,breakable,colback=gray!10," ..
            "boxrule=0pt,arc=0pt,left=4pt,right=4pt,top=2pt,bottom=2pt," ..
            "sharp corners]}{\\end{tcolorbox}}"))
        end

        blocks:insert(pandoc.RawBlock("latex", "\\begin{" .. cls .. "}"))
        blocks:extend(el.content)
        blocks:insert(pandoc.RawBlock("latex", "\\end{" .. cls .. "}"))

        if code_envs[cls] then
          blocks:insert(pandoc.RawBlock("latex", "\\endgroup"))
        end

        return blocks
      else
        return nil
      end
    end
  end
end