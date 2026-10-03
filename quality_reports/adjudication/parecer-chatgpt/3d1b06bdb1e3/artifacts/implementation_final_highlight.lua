-- Marca as intervenções desta revisão no PDF, inclusive referências novas.
-- As divisões .codex-edit permanecem legíveis no fonte Markdown.
local new_references = {
  ["ref-deChaisemartin_DHaultfoeuille_2026"] = true,
  ["ref-chen_pearl_2013"] = true,
  ["ref-abadie_etal_2020"] = true,
  ["ref-mahoney_goertz_2006"] = true,
}

function Pandoc(doc)
  -- Citeproc precede a marcação para permitir colorir as entradas geradas.
  -- citeproc: false no YAML impede uma segunda bibliografia pelo rmarkdown.
  doc = pandoc.utils.citeproc(doc)
  if not FORMAT:match("latex") then return doc end
  return doc:walk({
    Div = function(div)
      if new_references[div.identifier] then
        -- Preservar o identificador csl-entry e a estrutura de lista CSL.
        -- Referências são parágrafos curtos; uma parbox evita o conflito
        -- do ambiente framed com o início de um \bibitem.
        for _, block in ipairs(div.content) do
          if block.content then
            block.content:insert(1, pandoc.RawInline("latex",
              "\\colorbox{shadecolor}{\\parbox{\\dimexpr\\linewidth-2\\fboxsep\\relax}{"))
            block.content:insert(pandoc.RawInline("latex", "}}"))
          end
        end
        return div
      elseif div.classes:includes("codex-edit") then
        div = div:walk({Note = function(note)
          -- As notas desta revisão têm um único parágrafo. Colorbox/parbox
          -- evita parágrafos literais no argumento salvo pelo pacote footnote.
          if #note.content == 1 and note.content[1].content then
            local inlines = note.content[1].content
            inlines:insert(1, pandoc.RawInline("latex",
              "\\colorbox{shadecolor}{\\parbox{\\dimexpr\\linewidth-2\\fboxsep-1.8em\\relax}{"))
            inlines:insert(pandoc.RawInline("latex", "}}"))
          else
            error("Uma nota em bloco codex-edit exige revisão da marcação amarela")
          end
          return note
        end})
        local blocks = pandoc.List()
        -- Salvar a nota ao fim de seu parágrafo, sem adiá-la até o fim de
        -- uma subseção inteira que pode ocupar várias páginas.
        for _, block in ipairs(div.content) do
          blocks:insert(pandoc.RawBlock("latex", "\\begin{codexedit}"))
          blocks:insert(block)
          blocks:insert(pandoc.RawBlock("latex", "\\end{codexedit}"))
        end
        return blocks
      elseif div.identifier == "refs" then
        -- Composição local da bibliografia: espaço para URLs e DOI longos,
        -- sem alterar o tamanho ou o alinhamento dos parágrafos do artigo.
        div.content:insert(1, pandoc.RawBlock("latex",
          "\\begingroup\n\\fontsize{11}{13}\\selectfont\n\\raggedright\n\\widowpenalty=10000\n\\clubpenalty=10000"))
        div.content:insert(pandoc.RawBlock("latex", "\\endgroup"))
        return div
      end
    end,
  })
end
