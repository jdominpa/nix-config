local ls = require("luasnip")
local s = ls.s
local i = ls.i
local t = ls.t
local c = ls.choice_node
local f = ls.function_node
local d = ls.dynamic_node
local sn = ls.snippet_node

local fmta = require("luasnip.extras.fmt").fmta
local rep = require("luasnip.extras").rep

-- Snippets
local snippets, autosnippets = {}, {}

local package = s(
  {
    trig = "pkg",
    dscr = "Add package",
  },
  fmta("\\usepackage{<>}", i(1))
)
table.insert(snippets, package)

local package_opt = s(
  {
    trig = "pkgo",
    dscr = "Add package with optional parameters",
  },
  fmta("\\usepackage[<>]{<>}", { i(1), i(2) })
)
table.insert(snippets, package_opt)

return snippets, autosnippets
