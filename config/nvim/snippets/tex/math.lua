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

-- Expand conditions
local make_condition = require("luasnip.extras.conditions").make_condition
local line_begin = require("luasnip.extras.expand_conditions").line_begin

local in_mathzone = make_condition(function()
  return vim.fn['vimtex#syntax#in_mathzone']() == 1
end)

-- Snippets
local snippets, autosnippets = {}, {}

local inline_math = s(
  {
    trig = "mm",
    dscr = "Inline math environment",
    hidden = true,
  },
  fmta("\\(<>\\)", i(1)),
  { condition = -in_mathzone }
)
table.insert(autosnippets, inline_math)

local display_math = s(
  {
    trig = "dm",
    dscr = "Display math environment",
    hidden = true,
  },
  fmta([[
  \[
    <>
  \]
  ]], i(1)),
  { condition = line_begin * -in_mathzone }
)
table.insert(autosnippets, display_math)

local subindex = s(
  {
    trig = "__",
    dscr = "Subscript with {}",
    wordTrig = false,
    hidden = true,
  },
  fmta("_{<>}", i(1)),
  { condition = in_mathzone }
)
table.insert(autosnippets, subindex)

local superindex = s(
  {
    trig = "^^",
    dscr = "Superscript",
    wordTrig = false,
    hidden = true,
  },
  fmta("^{<>}", i(1)),
  { condition = in_mathzone }
)
table.insert(autosnippets, superindex)

local fraction = s(
  {
    trig = "ff",
    dscr = "Fraction",
    wordTrig = false,
    hidden = true,
  },
  fmta("\\frac{<>}{<>}", { i(1), i(2) }),
  { condition = in_mathzone }
)
table.insert(autosnippets, fraction)

local sum = s(
  {
    trig = "sum",
    dscr = "Sum with sigma notation",
    wordTrig = false,
  },
  fmta("\\sum_{<>}^{<>} <>", {
    i(1, "n=0"),
    i(2, "\\infty"),
    i(3),
  }),
  { condition = in_mathzone }
)
table.insert(snippets, sum)

local integral = s(
  {
    trig = "int",
    dscr = "Integral",
    wordTrig = false,
  },
  fmta("\\int_{<>}^{<>} <>\\,d<>", {
    i(1, "-\\infty"),
    i(2, "\\infty"),
    i(3),
    i(4, "x"),
  }),
  { condition = in_mathzone }
)
table.insert(snippets, integral)

return snippets, autosnippets
