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

local environment = s(
  {
    trig = "env",
    dscr = "New environment",
  },
  fmta([[
  \begin{<>}
    <>
  \end{<>}
  ]], { i(1), i(2), rep(1) })
)
table.insert(snippets, environment)

local enumerate = s(
  {
    trig = "enum",
    dscr = "Enumeration environment",
  },
  fmta([[
  \begin{enumerate}<>
    \item <>
  \end{enumerate}
  ]], {
    c(1, { t(""), t("[(i)]"), t("[(a)]") }),
    i(2),
  })
)
table.insert(snippets, enumerate)

local equation = s(
  {
    trig = "eq",
    dscr = "Equation environment",
  },
  fmta([[
  \begin{equation}
    <>
  \end{equation}
  ]], i(1))
)
table.insert(snippets, equation)

return snippets, autosnippets
