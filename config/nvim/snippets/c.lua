local ls = require("luasnip")
local s = ls.s
local i = ls.i
local t = ls.t
local c = ls.choice_node
local f = ls.function_node
local d = ls.dynamic_node
local sn = ls.snippet_node

local fmt = require("luasnip.extras.fmt").fmt
local rep = require("luasnip.extras").rep

local snippets, autosnippets = {}, {}

local for_loop = s(
  {
    trig = "for([%w_]+)",
    dscr = "For loop",
    regTrig = true,
    hidden = true,
  },
  fmt([[
  for (int {} = {}; {} < {}; {}++) {{
      {}
  }}
  ]],
  {
    d(1, function(_, snip)
      return sn(1, i(1, snip.captures[1]))
    end),
    i(2, "0"),
    rep(1),
    i(3, "n"),
    rep(1),
    i(4, "/* TODO */"),
  }))
table.insert(snippets, for_loop)

local while_loop = s(
  {
    trig = "while",
    dscr = "While loop",
  },
  fmt([[
  while ({}) {{
      {}
  }}
  ]],
  {
    i(1, "true"),
    i(2, "/* TODO */"),
  }))
table.insert(snippets, while_loop)

local do_while_loop = s(
  {
    trig = "do",
    dscr = "Do-while loop",
  },
  fmt([[
  do {{
      {}
  }} while ({});
  ]],
  {
    i(1, "/* TODO */"),
    i(2, "true"),
  }))
table.insert(snippets, do_while_loop)

local if_statement = s(
  {
    trig = "if",
    dscr = "If statement",
  },
  fmt([[
  if ({}) {{
    {}
  }}
  ]],
  {
    i(1, "condition"),
    i(2, "/* TODO*/"),
  }))
table.insert(snippets, if_statement)

local else_statement = s(
  {
    trig = "else",
    dscr = "Else condition",
  },
  fmt([[
  else {{
      {}
  }}
  ]], i(1, "/* TODO */")))
table.insert(snippets, else_statement)

local else_if = s(
  {
    trig = "elif",
    dscr = "Else if",
  },
  fmt([[
  else if ({}) {{
      {}
  }}
  ]],
  {
    i(1, "condition"),
    i(2, "/* TODO */"),
  }))
table.insert(snippets, else_if)

local include = s(
  {
    trig = "inc",
    dscr = "#include <>",
  },
  fmt("#include <{}>", i(1, "stdio.h")))
table.insert(snippets, include)

local include_local = s(
  {
    trig = "incl",
    dscr = "#include \"\"",
  },
  fmt("#include \"{}\"", i(1, "local.h")))
table.insert(snippets, include_local)

local main = s(
  {
    trig = "main",
    dscr = "Main function",
  },
  fmt([[
  int main(int argc, char *argv[])
  {{
      {}

      return 0;
  }}
  ]],
  {
    i(1, "/* TODO */")
  }))
table.insert(snippets, main)

local fn = s(
  {
    trig = "fn",
    dscr = "New function",
  },
  fmt([[
  {} {}({})
  {{
      {}
  }}
  ]],
  {
    i(1, "void"),
    i(2, "func"),
    i(3, "void"),
    i(4, "/* TODO */"),
  }))
table.insert(snippets, fn)

local malloc = s(
  {
    trig = "mal",
    dscr = "Memory allocation using malloc",
  },
  fmt([[
  {} *{} = ({} *) malloc({} * sizeof({}));
  if ({} == NULL) {{
      fprintf(stderr, "Memory allocation failed\n");
      {}
  }}
  ]],
  {
    i(1, "double"),
    i(2, "vec"),
    rep(1),
    i(3, "n"),
    rep(1),
    rep(2),
    i(4, "exit(1);"),
  }))
table.insert(snippets, malloc)

local calloc = s(
  {
    trig = "cal",
      dscr = "Memory allocation using calloc (same as malloc but sets allocated memory to 0)",
  },
  fmt([[
  {} *{} = ({} *) calloc({}, sizeof({}));
  if ({} == NULL) {{
      fprintf(stderr, "Memory allocation failed\n");
      {}
  }}
  ]],
  {
    i(1, "double"),
    i(2, "vec"),
    rep(1),
    i(3, "n"),
    rep(1),
    rep(2),
    i(4, "exit(1);"),
  }))
table.insert(snippets, calloc)

return snippets, autosnippets
