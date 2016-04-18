emacs = {}
function emacs.completion_candidates(str)
  debug.print_line(str)
  completions = mod_query.do_complete_lua(_ENV, str)
  el_list = ""
  for i, c in ipairs(completions) do
    el_list = el_list..c.." "
  end
  return el_list
end
