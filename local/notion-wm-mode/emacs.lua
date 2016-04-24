-- -*- mode: notion-wm; -*-

emacs = {}

function emacs.completion_candidates(str)
  local env = _ENV or getfenv()
  local completions = mod_query.do_complete_lua(env, str)
  return table.concat(completions, " ")
end

-- _Attempts_ to pretty print `obj`
function emacs.pprint(obj)
  if type(obj) == "table" then
    return table_to_string(obj)
  else
    return obj
  end
end

function parse_fname(fname)
  local dot = string.find(fname, "[.:]", nil)
  local tabpart, funpart
  if dot then
    tabpart = string.sub(fname, 1, dot-1)
    funpart = string.sub(fname, dot+1)
  else
    funpart = fname
  end
  return funpart, tabpart
end


function emacs.smart_loadstring(lua_code)
  local fn, err = loadstring("return "..lua_code)
  if not fn then
    fn, err = loadstring(lua_code)
  end
  return fn, err
end

function emacs.eval(lua_code)
  debug.print_line(lua_code)
  fn, err = emacs.smart_loadstring(lua_code)
  if err then
    error(err)
  else
    return fn()
  end
end


-- Walk upwards in the metatable tree looking for the owner of the function
-- Returns the _name_ (string)
-- notion class system specific, but might be equivalent to replace .__parent with get
-- metatable?
function emacs.function_owner(tab, funstr)
  if not tab or not tab.__typename then
    return nil -- we only understand notion classes
  end
  while tab do
    debug.print_line(tostring(tab))
    if rawget(tab, funstr) then
      return tab
    else
      tab = tab.__parentclass
    end
  end
end

function emacs.canonical_funcname(fname)
  -- fname: WMPlex.geom or 
  funpart, tabpart = parse_fname(fname)

  if not tabpart then
    return fname
  end

  local tab = _ENV[tabpart]
  if type(tab) == "userdata" then
    tab = getmetatable(tab)
    if not tab then
      return fname
    end
    tab = tab.__index
  end

  local owner = emacs.function_owner(tab, funpart)

  if not owner or not owner.__typename then
    return fname
  end

  return owner.__typename.."."..funpart
end
