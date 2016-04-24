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
  local dot = string.find(fname, ".", nil, true)
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

WMPlex.geom

WGroupWS == amt.__index
-- true
-- 
a = current_workspace()
amt = getmetatable(a)
amt.__index
idx = amt.__index
idx.
a:geom
-- function: 0x14a5b20
-- 
-- function: 0x14a5b20
-- 

b={}
type(b)
-- "table"
-- 
type(a)
-- "userdata"
-- 
type(WMPlex)
-- "table"
-- 
getmetatable(WMPlex) == getmetatable(getmetatable (WMPlex))
-- false
-- 

emacs.canonical_funcname("WMPlex.geom")
-- "WRegion.geom"
-- 
-- "WRegion"
-- 

a = current_workspace()
a.__parentclass
-- "\
-- bottom: function: 0x152f6e0\
-- attach: function: 0x152f650\
-- managed_i: function: 0x152f940\
-- __typename: WGroup\
-- set_bottom: function: 0x152f8d0\
-- set_fullscreen: function: 0x152f750\
-- __parentclass: \
-- +-rootwin_of: function: 0x152c780\
-- +-screen_of: function: 0x1529810\
-- +-set_activity: function: 0x152cfc0\
-- +-manager: function: 0x15298c0\
-- +-current: function: 0x1529730\
-- +-set_tagged: function: 0x15297a0\
-- +-groupleader_of: function: 0x152d640\
-- +-goto: function: 0x1529850\
-- +-geom: function: 0x152d4f0\
-- +-is_active: function: 0x152c9b0\
-- +-goto_focus: function: 0x1529930\
-- +-parent: function: 0x152ca20\
-- +-rqclose: function: 0x15296c0\
-- +-rqorder: function: 0x152cf50\
-- +-is_activity: function: 0x152d560\
-- +-set_name_exact: function: 0x152d1b0\
-- +-get_configuration: function: 0x152d360\
-- +-__typename: WRegion\
-- +-size_hints: function: 0x152d030\
-- +-rqclose_propagate: function: 0x152d230\
-- +-name: function: 0x152d480\
-- +-goto_: function: 0x152d2e0\
-- +-__return_target: function: 0x152d270\
-- +-begin_kbresize: function: 0x152d6b0\
-- +-is_mapped: function: 0x152d140\
-- +-__parentclass: \
-- | +-__typename: Obj\
-- +-is_tagged: function: 0x152d5d0\
-- +-rqgeom: function: 0x152d440\
-- +-set_name: function: 0x152d0a0\
-- +-display: function: 0x152d3d0\
-- attach_new: function: 0x152f5e0"
-- -- "WGroup"
-- table: 0x152f3c0
;; userdata: 0x1676ac8
