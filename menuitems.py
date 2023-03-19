import readline 
import inspect
import atexit


MENUITEM_ATTR  = "_menuitem"
MENUARG_ATTR   = "_menuarg"

def _get_exception_info(e):
  exception_traceback = e.__traceback__
  while exception_traceback.tb_next is not None:
    exception_traceback = exception_traceback.tb_next
  filename = exception_traceback.tb_frame.f_code.co_filename
  line_number = exception_traceback.tb_lineno
  return (filename, line_number)

def _func_num_args(func):
  sig = inspect.signature(func)
  return len(sig.parameters)
  
def set_metadata(key_val_pairs):
  def _wrapper(func):
    for k,v in key_val_pairs.items():
      setattr(func, k, v)
    return func
  return _wrapper

def menuitem_arg(_menu_item_name, arg_name):
  return set_metadata({MENUARG_ATTR: dict(name=_menu_item_name, option=arg_name)})

def menuitem_option(_menu_item_name, **args):
  return set_metadata({ MENUITEM_ATTR : dict(name=_menu_item_name, args=args) })


ORG_FUNCTION_HANDLE = "__org_func__"

def need_params(*names):
  def _wrap(org_func):
    def _proxy_func(*a,**kw):
      for name in names:
        if name not in kw:
          raise Exception(f"Please provide param : {name}=... ")
      return org_func(*a,**kw)
    setattr(_proxy_func, MENUARG_ATTR, getattr(org_func, MENUARG_ATTR, None))
    setattr(_proxy_func, MENUITEM_ATTR, getattr(org_func, MENUITEM_ATTR, None))
    setattr( _proxy_func, ORG_FUNCTION_HANDLE, org_func)
    return _proxy_func
  return _wrap


def _list_item_or_none(collection, n):
  if collection is None:
    return None
  if n >= len(collection):
    return None
  return collection[n] 

HISTFILE=".history"
atexit.register( readline.write_history_file, HISTFILE)

class NoSuchOptionException(Exception):
  pass

class CmdMenu:
  _menu = None
  def __init__(self):
    self._menu = {}
    self.prompt = "foo: "
    
    readline.set_completer(self._complete_func)
    readline.parse_and_bind("tab: complete")
    readline.set_completer_delims(' \t\n=')
    try:
      readline.read_history_file(HISTFILE)
    except : 
      pass
    
    for item in dir(self):
      obj = getattr(self, item)
      tmp = getattr(obj,MENUITEM_ATTR,None)
      if tmp is not None:
        #print(f"Process ITEM: {item} -> {tmp} ")
        self._add_menu_option(obj, **tmp)

      tmp = getattr(obj, MENUARG_ATTR, None)
      if tmp is not None:
        #print(f"Process ARG: {item} -> {tmp} ")
        self._add_menuitem_arg(obj, **tmp)
    self._initialized()

  def _initialized(self):
    pass
  
  def _create_menuentry_if_not_exists(self, key):
    if key not in self._menu:
      # print(f"Create {key=}")
      self._menu[key] = dict(handle=None, args=dict())  
  
  def _add_menu_option(self, what, name=None, args=None):
    self._create_menuentry_if_not_exists(name)
    self._menu[name]['handle'] = what
    
    while True:
      tmp = getattr(what, ORG_FUNCTION_HANDLE, None)
      if tmp is None:
        break
      what = tmp

    sig = inspect.signature(what)
    for param_name, param in sig.parameters.items():
      #print(f"{param=}")
      #if param.kind == inspect.Parameter.KEYWORD_ONLY:
      if param_name not in self._menu[name]['args']:
        self._menu[name]['args'][param_name] = None
    

    for arg_name, arg_vals in args.items():
      self._menu[name]['args'][arg_name] = arg_vals
    
  def _add_menuitem_arg(self, what, name=None, option=None):
    self._create_menuentry_if_not_exists(name)
    self._menu[name]['args'][option] = what
    print(self._menu[name]['args'])



  def _complete_menuitem(self, text):
    all_items = list(self._menu.keys())
    matching = list(filter(lambda x:x.startswith(text), all_items))  
    return matching

  
  def _complete_menuitem_args(self, parts):
    item = parts[0]
    item_conf = self._menu[item]
    cur_args = parts[1:-1]
    cur_arg_names = list(map(lambda x:x.split("=")[0], cur_args)) 
    last_arg = parts[-1]
    arg_names = item_conf['args'].keys()
    if len(arg_names)==0:
      return None
    #print(f"Hint for {parts=} {text=} => {item=} ... {cur_arg_names=}")
    available_args = []
    for e in arg_names:
      if e not in cur_arg_names:
        available_args.append( f"{e}=" )
    if "=" in last_arg:
      sug_arg, sug_val = last_arg.split("=",1)
      arg_opt = item_conf['args'][sug_arg]
      
      if arg_opt == None:
        return None
      
      if callable(arg_opt): # handle function
        #print(f"CALL {arg_opt}")
        sig = inspect.signature(arg_opt)
        #print(sig)
        if _func_num_args(arg_opt) >1:
          #print(" ARGS > 1", sug_val)
          values = arg_opt(self, sug_val)
        else:
          #print(" ARGS = 1 ")
          values = arg_opt(self)
      else:
        values = arg_opt
        
      #print(f"{values=}")
      values = map(str, values)
      matching = list(filter(lambda x:x.startswith(sug_val), values))
      return matching
    else:
      sug_arg = last_arg
      matching = list(filter(lambda x:x.startswith(sug_arg), available_args))
      return matching

    
  # this way you can do bash-completion
  def _get_suggestion_for_commandline(self, cmdline):
    parts = cmdline.split(" ")
    n_parts = len(parts)
    if n_parts == 0:
      return []
    if n_parts == 1 :  
      return self._complete_menuitem(parts[0])
    else :
      return self._complete_menuitem_args(parts)  
  
  def _complete_func(self, text, state):
    try:
      cur_line = readline.get_line_buffer()
      matching = self._get_suggestion_for_commandline(cur_line)
      return _list_item_or_none(matching, state)
    
    except Exception as ex:
      print(f"ERROR : {ex} in {_get_exception_info(ex)}")
      return None
        
  def _execute(self, cmdline, callback_func=None):
    try:
      if cmdline is None or cmdline == "" or len(cmdline) < 1:
        return print("<NO COMMAND>")
      parts = cmdline.split(" ")
      menuitem = self._menu.get( parts[0], None)
      if menuitem is None:
        raise NoSuchOptionException(f"Unknown item [{parts[0]}] (when executing: {cmdline} ) ")
      func = menuitem['handle']
      kwargs = {}
      for item in parts[1:]:
        if "=" in item:
          k,v = item.split("=",1)
          kwargs[k] = v
      
      ret_val = func(**kwargs)
      if callback_func and callable(callback_func):
        callback_func(ret_val)
  
    except Exception as ex:
      print(f"Fail to execute. Reason : [ {ex} ] {_get_exception_info(ex)}")
    
  def get_prompt(self):
    return self.prompt

  def _run(self, callback_func=None):
    self.KEEP_RUNNING = True
    while self.KEEP_RUNNING:
      cmd = input(self.get_prompt())
      self._execute(cmd, callback_func)
      
      
  @menuitem_option("exit")
  def _end(self):
    self.KEEP_RUNNING = False



if __name__ == '__main__':
  class CustomCommands(CmdMenu):

    @menuitem_option("challages.list")
    def do_chal_list(self):
      """ make foo """
      pass

    @menuitem_option("challages.show")
    def do_chal_show(self, chal=None):
      """ make foo """
      pass 
    
    #@menuitem_arg("vpn", "baz")
    def func_baz(self, val=None, val2=None):
      #print("ARGS : ", self, val, val2)
      #return []
      #print(f"baz call -> {val=}")
      return [ f"{val}{x}" for x in range(3) ]
      
    def foo_func(self):
      #print("foo call")
      return ["foo1","bar1"]
    
    @menuitem_option("vpn", arg1=[1,2,3,4,44,33], foo=foo_func, baz=func_baz)
    def do_vpn(self, arg1=None, foo=None, baz=None):
      print(f"Execute {arg1} {foo} {baz}")

    def dir_dir2(self, value:str):
      import glob
      return glob.glob(f"{value}*")
    
    @menuitem_option("ls", target=dir_dir2)
    def do_ls(self, target=None):
      print(f"LS {target=}")


  x = CustomCommands()
  x._run()