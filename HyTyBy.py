import shutil
import subprocess
import requests
import urllib.parse
import sys 
import os 
import re 

import json


from menuitems import *

APIKEY = ""
try:
  APIKEY = open(".apikey").read()
except:
  print("Fail to read apikey ! ")
  print("Put you api-key in .apikey file ")
  exit

BASE_URL = 'https://www.hackthebox.com/api/v4/'

HTB_ZIP_PASSWORD = "hackthebox"
DEFAULT_PROMPT   = " HTB:> "

AVAILABLE_DIF = list(map(str,[1,2,3,4,5,6,7,8,9,10]))



class BareApi:
  baseurl = ''
  headers = None
  requests_kwargs = {}

  
  def __init__(self, baseurl, apikey, ):
    self.baseurl = baseurl
    self.requests_kwargs = {}
    self.headers = {}
    self.headers['Authorization'] = f"Bearer {apikey}"
    self.headers['User-Agent'] = "Python HTP-CLI"
  
  def _enable_debug(self):
    BURP = "http://127.0.0.1:8080"
    self.requests_kwargs['proxies'] = dict(
      http = BURP,
      https = BURP,
    )
    self.requests_kwargs['verify'] = False
  
  def make_url(self, ep):
    url = urllib.parse.urljoin(self.baseurl, ep)
    # print(f"URL: {url}")
    return url 
    
  def make_get(self, ep, **kw):
    return requests.get(
      url = self.make_url(ep),
      headers = self.headers,
      allow_redirects=False,
      **self.requests_kwargs ,   
      **kw,
    )

  def make_post(self, ep, **kw): 
    ''' pass data= or json= ... '''
    return requests.post(
      url = self.make_url(ep),
      allow_redirects=False,
      headers = self.headers,
      **self.requests_kwargs,
      **kw,
    )


def get_free_filename(workdir, fname):
  n = 0
  new_fname = fname
  while True:
    fpath = f"{workdir}/{new_fname}"
    if not os.path.exists(fpath):
      return fpath
    new_fname = f"{n}__{fname}"
    n += 1


def _exctract_fields(record, *fieldlist):
  result = {}
  for field in fieldlist:
    result[field] = record[field]
  return result

def _extract_fields_from_list(collection, *fieldlist):
  return [ _exctract_fields(row, *fieldlist) for row in collection]


def _cacheable(key):
  
  def _wrap(org_func):
    #print("Wrapping", org_func)
    def _new_func(self):
      info = self._cache.get(key, None)
      if info is not None:
        # TODO cache timeout
        if 'data' in info:
          #print(f"Served from cache {key=}")
          return info['data']
      value = org_func(self)
      #print(f"Cache store {key=}")
      self._cache[key] = dict(
        data = value
      )
      return value
    
    return _new_func

  return _wrap






class HtbCli(CmdMenu):
  _cache = {}
  _cur_chal = None
  _cur_machine = None
  _target_dir = './HackTheBox/'

  def _id_from_name_or_current_chal(self, name):
    if name is not None:
      self._get_chal_list() # make sure chal list is there
      id = int( self._chal_name2id[name] )
      return id
    if self._cur_chal is not None:
      print(" < using current chall name > ")
      return self._cur_chal['id']
    raise Exception("Need challange NAME !")
    
  def setup(self, api):
    self.api = api
    
  def _complete_path(self, value:str):
    import glob
    return glob.glob(f"{value}*")

  @_cacheable("chal/cat/list")
  def _get_chal_category_list(self):
    data = self.api.make_get('challenge/categories/list').json()
    result = _extract_fields_from_list(data['info'], 'id', 'name')
    self._chal_cat_names = []
    self._chal_cat_ids   = []
    self._chal_cat_name2id = {}
    for item in result:
      self._chal_cat_names.append( f"{item['name']}::{item['id']}")
      self._chal_cat_ids.append( str(item['id']) )
      self._chal_cat_name2id[ item['name'] ] = item['id']
    return result
  
  @_cacheable("chal/list")
  def _get_chal_list(self):
    data = self.api.make_get('challenge/list').json()
    result = _extract_fields_from_list(data['challenges'], 'id', 'url_name','difficulty','points', 'isCompleted','challenge_category_id')
    self._chal_names = []
    self._chal_ids   = []
    self._chal_name2id = {}
    for item in result:
      self._chal_names.append( item['url_name'] )
      self._chal_ids.append( str(item['id']) )
      self._chal_name2id[ item['url_name'] ] = item['id']
    return result
  
  @_cacheable("chal/list_names")
  def get_chal_names(self):
    self._get_chal_list()
    return self._chal_names
  
  def get_category_names(self):
    self._get_chal_category_list()
    return self._chal_cat_names
  
  def _id_from_name(self, name):
    return int( name.split("::")[1] )
  
  
  
  
  
  
  
  
  
  
  @menuitem_option("challenge.category.list") 
  def do_chal_cat_list(self):
    return self._get_chal_category_list()
    
  @menuitem_option("challenge.list", solved=[1,0], cat=get_category_names)
  def do_chal_list(self, solved=None, cat=None, grep=None):
    data = self._get_chal_list()
    #print(data)
    if solved is not None:
      solved = bool(int(solved))
      #print(solved)
      data = list( filter(lambda x:x['isCompleted']==solved, data) )
    if cat is not None:
      cat = self._id_from_name(cat)
      data = list( filter(lambda x:x['challenge_category_id']==cat, data) )
    if grep is not None:
      data = list( filter(lambda x:grep in x['url_name'], data) )
    return data
      
  @menuitem_option("challenge.download", name=get_chal_names, target=_complete_path)
  def do_chal_download(self, name:str=None, id:int=None, target:str=None):
    if target is None:
      return "Please provide TARGET"
    
    if id is None:
      id = self._id_from_name_or_current_chal(name)

    rsp = self.api.make_get(f'challenge/download/{id}' ,stream=True)
    disp = rsp.headers['content-disposition']
    if "filename=" in disp:
      fname = re.findall("filename=(.+)", disp)[0]
    else:
      fname = "task.raw"
    
    fpath = get_free_filename(target, fname)
    with open(fpath, 'wb') as f:
      shutil.copyfileobj(rsp.raw, f)
    return dict(name=name, filepath=fpath, filename=fname)
  
  @menuitem_option("challenge.start", name=get_chal_names)
  def do_chal_start(self, name=None):
    id = self._id_from_name_or_current_chal(name)
    
    info = self.do_get_chal_info(id=id)
    if not info['docker']:
      print("There is no service to launch !")
      return 
    if info['docker_ip'] is not None and "." in info['docker_ip']:
      print(f"Instance already UP ! {info['docker_ip']} {info['docker_port']}")
      return
    rsp = self.api.make_post(
      f"challenge/start",
      json = {
        'challenge_id' : info['id']
      }
    )
    return rsp.json()
    
    
  @menuitem_option("challenge.info", name=get_chal_names)
  def do_get_chal_info(self, name=None, id=None):
    if id is None:
      id = self._id_from_name_or_current_chal(name)

    rsp = self.api.make_get(f"challenge/info/{id}")
    return rsp.json()['challenge']

  @need_params("flag", "difficulty")
  @menuitem_option("challenge.flag", name=get_chal_names, difficulty=AVAILABLE_DIF)
  def do_chal_flag(self, name=None, id=None, flag=None, difficulty=None):
    if id is None:
      id = self._id_from_name_or_current_chal(name)
    if flag is None:
      raise Exception(" >> GIB FLAG WTF !?")
    if difficulty is None:
      raise Exception(" >> Gib dificulty plz !")
  
    rsp = self.api.make_post(
      f"challenge/own", 
      json = {
        'challenge_id' : id,
        'flag' : flag,
        'difficulty' : int(difficulty) * 10,
      }
    )
    return rsp.json()



  @menuitem_option("challenge.workOn", name=get_chal_names)
  @need_params("name")
  def work_on_chal(self, name=None):
    if name is None:
      return "Please provide NAME"
    
    self._get_chal_list() # make sure chal list is there
    id = int( self._chal_name2id[name] )
    data = self.api.make_get(f"challenge/info/{id}").json()['challenge']
    print(f" 📎 {data['name']} / {data['category_name']} ")
    print(f" 📐 {data['difficulty']} / {data['points']} pts ")
    print(f"    Solves : {data['solves']} ✅ ")
    print(f"    Start  : {data['stars']}⭐ ")
  
    wd = f"{self._target_dir}/{data['category_name'].lower()}/{data['name']}/"
    print(f"   📁 {wd}")
    os.makedirs(wd, exist_ok=True)
    os.makedirs(f"{wd}/code", exist_ok=True)
  
    for fname in ['NOTES.md','flag.txt']:
      fpath = f"{wd}/{fname}"
      if not os.path.exists(fpath):
        open(f"{wd}/{fname}",'w').write('')
        print(f" ⇒  🗎 {fname}")
    
    if data['download']:
      asset_dir = f"{wd}/assets"
      if not os.path.exists(asset_dir):
        os.makedirs(asset_dir, exist_ok=True)
        print(f" ⇒ 📁 {asset_dir}")
      mark_downloaded = f"{asset_dir}/_downloaded"
      if not os.path.exists(mark_downloaded):
        meta = self.do_chal_download(name=name, target=asset_dir)
        print(f"  ⇩⇩ Downloaded : {meta['filename']}")
        json.dump(meta, open(mark_downloaded,"w") )
      else:
        meta = json.load( open(mark_downloaded, "r") )
        
      mark_extracted = f"{asset_dir}/_extracted"
      if not os.path.exists(mark_extracted):
        print("   Extracting ... ")
        fname = meta['filename']
        ext = fname.split(".")[-1].lower()
        if ext in [ "zip", "7z", "rar" ]:
          cmd = f"cd {asset_dir}; pwd; 7z -o../unpack/ -p{HTB_ZIP_PASSWORD} x {fname}"
          print(f" EXECUTE {cmd} ")
          p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
          output = p.communicate()
          open(mark_extracted,"wb").write(b'\n\n'.join(output))
          print(output[0].decode())
        else:
          open(mark_extracted,"w").write("Cant extract")  
    
    self._cur_chal = dict(
      name = data['name'],
      id = data['id'],
      workdir = wd,
    )
    self.prompt = f"(chal:{data['name']}) {DEFAULT_PROMPT}"
    return self._cur_chal
  

    
    
    
  @menuitem_option("set.workdir", target=_complete_path)
  @need_params("target")
  def set_the_workdir(self, target=None):
    if not os.path.isdir(target):
      return "TARGET is not a directory"
    self._target_dir = target
    return ["OK", self._target_dir]
  

  
  

def _json_output_formatter(data):
  if data is not None:
    import json
    print( json.dumps(data, indent=1) )

def _yaml_output_formatter(data):
  if data is not None:
    import yaml
    print(yaml.dump(data))
  
def _main_():
  API = BareApi(BASE_URL, APIKEY)
  output_formatter = _json_output_formatter
  if '-y' in sys.argv[1:]:
    output_formatter = _yaml_output_formatter
    del sys.argv[1]

  #API._enable_debug()

  obj = HtbCli()
  obj.prompt = DEFAULT_PROMPT
  obj.setup( API )
  obj._run( callback_func=output_formatter )


if __name__ == '__main__':
  _main_()