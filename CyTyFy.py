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
  APIKEY = open(".ctfkey").read()
except:
  print("Fail to read apikey ! ")
  print("Put you api-key in .apikey file ")
  exit

BASE_URL = 'https://ctf.hackthebox.com/api/'

HTB_ZIP_PASSWORD = "hackthebox"
DEFAULT_PROMPT   = " HTB:CTF> "

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



def _name_to_id(s):
  return int(s.split("::")[-1])




class HtbCli(CmdMenu):

  _current_ctf = None
  _categories = []
  _cat_id2name = {}
  _chal_names = []
  _chal_by_id = {}

  workdir = "ctf_stuff"

  def set_option(self, name, value):
    self.settings[name] = value
    json.dump(self.settings, open(".settings","w"))

  def get_option(self, name):
    self.settings.get(name, None)


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
    
    self._categories = []
    rsp = self.api.make_get("public/challengeCategories")
    data = rsp.json()
    for item in data:
      self._categories.append(f"{item['name']}::{item['id']}")
      self._cat_id2name[item['id']] = item['name']
    print(self._categories)
    
  def _get_cats(self):
    #print("GET CATS")
    return self._categories

  def _get_tasks(self):
    return self._chal_names

    
  def _complete_path(self, value:str):
    import glob
    return glob.glob(f"{value}*")

  
  def get_prompt(self):
    name = self._current_ctf['name']
    return f"HTB:CTF:({name}):>"
  
  def play_ctf(self, id):
    data = self.api.make_get(f'ctf/{id}').json()
    self._current_ctf = data
    self._chal_names = []
    for chal in data['challenges']:
      n = chal['name'].replace(" ","_")
      self._chal_names.append(f"{n}::{chal['id']}")
      self._chal_by_id[chal['id']] = chal


  @menuitem_option("task.list", solved=[1,0], cat=_get_cats)
  def do_chal_list(self, solved=None, cat=None, grep=None, fmt=None):
    data = self._current_ctf['challenges']
    if solved is not None:
      solved = bool(int(solved))
      #print(solved)
      data = list( filter(lambda x:x['solved']==solved, data) )
    if cat is not None:
      cat_id = _name_to_id(cat)
      data = list( filter(lambda x:x['challenge_category_id']==cat_id, data) )
    if grep is not None:
      data = list( filter(lambda x:grep in x['name'], data) )
    
    print("Found :", len(data))
    for i in data:
      cat_name = self._cat_id2name[i['challenge_category_id']]
      if fmt is not None:
        print(fmt.format(**i))
      else:
        print(f"{i['id']:5}  | {i['name']} / {cat_name} / {i['difficulty']} / {i['points']}pts ")
   
      
  @menuitem_option("task.info", name=_get_tasks)
  def do_chal_info(self, name=None, id=None):
    if id is None:
      id = _name_to_id(name)
    

  @menuitem_option("task.download", name=_get_tasks, target=_complete_path)
  def do_chal_download(self, name:str=None, id:int=None, target:str=None, extract=1):          
    if id is None:
      id = _name_to_id(name)
    chal = self._chal_by_id[id]
    if target is None:
      print("No target dir, Using default path")

      cat_name = self._cat_id2name[ chal['challenge_category_id'] ]
      target = "{0}/{1}/{2}".format(
        self.workdir,
        cat_name.lower(),
        chal['name'].replace("/","_").replace("\\","_")
      )
    os.makedirs(target, exist_ok=1)

    rsp = self.api.make_get(f'challenge/download/{id}' ,stream=True)
    disp = rsp.headers['content-disposition']
    if "filename=" in disp:
      fname = re.findall("filename=(.+)", disp)[0]
    else:
      fname = "task.raw"
    
    if os.path.exists(fname):
      print('SKIP DOWNLOAD - FILE EXISTS')
    else:
      fpath = get_free_filename(target, fname)
      with open(fpath, 'wb') as f:
        shutil.copyfileobj(rsp.raw, f)


    if extract:
      print("   Extracting ... ")
      ext = fname.split(".")[-1].lower()
      if ext in [ "zip", "7z", "rar" ]:
        cmd = f"cd '{target}'; pwd; mkdir unpack; 7z -o./unpack/ -p{HTB_ZIP_PASSWORD} x {fname}"
        print(f" EXECUTE {cmd} ")
        p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
        output = p.communicate()
        print("STDOUT : ", output[0])
        print("STDERR : ", output[1])
      else:
        print(f"Unknown EXTENSION {ext}")


    return dict(name=name, filepath=fpath, filename=fname)
  
    
  @menuitem_option("task.start", name=_get_tasks)
  def do_chal_start(self, name=None):

    def _show_chal_addr(host, port):
      print(f" -> http://{host}:{port}/")
      print(f" -> nc {host} {port}")
      print(f" -> socat - TCP4:{host}:{port}")

    if id is None:
      id = _name_to_id(name)
    
    chal = self._chal_by_id[id]
    if not chal['hasDocker'] > 0:
      print("This TASK has no DOCKER !")
      return 

    rsp = self.api.make_post(
      "challenge/container/start",
      json = {
        'id' : id
      }
    )
    print(rsp.json())
    

  @need_params("flag", "difficulty")
  @menuitem_option("task.flag", name=_get_tasks)
  def do_chal_flag(self, name=None, id=None, flag=None, ):
    if id is None:
      id = _name_to_id(name)
    
    rsp = self.api.make_post(
      f"flag/own",
      json = {
        'challenge_id' : id,
        'flag' : flag,
      }
    )
    return rsp.json()

    
  @menuitem_option("set.workdir", target=_complete_path)
  @need_params("target")
  def set_the_workdir(self, target=None):
    if not os.path.isdir(target):
      return "TARGET is not a directory"
    self.workdir = target
    print(" OK ")

  

def _json_output_formatter(data):
  if data is not None:
    import json
    print( json.dumps(data, indent=1) )

  
def _main_():
  if len(sys.argv) != 2 :
    print(f"USAGE : {sys.argv[0]} [CTF-ID]")
    return
  API = BareApi(BASE_URL, APIKEY)
  obj = HtbCli()
  obj.prompt = DEFAULT_PROMPT
  obj.setup( API )
  obj.play_ctf( int( sys.argv[1] ) )
  obj._run( callback_func=_json_output_formatter )


if __name__ == '__main__':
  _main_()