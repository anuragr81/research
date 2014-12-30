#!/usr/bin/python

import os
import sys
import getopt
from xml.sax.handler import ContentHandler
import xml.sax
import re
import locallog;
import traceback;

try : 
 from ctypes import *
except Exception: 
 print "Could not load ctypes. Please use Python 2.6+."
 sys.exit(1)
 
### CONFIG VARS #####################

DEFAULT_LIBNAME="proteusalerts_py.dll"

#####################################

logger= locallog.Logger("alert_proteus").logger;

def parseSubject(id,subject_string): 
  pattern=re.compile('(.*)\.(.*)');
  field_arr=[];
  res = re.search(pattern,subject_string);
  while res: 
    field_arr.append(res.group(2));
    next=res.group(1);
    res = re.search(pattern,res.group(1));
  field_arr.append(next)

  if len(field_arr)< 4 : 
    raise Exception("Insufficient Subject Info");
  else: 
    return Subject(id,field_arr[3],field_arr[2],field_arr[1],field_arr[0]);

  return None;


def search_gftimf_in_path(libname):
  path=os.environ['LD_LIBRARY_PATH'];
  pattern=re.compile('(.*)\:(.*)');
  res = re.search(pattern,path);
  next = None ;
  while res : 
    cur_dir=res.group(2);
    logger.debug("Dir="+cur_dir);
    dirs.append(cur_dir);
    next=cur_dir;
    res = re.search(pattern,res.group(1));

  if next is not None: 
    dirs.append(next)

  for cur_dir in dirs: 
    try : 
       for content in os.listdir(cur_dir): 
         if os.path.isfile(cur_dir+"/"+content) and content == libname : 
             return cur_dir+"/"+content
    except Exception: 
       logger.warn("Ignoring \""+ cur_dir+"\"");
  return None;
    
class ProteusCLibWrapper (object):
    def __init__(self,lib,environment,subject):
        self.environment=environment
        self.subject=subject
        self.lib = lib

    def test_send(self):
        print "testing... "
        buf=create_string_buffer("Test Message");
        type=create_string_buffer("STATUS");
        severity=c_int(1)
        self.obj = self.lib.send_alert(self.environment,self.subject,type,severity,buf)

    def send_alert(self,severity_int,message): 
        buf=create_string_buffer(message);
        type=create_string_buffer("STATUS");
        severity=c_int(severity_int)
        self.obj=self.lib.send_alert(self.environment,self.subject,type,severity,buf);

class ProteusInitConfigReader : 
    def __init__(self,file): 
      parser=xml.sax.make_parser()
      self.handler=xmlConfigFileHandler()
      parser.setContentHandler(self.handler)
      parser.parse(file)
      self.environment=self.handler.environment
      self.pathgfitmf=self.handler.pathgfitmf

class Environment (Structure): 
       _fields_ = [ ("id",c_char_p),("service",c_char_p),("network",c_char_p),("daemon",c_char_p) ];


class Subject(Structure): 
       _fields_ = [ ("id",c_char_p),("domain",c_char_p),("location",c_char_p),("group",c_char_p),("application",c_char_p) ];


class xmlConfigFileHandler (ContentHandler): 
     def __init__(self):
      self.listschema=[];
      self.environment=None;
      self.pathgfitmf=None

     def parseEnvironmentSettings(self,attrs):
            id=None
            service=None
            network=None
            daemon=None
            for a in attrs.getNames(): 
               if a == "id" : 
                     id=attrs.getValue(a)
               elif a == "service": 
                     service=attrs.getValue(a)
               elif a == "network": 
                     network=attrs.getValue(a)
               elif a =="daemon": 
                     daemon=attrs.getValue(a)
               else: 
                     print "Error: Unknown attribute for environment"  
                     return None

            if id == None or service==None or network ==None or daemon ==None: 
               print "Error: Missing environment attributes"
               return None
            else: 
               return Environment(id,service,network,daemon)
          
     def parseLibrary(self,attrs): 
            for a in attrs.getNames(): 
               if a == "path": 
                   return attrs.getValue(a) ;

     def startElement (self, name,attrs) :
        logger.debug("<< name=\""+ name + "\" size(attrs)=" +str(len(attrs)));
        if name == "environment" : 
            self.environment=self.parseEnvironmentSettings(attrs)
        elif name == "library": 
            self.pathgfitmf=self.parseLibrary(attrs)

     def endElement (self, name ) :
        logger.debug("name=\""+ name + "\" >>");
 
### MAIN @@@@@

class ProteusSender :
     def __init__(self, initFile, subjectstring ):
            # Check mandatory arguments 
            if  initFile == None : 
              print "Error: Please provide initFile with -i <file> "
              raise Exception(); 
           
            if subjectstring == None: 
              print "Error: Cannot send withouth subject string. Please specify subject as -s  domain.location.group.application ";
              raise Exception();
          
            config=ProteusInitConfigReader(initFile) 
            if config.environment== None : 
               print "Fatal Error. Please verify enviroment in the config file"
               raise Exception();
            
            subject = None
            subject = parseSubject("subj1",subjectstring);
            
            if subject == None : 
              print "Could not parse subject. Returning without sending message."
              raise Exception();
            
            libname=DEFAULT_LIBNAME; 
            
            if config.pathgfitmf == None : 
                libpath=search_gftimf_in_path(libname); 
            else : 
                libpath=config.pathgfitmf;
            
            if libpath == None: 
               print "Cannot find library \""+libname + "\". Please provide the library path in setup.xml or add it to LD_LIBRARY_PATH";
               raise Exception();
            else : 
               try : 
                 if os.path.isfile(libpath): 
                     print "Loading library \""+libpath+"\""
                     lib = cdll.LoadLibrary(libpath);
                 else : 
                     print "Loading library \""+libpath+"/"+libname+"\""
                     lib = cdll.LoadLibrary(libpath+"/"+libname);
               except Exception: 
                 print "Could not Load Library \""+libname +"("+libpath+")"+"\". Exiting.";
                 traceback.print_exc(file=sys.stdout)
                 raise Exception();
             
            # initialize the proteus system            
            self.proteus=ProteusCLibWrapper(lib,config.environment,subject)

   
     def sendAlert (self, priority , message )  : 
           if message == None: 
               print "Error: Cannot send without message. Please provide message to send with -m <message> "
               raise Exception();
           
           logger.debug("message=\""+ message+ "\" priority=\""+ str(priority) +"\"");
            # send the message 
           self.proteus.send_alert(int(priority),message);
