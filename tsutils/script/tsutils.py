import sys
import ruleparser
import locallog
import traceback

logger= locallog.Logger("tsutils").logger;

try :
 from ctypes import *
except Exception:
 print "Could not load ctypes. Please use Python 2.6+."
 sys.exit(1)

class MessageCache:
    def __init__(self,interpreter):
         self.filter_data= dict()
         self.dict_watchfiltermax =interpreter.getWatchFilters();
         for filt in self.dict_watchfiltermax.keys() :
             self.filter_data[filt]=[]

    def addPacket(self,packet):
        try : 
         for filtIndex  in self.filter_data.keys():
           if filtIndex.fitsPacket(packet) :
               self.filter_data[filtIndex].append(packet)
               if len(self.filter_data[filtIndex]) > (self.dict_watchfiltermax[filtIndex]+1) :
                  self.filter_data[filtIndex].pop(0)
           #else : 
               #print "Packet doesn't fit"
         logger.debug("added packet"+ packet.dump());
        except :
            logger.error("General Exception"); 
            traceback.print_exc(file=sys.stdout)

    def dump(self): 
        output = "\n<cache>";
        for filtIndex in self.filter_data.keys(): 
          output = output + "\n\t<key>"
          output=output+"\t"+filtIndex.dump();
          output = output + "\n\t</key>\n\t<value>\n"; # format : { (key,[valuearray]) ... } 
          for entry in self.filter_data[filtIndex] : 
               output = output + entry.dump(); 
          output = output + "\n\t</value>";
        output = output + "\n\t</cache>";
        return output;

        
class Packet:

  def __init__(self):
      self.version=None
      self.packet_type=None # redundant; TODO: reconsider
      self.flags=None
      self.sequence_id=None
      self.instrument_id=None
      self.feed_id=None
      self.fielddict=dict()
      self.fieldflagdict=dict()
      self.fieldmap = { 'version': self.get_version,
                        'flags'  : self.get_flags,
                        'sequenceid' : self.get_sequence_id,
                        'instrumentid' : self.get_instrument_id,
                        'feedid' : self.get_feed_id } 

  def get_version(self):
      return self.version
  def get_flags(self):
      return self.flags
  def get_sequence_id(self):
      return self.sequence_id
  def get_instrument_id(self):
      return self.instrument_id
  def get_feed_id(self):
      return self.feed_id


  def value(self, fname) :
     if fname not in self.fieldmap: 
          if fname in self.fielddict : 
               return self.fielddict[fname];
          else : 
               logger.error("Field:"+fname + " not found in <<packet"+ self.dump() + ">>packet");
               raise ruleparser.ValueNotFoundException(fname)
     else : 
          return self.fieldmap[fname]();

  def dump(self):
      strout="\npacket { version=\""+str(self.version)+"\" packet_type=\"" \
                      + str(self.packet_type)+"\" flags=\""+str(self.flags)+"\" seqid=\"" \
                      + str(self.sequence_id)+"\" instrumentid=\""+str(self.instrument_id )\
                      + "\" feedid=\""+str(self.feed_id)+"\"";
      for i in self.fielddict:
          if self.fielddict[i] != None :
            strout = strout +  str(i) + "=\""+str(self.fielddict[i])+"\" ";
      strout = strout + "}"
      return strout

  def fillfields(self,nbFields):
      for i in range(0,nbFields):
         self.fielddict[i]=None


#### BEGIN CTYPES_DECL

class Message (Structure):
       _fields_ = [ ("input",c_char_p),("output",c_char_p) ];

#### END CTYPES_DECL
#libname="bin/libparser.so"

class InterpreterBuilder : 
   def __init__(self,libname) : 
     try :
       self.lib=cdll.LoadLibrary(libname);
     except Exception:
        print "Could not load library:"+libname;
        sys.exit(1);

   def getInterpreter(self,str) : 
       ctypes_msg = Message(str,"Init");
       self.lib.yacc_parse(byref(ctypes_msg));
       xml2parse=ctypes_msg.output
       parser=ruleparser.XMLParser(xml2parse)
       return parser.interpreter
