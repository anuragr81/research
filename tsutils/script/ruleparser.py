#!/usr/bin/python

from xml.sax.handler import ContentHandler;
import xml.sax;
import xml.dom.minidom;

import locallog
import exceptions,traceback,sys,operator

logger= locallog.Logger("rulesparser").logger;

########################################################


class InvalidLValueException(Exception): 
   def __init__(self,name): 
      self.valuename=name
   def __str__(self): 
      return repr(self.name)

class ValueNotFoundException(Exception): 
   def __init__(self,valuename): 
      self.valuename=valuename
   def __str__(self): 
      return repr(self.valuename)

class IndexNotinCacheKey (Exception): 
   def __init__(self,filter_index): 
      self.filter_index=filter_index
   def __str__(self): 
      return repr(self.filter_index)

class DataforFilterNotinCache (Exception): 
   def __init__(self,data): 
      self.data= data 
   def __str__(self): 
      return repr(self.data)


class Interpreter: 
    def __init__(self,mcl): 
        self.mcl=mcl;
        self.mcl.reverse();

#   This Function is intended to absorb all exceptions and return True/False/None to the caller
    def doesRuleApplytoCache(self,cache) :
       try : 
         for cond in self.mcl :
           if cond.condop == 0 : 
              res = cond.eval(cache)
              logger.debug("res="+str(res)+ " res assigned cache.eval.");
           else : 
              if cond.condop == ord('&') : 
                 curRes = cond.eval(cache)
                 logger.debug("res("+str(res)+") and cache.eval("+str(curRes) +")= "+str(res and curRes)+".");
                 res = res and curRes
              else : 
                  if cond.condop == ord('|'): 
                     curRes = cond.eval(cache)
                     res = res or curRes
                     logger.debug("res("+str(res)+") or cache.eval("+str(curRes) +")= "+str(res or curRes)+".");
                  else : 
                     logger.error("Invalid Operation"); 
                     traceback.print_exc(file=sys.stdout)
                     return None;

         logger.debug("doesRuleApplytoCache: returns " + str (res) ) # None might be used in logical op
         return res;
       except IndexNotinCacheKey: 
         return None;
       except DataforFilterNotinCache : 
         return None;
       except ValueNotFoundException : 
         print "Consider adding criteria to filter";
         return None;
       except : 
         logger.error("Exception"); 
         traceback.print_exc(file=sys.stdout)
         return None;
         

    def getWatchFilters(self): 
       watchfilters=dict(); # should be a dict of max filters (or a set)
       for cond in self.mcl:
            for lvalue in cond.lvalueset.lvalues:
               if lvalue.filter in watchfilters: 
                 if watchfilters[lvalue.filter] < lvalue.filter.index : 
                    watchfilters[lvalue.filter]= lvalue.filter.index
               else : 
                 watchfilters[lvalue.filter]=lvalue.filter.index
                                          
       return watchfilters;
    
class XMLParser:
    def __init__(self,str):
      handler=MCLBuilderXMLHandler()
      xml.sax.parseString(str,handler)
      self.interpreter=Interpreter(handler.masterconditionlist)
      logger.debug(str);

def prettyXML(str):
         xmlStructure = xml.dom.minidom.parseString(str);
         return xmlStructure.toprettyxml();


########################################################

class MasterConditionBuilder :
   def __init__(self):
        self.masterconditionlist =[]
        self.condition=None
        self.lvaluetree = None
        self.filter=None
        self.filterparam=None

########################################################
#              Condition Structure                     # 
########################################################
#
#      Condition{
#       tree of LValue {
#          Filter {
#           array of FilterParam {  
#                                }
#          }
#        }
#      }
#
########################################################

class Condition: 
   def __init__(self):
      self.condop = None
      self.relop =  None
      self.lvalue= None
      self.rvalue = None

   def eval(self,cache): 
      if self.relop== ord('<') : 
         try : 
            lvalue = self.lvalue.eval(cache)
            if lvalue == None : 
                raise Exception("None evaluated");
            else : 
                 logger.debug("Evaluating lvalue("+str(lvalue)+") < ("+str(self.rvalue)+")")
                 return lvalue < self.rvalue
         except ValueNotFoundException :
            logger.info("ValueNotFound. Returning False.");
            return False;
      else : 
         if self.relop == ord('>') : 
            try: 
               lvalue  = self.lvalue.eval(cache)
               if lvalue == None : 
                   raise Exception("None evaluated");
               else : 
                   logger.debug("Evaluating : lvalue("+str(lvalue)+") > rvalue("+str(self.rvalue)+")")
                   return lvalue > self.rvalue
            except ValueNotFoundException:
               logger.info("ValueNotFound. Returning False.");
               return False;
         else : 
            if self.relop == ord('=') : 
              try : 
                lvalue = self.lvalue.eval(cache)
                if lvalue == None : 
                    raise Exception("None evaluated");
                else : 
                    logger.debug("Evaluating : lvalue("+str(lvalue)+") == rvalue("+str(self.rvalue)+")")
                    return lvalue == self.rvalue ; 
              except ValueNotFoundException:
                logger.info("ValueNotFound. Returning False.");
                return False;
            else : 
               raise Exception("Unknown operator")

   def dump(self): 
      output="\nCondition{ condition.logical_op=\""+(self.condop)+"\" condition.relation_op=\""+(self.relop) +"\" " 

      for lv in self.lvalueset.lvalues:
         output=output+ lv.dump();
      output=output+" condition.rvalue=\""+self.rvalue+"\""
      output=output+"}"
      return output;


class Node : 
   def __init__(self,op) : 
       if op == 0 : 
          self.op = 0 # 0 means terminal 
          self.data = None
       else : 
          self.op = op # 1 means non-terminal


def evalNodes(nodes,cache): 
         node = nodes.popleft() 
         if node.op == 0 : 
           return node.data.eval(cache) 
         if node.op == ord('+') : 
           return evalNodes(nodes,cache) + evalNodes(nodes,cache)
         if node.op == ord('-') : 
           return evalNodes(nodes,cache) - evalNodes(nodes,cache)
         if node.op == ord('*') : 
           return evalNodes(nodes,cache) * evalNodes(nodes,cache)
         if node.op == ord('/') : 
           return evalNodes(nodes,cache) / evalNodes(nodes,cache)


class LValueTree : 
   def __init__(self,op):
      # nodes is a deque of nodes arranged in a prefix notation node. Op-node has not data.
      self.nodes=deque()

   def addOp(self,op): 
      self.nodes.append(Node(op))

   def addFilterEvaluator(self,filter): 
      n = Node(0)
      n.data = FilterEvaluator(filter)
      self.nodes.append(n)

   def eval(self,cache): 
      if len(self.nodes) == 0 : 
        raise InvalidLValueException("LValueTree")
      else : 
        nodes = self.nodes
        return evalNodes(nodes)
 
class FilterEvaluator :
   def __init__(self):
     self.filter=None

   def eval(self,cache): 
   # Note that watchfilter has no role in evaluation 
          if self.filter not in cache.filter_data : 
             errString = "Data for <<filter"+ self.filter.watchdump() + " does not exist in "
             errString = errString + " <<cachekeys "+ str(map(operator.methodcaller('watchdump'),cache.filter_data.keys())) + "cachekeys>>"
             logger.error(errString);
             raise DataforFilterNotinCache(self.filter.watchdump())
          else : 
               matching_packet_list  = cache.filter_data[self.filter]
               if len(matching_packet_list)< self.filter.index : 
                 errString = "Index : " + str(self.filter.index) +  " does not exist in \n<<packet_list (type="+str(type(matching_packet_list))+")" ;
                 errString = errString + str(map(operator.methodcaller('dump'),matching_packet_list)) + "\npacket_list>>";
                 logger.error(errString)
                 raise IndexNotinCacheKey(self.filter.index)
               else : 
                 packet = matching_packet_list [self.filter.index]
                 res = packet.value(self.filter.field)
                 logger.debug("Obtained "+ self.filter.field + "=" + str(res) + " from packet:" + packet.dump());
                 return res;
         
   def dump(self):
     output="\nLValue { "
     output=output+self.filter.dump();
     output=output+"}"
     return output 

class Filter: # Could call it LValueData instead
   def __init__(self):
     self.field=None
     self.packet_type=None
     self.index=None
     self.filterparams=[]

   def __eq__ (self,other):
      if isinstance(other,Filter) :
        # index,field not part of filter comparison
        res = str(self.packet_type) == str(other.packet_type) \
                  and self.filterparams== other.filterparams
        if (res) : 
              #logger.debug(other.dump() +  " = " + self.dump() ) 
              return True;
        else : 
              #logger.debug(other.dump() +  " != " + self.dump() ) 
              return False;
      else:
        return NotImplemented;

   def __ne__(self,other):
      result = self.__eq__(other);
      if result is NotImplemented:
         return result;
      return not result;

   def __hash__(self): 
      hash_string = str(self.packet_type) + "{"
      # again, index,field are not part of filter comparison
      for wf in self.filterparams: 
          hash_string = hash_string + "("+str(wf.field)+","+str(wf.rvalue)+")"
      hash_string = hash_string + "}"
      #logger.debug("Hashing: "+hash_string + " to " + str(hash(hash_string)))
      return hash(hash_string)
      
   def fitsPacket(self,packet): 
      if str(packet.packet_type) == str(self.packet_type) : 
         for fp in self.filterparams : 
           if not fp.fitsPacket(packet) :
             return False;
         return True;
      else : 
         logger.debug("Packet type does NOT match");
         return False;

   def dump(self):
     output="\nLVD{ filter.field=\""+self.field + "\" " 
     output= output+ " filter.packet_type=\""+self.packet_type+"\""
     output= output+ " filter.index=\""+str(self.index)+"\""
     for wf in self.filterparams:
          output= output+ "\t"+wf.dump();
     output= output + "}"
     return output;

   def watchdump(self): 
     output= "\nLVD::Watch {  packet_type=\""+self.packet_type+"\""
     for wf in self.filterparams:
          output= output+ "  "+wf.dump();
     output= output + "}"
     return output;
     

class FilterParam:
   def __init__(self,field,rvalue): # hashable must be immutable
     self.field=field
     self.rvalue=rvalue

   def __eq__(self,other):
      if isinstance(other,FilterParam) :
        return str(self.field) == str(other.field) and str(self.rvalue) == str(other.rvalue) ;
      else:
        return NotImplemented;

   def __ne__(self,other):
      result = self.__eq__(other);
      if result is NotImplemented:
         return result;
      return not result;

   def fitsPacket(self,packet): 
      logger.debug("Comparing "+ str(packet.value(self.field)) + " with " + str(self.rvalue));
      return str(packet.value(self.field)) == str(self.rvalue)

   def dump(self): 
      output=" { filterparams.field=\""+self.field+"\""
      output= output+ " filterparams.rvalue=\""+str(self.rvalue) + "\"  }"
      return output;
   
###################   HANDLERS   ############################

     
def start_condition(name,attrs,mb):
      mb.condition=Condition();
      mb.condition.condop=int(attrs.getValue("logical_operation"));
      mb.condition.relop=int(attrs.getValue("relation_operation"));
      mb.condition.rvalue=num(attrs.getValue("rvalue").encode('ascii','ignore'));

def end_condition(name,mb):
      mb.masterconditionlist.append(mb.condition)

def start_lvalueset (name,attrs,mb): 
      mb.condition.lvaluetree = LValueTree()

def start_lvalue(name,attrs,mb):
      op = int(attrs.getValue("operation"))
      # non-terminal lvalues are stored as OpNodes
      if op != 0 : 
            mb.condition.lvaluetree.addOp(op)
         
def start_messagefilter(name,attrs,mb):
      mb.filter=Filter()
      mb.filter.field=attrs.getValue("field");
      mb.filter.packet_type=attrs.getValue("packet_type");
      mb.filter.index=int(attrs.getValue("index"));

def end_messagefilter(name,mb):
      # terminal lvalues are stored as non-OpNodes
      mb.condition.lvaluetree.addFilterEvaluator(mb.filter)

def start_watchfilter(name,attrs,mb):
      field=attrs.getValue("field")
      rvalue=num(attrs.getValue("rvalue").encode('ascii','ignore'))
      mb.filterparam=FilterParam(field,rvalue)

def end_watchfilter(name,mb):
      mb.filter.filterparams.append(mb.filterparam)

#############################################################

class MCLBuilderXMLHandler (ContentHandler):
    def __init__(self): 
       self.mb=MasterConditionBuilder();
       self.masterconditionlist=[]
       self.starttagoptions ={ "condition"  : start_condition,
                           "lvalueset"  : start_lvalueset,
                           "lvalue"  : start_lvalue,
                           "messagefilter"  : start_messagefilter,
                           "filterparam"  : start_watchfilter}
       self.endtagoptions = { "condition"  : end_condition,
                           "lvalue"  : end_lvalue,
                           "messagefilter"  : end_messagefilter,
                           "filterparam"  : end_watchfilter}
  
    def startElement(self,name,attrs): 
       if name in self.starttagoptions: 
         self.starttagoptions[name](name,attrs,self.mb);

    def endElement(self,name) :
       if name in self.endtagoptions:
         self.endtagoptions[name](name,self.mb);
       else : 
         if name == "conditions" : 
          self.masterconditionlist=self.mb.masterconditionlist
          self.mb=None

#############################################################

def num (s):
    try:
        return int(s)
    except exceptions.ValueError:
        return float(s)

