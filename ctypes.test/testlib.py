from ctypes import cdll,create_string_buffer,Structure,c_char_p,byref

try :
   lib = cdll.LoadLibrary("./libsender.so");
except:
   print "Could not load library"


class Message (Structure):
       _fields_ = [ ("input",c_char_p),("output",c_char_p) ];

inputstring="request"
ctypes_msg = Message(inputstring,"Init");
lib.sendData(byref(ctypes_msg));

print ctypes_msg.output
