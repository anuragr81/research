fh = open('test1.txt',encoding="utf-8") ; x=fh.read() ; fh.close()
x[0].encode('utf-8').hex()

# Reading as fh = open(path,encoding='utf-8'); a = fh.read(); fh.close()
bytes(a.encode('utf-8')) # but this is for reading
bytes(a,'unicode_escape')          

# Writing
chr(0x09F0).encode('utf-8') # can be written straight to the file if 0x09F0 is the uncode to be written -it's the assamese extra letter


#Given a hex 
#import struct
#struct.pack(">H", 0xC484).decode("utf8")