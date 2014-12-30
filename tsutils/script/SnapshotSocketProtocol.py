""" LQ2 Snapshot socket protocol """
__revision__ = "$Revision: 81100 $"

# RCS: $Id: SnapshotSocketProtocol.py 81100 2012-05-03 13:33:51Z kostas.kostiadis.437843 $
version = ("$RCSfile: SnapshotSocketProtocol.py,v $", "$Revision: 81100 $")
if __name__ == "__main__":
	print version

import socket
import re
import struct
from time import sleep

class SnapshotSocketProtocol:
	""" LQ2 Snapshot protocol """
	def __init__(self, sock=None, newWaySnapshot=False):
		self.newWaySnapshot = newWaySnapshot
		if sock is None:
			self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		else:
			self.sock = sock
	
	def connect(self, host, port):
		""" connect to the LQ2 snapshot """
		try:
			self.sock.connect((host, port))
		except:
			raise RuntimeError, "socket connection refused"
	def close(self):
		""" close the connection to the LQ2 snapshot """
		self.sock.shutdown(1)
		msg = ''
		while msg != '':
			msg = self.sock.recv(512)
		self.sock.close()
	
	def send(self, msg):
		""" send message to the LQ2 snapshot """
		totalSent = 0
		messageLength = len(msg)
		while totalSent < messageLength:
			sent = self.sock.send(msg[totalSent:])
			if sent == 0:
				raise RuntimeError, "socket connection broken"
			totalSent = totalSent + sent
	
	def receive(self, length):
		""" receive message from the LQ2 snapshot """
		msg = ''
		while not len(msg) == length:
			chunk = self.sock.recv(length - len(msg))
			if chunk == '':
				raise RuntimeError, "socket connection broken"
			msg = msg + chunk
		return msg
	
	def sendRequest(self, fid, pid):
		""" Send instrument request """
		#message = "\x00\x00\x00\x01\x02\x00\x00\x01"
		message = "\x00\x00\x00\x01"
		message += chr(fid)
		message += chr((pid & 0x00FF0000) >> 16)
		message += chr((pid & 0x0000FF00) >> 8)
		message += chr(pid & 0x000000FF)
		#message += "\x00\x00\x01"
		self.send(message)
	
	def sendRequests(self, fid, pidList):
		""" Send instrument requests """
		if len(pidList) <= 0xFF:
			message = "\x00\x00\x00"
			message += chr(len(pidList) & 0xFF)
			for pid in pidList:
				message += chr(fid)
				message += chr((pid & 0x00FF0000) >> 16)
				message += chr((pid & 0x0000FF00) >> 8)
				message += chr(pid & 0x000000FF)
			self.send(message)

	def recvSnapshots(self):
		""" Receive instrument snapshot """
		data = None
		self.sock.settimeout(0.01)
		try:
			data = self.receive(4)
		except:
			self.sock.settimeout(None)
			return data
		self.sock.settimeout(None)
		(snap_length, ) = struct.unpack('!I', data);
		if self.newWaySnapshot:
			data = self.receive(snap_length)
		else:
			data = self.receive(snap_length - 4)
		return data

