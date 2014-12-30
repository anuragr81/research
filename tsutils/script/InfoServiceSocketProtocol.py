""" LQ2 Info service socket protocol """
__revision__ = "$Revision: 81100 $"

# RCS: $Id: InfoServiceSocketProtocol.py 81100 2012-05-03 13:33:51Z kostas.kostiadis.437843 $
version = ("$RCSfile: InfoServiceSocketProtocol.py,v $", "$Revision: 81100 $")
if __name__ == "__main__":
	print version

import socket
import re
import string

class InfoServiceSocketProtocol:
	""" LQ2 Info service socket protocol """
	def __init__(self, sock=None):
		if sock is None:
			self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		else:
			self.sock = sock
	
	def connect(self, host, port):
		""" connect to the LQ2 info service """
		try:
			self.sock.connect((host, port))
		except:
			raise RuntimeError, "socket connection refused"
	def close(self):
		""" close the connection to the LQ2 info service """
		self.sock.shutdown(1)
		msg = ''
		while msg != '':
			msg = self.sock.recv(512)
		self.sock.close()
	
	def send(self, msg):
		""" send message to the LQ2 info service """
		totalSent = 0
		messageLength = len(msg)
		while totalSent < messageLength:
			sent = self.sock.send(msg[totalSent:])
			if sent == 0:
				raise RuntimeError, "socket connection broken"
			totalSent = totalSent + sent
	
	def receive(self, length):
		""" receive message from the LQ2 info service """
		msg = ''
		while not len(msg) == length:
			chunk = self.sock.recv(length - len(msg))
			if chunk == '':
				raise RuntimeError, "socket connection broken"
			msg = msg + chunk
		return msg
	
	def login(self, userName, passWord):
		""" login to the LQ2 info service """
		self.emptyRecvBuffer()
		message = "LOGIN %s %s\r\n" % (userName, passWord)
		self.send(message)
		return self.receiveMsg()
	
	def getInstrumentId(self, feedId, instrument):
		""" Send 1 instrument to the LQ2 info service """
		self.emptyRecvBuffer()
		message = "INSTRUMENT %s %s\r\n" % (feedId, instrument)
		self.send(message)
		instrumentId = self.receiveMsg()
		if not instrumentId:
			return None
		return int(instrumentId)
	
	def getInstruments(self, feedId):
		""" Get a map of pair (instrument_id, instrument_name) from the LQ2 info service """
		self.emptyRecvBuffer()
		instrumentMap = {}
		message = "INSTRUMENTS %s\r\n" % (feedId, )
		self.send(message)
		msg = self.receiveMsg()
		while msg: 
			(instrumentName, instrumentId) = msg.strip(' ').split(' ')
			instrumentId = int(instrumentId)
			if instrumentId!=0 and not instrumentId in instrumentMap:
				instrumentMap[instrumentId] = instrumentName
			msg = self.receiveMsg()
		return instrumentMap
	
	def getMulticastGroup(self, feedId):
		""" retreive the multicast group ip and port from the LQ2 info service """
		self.emptyRecvBuffer()
		message = "UPDATES_SERVER %s\r\n" % (feedId, )
		self.send(message)
		multicastGroup = self.receiveMsg()
		if not (multicastGroup):
			return None
		multicastGroup = multicastGroup.strip(' ')
		multicastGroup = string.join(multicastGroup.split(' '), ':')
		return multicastGroup

	def getSnapshotIp(self, feedId):
		""" retreive the multicast group ip and port from the LQ2 info service """
		self.emptyRecvBuffer()
		message = "SNAPSHOT_SERVER %s\r\n" % (feedId, )
		self.send(message)
		snapshotIp = self.receiveMsg()
		if not (snapshotIp):
			return None
		snapshotIp = snapshotIp.strip(' ')
		snapshotIp = string.join(snapshotIp.split(' '), ':')
		return snapshotIp
	
	def getFeedIdFromName(self, feedName):
		""" retreive the feed id from the LQ2 info service """
		self.emptyRecvBuffer()
		message = "FEED %s\r\n" % (feedName, )
		self.send(message)
		feedId = self.receiveMsg()
		if not feedId:
			return None
		return int(feedId)
	
	def getFeeds(self):
		""" Get a map of pair (feed_id, feed_name) from the LQ2 info service """
		self.emptyRecvBuffer()
		feedMap = {}
		message = "FEEDS \r\n"
		self.send(message)
		msg = self.receiveMsg()
		while msg: 
			(feedName, feedId) = msg.strip(' ').split(' ')
			feedId = int(feedId)
			if feedId!=0 and not feedId in feedMap:
				feedMap[feedId] = feedName
			msg = self.receiveMsg()
		return feedMap
	
	def receiveMsg(self):
		""" receive message from the LQ2 info service """
		reg = re.compile(r"\r\n")
		msg = ''
		while not reg.search(msg):
			msg = msg + self.receive(1)
		if len(msg) > 2:
			return msg[:-2]
		else:
			return None
	
	def emptyRecvBuffer(self):
		""" empty socket recv buffer """
		msg = None
		self.sock.settimeout(0.1)
		try:
			while msg != '':
				msg = self.sock.recv(512)
		except:
			pass
		#finally:
		#	self.sock.settimeout(None)
		self.sock.settimeout(None)

