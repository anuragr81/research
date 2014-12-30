__revision__ = "$Revision: 81100 $"

# RCS: $Id: Multicast.py 81100 2012-05-03 13:33:51Z kostas.kostiadis.437843 $
version = ("$RCSfile: Multicast.py,v $", "$Revision: 81100 $")
if __name__ == "__main__":
	print version

import socket, select

class MulticastSocket:
	"""MulticastSocket"""
	def __init__(self, addrs, ttl=255, interface='0.0.0.0'):
		"""Creates sockets and establishes multicast membership"""
		self.socks = []
		self.sock_defs = []
		if interface is None:
			interface='0.0.0.0'
		self.interface = interface
		print addrs
		for (address, port) in addrs:
			if address is None:
				continue
			sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
			self.sock_defs.append((sock, (address, port)))
			self.socks.append(sock)

			sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
			if hasattr(socket, "SO_REUSEPORT"):
				sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEPORT, 1)
			try:
				sock.setsockopt(socket.SOL_IP, socket.IP_MULTICAST_TTL, ttl)
				sock.setsockopt(socket.SOL_IP, socket.IP_MULTICAST_LOOP, 1)
			except:
				print "MulticastSocket:init - can't set socket options : 'IP_MULTICAST_TTL' and 'IP_MULTICAST_LOOP'"
			sock.bind(('', port))
			try:
				sock.setsockopt(socket.SOL_IP, socket.IP_MULTICAST_IF, socket.inet_aton(socket.gethostbyname(socket.gethostname())) + socket.inet_aton(self.interface))
			except:
				print "MulticastSocket:init - can't set socket options : 'IP_MULTICAST_IF'"
			sock.setsockopt(socket.SOL_IP, socket.IP_ADD_MEMBERSHIP, socket.inet_aton(socket.gethostbyname(address)) + socket.inet_aton(self.interface))

	def receive(self, length=4096):
		"""receive data from multicast group"""
		messages = []
		select_result = select.select(self.socks, [], [], 3600)
		for sock in select_result[0]:
			data, (addr, port) = sock.recvfrom(length)
			messages.append((data, addr, port))
		return messages

	def close(self):
		"""Ends multicast membership and closes socket"""
		for (sock, (address, port)) in self.sock_defs:
			sock.setsockopt(socket.SOL_IP, socket.IP_DROP_MEMBERSHIP, socket.inet_aton(socket.gethostbyname(address)) + socket.inet_aton(self.interface))
			sock.close()

