from optparse import OptionParser

import os,threading,string,time,struct
import sys,traceback,operator

import Multicast
from InfoServiceSocketProtocol import InfoServiceSocketProtocol
from SnapshotSocketProtocol import SnapshotSocketProtocol
from lq2_fields import *
from tsutils import Packet,InterpreterBuilder,MessageCache
import alert_proteus
import locallog


logger= locallog.Logger("lq2decode").logger;

cache=None

def run_callback(cache,alertwrapper) :
    print "callback ."

def scheduledCacheCheck(alertwrapper,tolerance): 
        global cache ; 
        logger.debug("tolerance="+str(tolerance)+" FilterSize="+str(len(cache.filter_data.keys())))
        try : 
            last_timestamp = -1
            #logger.debug("Current Cache"+cache.dump())
            for filtIndex in cache.filter_data.keys():
              last_timestamp = -1
              for packet in cache.filter_data[filtIndex] :
                   curfhTimeStamp = packet.value('fh_sent_time')
                   if curfhTimeStamp > last_timestamp : 
                      last_timestamp = curfhTimeStamp
    
              if last_timestamp != -1 : 
                  curTime = int(time.time())
                  if curTime - last_timestamp > tolerance : 
                     logger.info("Filter violated : " + filtIndex.dump())
                     alertwrapper.sendAlert(1,"Cache not updated for more than " + str(tolerance) + " seconds " );
                     logger.info( "curTime("+str(curTime) + ")- last_timestamp("+str(last_timestamp)+") > tolerance("+str(tolerance)+")")
                     logger.info(cache.dump());
                  else : 
                     logger.debug("Filter Checked: " + filtIndex.dump())
                     logger.debug( "curTime("+str(curTime) + ")- last_timestamp("+str(last_timestamp)+") <= tolerance("+str(tolerance)+")")
        except : 
            logger.error("Exception");
            traceback.print_exc(file=sys.stdout)
            alertwrapper.sendAlert(1,"Error retrieving critical LQ2 fields."); #TODO: Catch specific exception

        threading.Timer(5.0,scheduledCacheCheck,[alertwrapper,tolerance]).start() ; # TODO: Implement thread-safety

   
class AlertWrapper :
    def __init__(self,proteus):
        self.proteus=proteus;
    def sendAlert(self,priority,message): 
        if (self.proteus!=None) : 
            try : 
               self.proteus.sendAlert(priority,message);
            except : 
               traceback.print_exc(file=sys.stdout)
               sys.exit(1);

        logger.info("Alert sent : " + message ) ;
             
def hexdump(src, length=8): 
    result = [] 
    digits = 2
    hexa = ""
    text = ""
    for i in xrange(0, len(src), length): 
        s = src[i:i+length] 
       #hexa = b' '.join(["%0*X" % (digits, ord(x))  for x in s]) 
       #text = b''.join([x if 0x20 <= ord(x) < 0x7F else b'.'  for x in s]) 
       #result.append( b"%04X   %-*s   %s" % (i, length*(digits + 1), hexa, text) ) 
     #return b'\n'.join(result)
        for x in s:
            hexa += "%0*X " % (digits, ord(x))
        for x in s:
            if 0x20 <= ord(x) and ord(x) < 0x7F:
                text += x
            else:
                text += "."
        result.append( "%04X   %-*s   %s" % (i, length*(digits + 1), hexa, text) ) 
    return result

class CrossingTime:
    def __init__(self):
        self.ts_in = 0.0
        self.ts_out = 0.0
        self.ts_client = 0.0
        self.ts_market = 0.0
        self.crossing_time_fh = None
        self.crossing_time_client = None
        self.crossing_time_market = None
        
bookData = lq2_fields_map_by_id


flag_cpt_map = {}

    #############
def main():
    #############
    #  options  #
    #############
    parser = OptionParser()
    parser.add_option("-v", "--verbose",
                      action="store_true", dest="verbose_mode", default=False,
                      help="flag causes info to be displayed verbosely")
    parser.add_option("-c", "--ts_receipt",
                      action="store_true", dest="timestamp_receipt_mode", default=False,
                      help="timestamp receipt of a BUFFER")
    parser.add_option("-e", "--cachetolerance",
                      action="store", dest="cachetolerance", default=10,type="int",
                      help="maximum permissible value for last cache-packet arrived")
    parser.add_option("-x", "--ts_market_in",
                      action="store_true", dest="timestamp_market_in_mode", default=False,
                      help="display market/in timestamps diff")
    parser.add_option("-y", "--ts_in_out",
                      action="store_true", dest="timestamp_in_out_mode", default=False,
                      help="display in/out timestamps diff")
    parser.add_option("-z", "--ts_out_client",
                      action="store_true", dest="timestamp_out_client_mode", default=False,
                      help="display out/client timestamps diff")

    parser.add_option("-f", "--feed",
                      action="store", type="string", dest="feed_name", default=None,
                      metavar="NAME", help="specify the feed name - ex: EUREX.L2")
    parser.add_option("-p", "--products",
                      action="store", type="string", dest="product_name_list", default=None,
                      metavar="PRODUCT(s)", help="specify the product(s) name(s) - ex: \"FADX_DEC_2006, FSMI_DEC_2006\"")
    parser.add_option("-a", "--product_all",
                      action="store_true", dest="product_all", default=False,
                      help="all products")
    parser.add_option("-i", "--info_service_ip",
                      action="store", type="string", dest="info_service_ip", default=None,
                      metavar="0.0.0.0:0", help="specify the infoservice Ip and Port - ex: 10.252.49.70:12345")
    parser.add_option("-m", "--multicast_ip",
                      action="store", type="string", dest="multicast_ip", default=None,
                      metavar="0.0.0.0:0", help="specify multicast group Ip and Port - ex: 239.193.3.24:12345")
    parser.add_option("-d", "--dynamic_subscription",
                      action="store_true", dest="dynamic_subscription", default=False,
                      help="enable dynamic subscription")
    parser.add_option("-w", "--new_way_snapshot",
                      action="store_true", dest="new_way_snapshot", default=False,
                      help="set in new way snapshot mode")
    parser.add_option("-j", "--file_products",
                      action="store", type="string", dest="product_name_file", default=None,
                      help="to specify a file containing the products name list to subscribe")
    parser.add_option("-o", "--output_file",
                          action="store", type="string", dest="output_file", default=None,
                          help="to specify an output file")
    parser.add_option("-n", "--network_interface",
                      action="store", type="string", dest="network_interface", default=None,
                      metavar="0.0.0.0", help="specify network interface - ex: 192.168.10.12")
    parser.add_option("-t", "--monitor_spec_file",
                      action="store",type="string",dest="monitor_spec_file", default=None,
                      help="file with rules for sending alerts" )
    parser.add_option("-l", "--alertproteus",
                      action="store", type="string", dest="alert_proteus", default=None,
                      metavar="0.0.0.0", help="specify proteus alert XML file - ex: setup.xml" )

    (options, args) = parser.parse_args()

    if not (options.multicast_ip or options.feed_name or options.product_name_list or options.info_service_ip):
        parser.error("not enough parameters (option -h for help)")
    if options.feed_name and options.multicast_ip:
        parser.error("options '--feed' and '--multicast_ip' are mutually exclusive")
    if options.product_name_list and options.multicast_ip:
        parser.error("options '--product' and '--multicast_ip' are mutually exclusive")
    if options.product_all and options.multicast_ip:
        parser.error("options '--product_all' and '--multicast_ip' are mutually exclusive")
    if options.feed_name and not options.info_service_ip:
        parser.error("for option '--feed', option '--info_service_ip' is needed")
    if options.product_name_list and not (options.feed_name and options.info_service_ip):
        parser.error("for option '--product', options '--feed' and '--info_service_ip' are needed")
    if options.product_all and not (options.feed_name and options.info_service_ip):
        parser.error("for option '--product_all', options '--feed' and '--info_service_ip' are needed")
    if options.product_name_file and not (options.feed_name and options.info_service_ip):
        parser.error("for option '--fileProducts', options '--feed' and '--info_service_ip' are needed")
    if options.dynamic_subscription and not ((options.product_name_list or options.product_all) and options.feed_name and options.info_service_ip):
        if options.product_name_list:
            parser.error("for option '--dynamic_subscription', options '--product', '--feed' and '--info_service_ip' are needed")
        elif options.product_all:
            parser.error("for option '--dynamic_subscription', options '--product_all', '--feed' and '--info_service_ip' are needed")
    if options.monitor_spec_file == None : 
        parser.error ("monitor_spec_file must be defined.");


    ##############################################
    # get information from InfoService if needed #
    ##############################################
    info_service                    = None
    info_service_ip                = options.info_service_ip
    info_service_host                = None
    info_service_port                = None
    info_service_user_name        = 'admin'
    info_service_password        = 'pass'
    dynamic_subscription            = options.dynamic_subscription
    list_feed_names                        = options.feed_name.split(",")
    feed_id                            = None
    list_multicast_ips           = None
    network_interface                = options.network_interface
    snapshot                            = None
    snapshot_ip                        = None
    snapshot_host                    = None
    snapshot_port                    = None
    new_way_snapshot                = options.new_way_snapshot
    verbose_mode                    = options.verbose_mode
    timestamp_receipt_mode        = options.timestamp_receipt_mode
    timestamp_market_in_mode    = options.timestamp_market_in_mode
    timestamp_in_out_mode        = options.timestamp_in_out_mode
    timestamp_out_client_mode    = options.timestamp_out_client_mode
    product_all                        = options.product_all
    product_name_list                = None
    proteus_alert_file          = options.alert_proteus
    cachetolerance              = options.cachetolerance


    if options.output_file:
        output_file        = open(options.output_file, 'w')
    else:
        output_file        = sys.stdout

    try :
       if proteus_alert_file != None: 
          proteus= alert_proteus.ProteusSender(proteus_alert_file,"EFX.LON.LQ2.TRIGGER");
       else : 
          proteus=None
          logger.info("Proteus is Disabled");
    except : 
       proteus = None
       logger.error("Proteus Init Failed.");
       sys.exit(1);
    alertwrapper = AlertWrapper(proteus);

    if options.monitor_spec_file:
        iprBuilder= InterpreterBuilder("../lib/libparser.so");
        ipr=iprBuilder.getInterpreter(open(options.monitor_spec_file).read());
        global cache
        cache=MessageCache(ipr);
    else :
        global cache
        cache=None
        logger.info("Market Data Trigger would be disabled")

    try : #KeyboardInterrupt
        if options.product_name_list:
            product_name_list = options.product_name_list.split(',')
            logger.debug('product_name_list : ');
            logger.debug(product_name_list)

        if options.product_name_file:
            product_name_list=[]
            f=file(options.product_name_file,"r")
            liste_frf=f.readlines()
            for item in liste_frf:
                item = string.join(item.split('\n'), '')
                product_name_list.append(item.split(':')[0])
        
        if product_all:
            logger.debug("[all products]")
        else:
            logger.debug(product_name_list)

        logger.debug('feed_names=' + str(list_feed_names) + ' - info_service_ip=' + info_service_ip)

        ( FeedIdToName , bookList, product_id_map, list_multicast_ips )  = initializeMulticastReceivers(output_file,info_service_ip,info_service_user_name,info_service_password,list_feed_names,dynamic_subscription,product_name_list,product_all,verbose_mode, timestamp_receipt_mode, timestamp_in_out_mode, timestamp_market_in_mode, timestamp_out_client_mode,new_way_snapshot)

        if len(list_multicast_ips) <= 0 :
            raise RuntimeError, "the multicast group is not set"
        
        print "================OPTIONS=================";
        print options;
        print "========================================";
        ##############################################
        # scheculed Timed Checks 
        ##############################################
        if cache != None : 
             print cache.dump()
             scheduledCacheCheck(alertwrapper,cachetolerance)
        
        ##############################################
        # compute today_midnight                     #
        ##############################################
        tmp = time.localtime(time.time())
        tmp2 = (tmp.tm_year, tmp.tm_mon, tmp.tm_mday, 0, 0, 0, tmp.tm_wday, tmp.tm_yday, tmp.tm_isdst)
        today_midnight = time.mktime(tmp2) * 1000

        ##############################################
        # decode multicast market data flow          #
        ##############################################
        client = Multicast.MulticastSocket(list_multicast_ips, 5, network_interface)
        while 1:
            output_header = ''
            output_body = ''
            msgList = client.receive()
            for (data, addr, port) in msgList:
                (output_header, output_body, output_bookUpdate) = decodeMessage(data, product_id_map,cache,ipr,alertwrapper,bookList,FeedIdToName)
                if output_body != '':
                    output_file.write("%s %s" %(output_header, output_body))
                if (output_bookUpdate != '' ): 
                    output_file.write("%s" %(output_bookUpdate))

    except KeyboardInterrupt:
        print flag_cpt_map
        if info_service:
            info_service.close()
        if client:
            client.close()
        if snapshot:
            snapshot.close()
        if output_file:
            output_file.close()


#############################################################

def initializeMulticastReceivers(output_file,info_service_ip,info_service_user_name,info_service_password,list_feed_names,dynamic_subscription,product_name_list,product_all,verbose_mode, timestamp_receipt_mode, timestamp_in_out_mode, timestamp_market_in_mode, timestamp_out_client_mode,new_way_snapshot):
    
        FeedIdToName = {}
        product_id_map = None
        bookList = {}
        list_multicast_ips = []
        
        for feed_name in list_feed_names : 
          if info_service_ip:
            # INFO SERVICE CONNEXION
            try:
                (info_service_host, info_service_port) = info_service_ip.split(':')
                logger.debug('info_service_host=' + info_service_host + ' - info_service_port=' + info_service_port);
                info_service_port = int(info_service_port)
                logger.debug('info_service_port ='+ str(info_service_port))
            except:
                traceback.print_exc(file=sys.stdout)
                raise RuntimeError, "invalid adress for the InfoService"
            info_service = InfoServiceSocketProtocol()
            info_service.connect(info_service_host, info_service_port)
            log_result = info_service.login(info_service_user_name, info_service_password)
            if not log_result == "OK":
                raise RuntimeError, "login to the InfoService refused"
            # GET FEED ID
            
            logger.debug("Attempting to query info service for " + feed_name );    

            feed_id = info_service.getFeedIdFromName(feed_name)
            if not feed_id:
                raise RuntimeError, "can't retreive the feed id from the InfoService"
            # GET MULTICAST IP
            else:
                FeedIdToName[feed_id] = feed_name
                logger.info("Setup to Listen for FeedName="+str(feed_name)+", FeedId="+str(feed_id))

            multicast_ip = info_service.getMulticastGroup(feed_id)
            if not multicast_ip:
                raise RuntimeError, "can't retreive the multicast group from the InfoService"
            try:
                (multicast_host, multicast_port) = multicast_ip.split(':')
                multicast_port = int(multicast_port)
            except:
                raise RuntimeError, "can't decode the multicast group from the InfoService"
            list_multicast_ips.append([multicast_host, multicast_port]);
            # GET SNAPSHOT IP
            if dynamic_subscription:
                snapshot_ip = info_service.getSnapshotIp(feed_id)
                if not snapshot_ip:
                    raise RuntimeError, "can't retreive the snapshot ip from the InfoService"
                try:
                    (snapshot_host, snapshot_port) = snapshot_ip.split(':')
                    snapshot_port = int(snapshot_port)
                except:
                    raise RuntimeError, "can't decode the snapshot ip from the InfoService"
            # GET PRODUCTs IDs
            if product_name_list:
                product_id_map = {}
                for p_name in product_name_list:
                    p_id = info_service.getInstrumentId(feed_id, p_name)
                    if not p_id:
                        raise RuntimeError, "can't retreive the product id from the InfoService for product " + p_name
                    if not p_id in product_id_map:
                        product_id_map[p_id] = p_name
                    FeedIdToName[feed_id] = feed_name
                    bookList[bookIdKey(feed_id, p_id)] = {
                                'BID_1' :0.0,'ASK_1' :0.0, 'BID_SIZE_1' :0, 'ASK_SIZE_1' :0,
                                'BID_2' :0.0,'ASK_2' :0.0, 'BID_SIZE_2' :0, 'ASK_SIZE_2' :0,
                                'BID_3' :0.0,'ASK_3' :0.0, 'BID_SIZE_3' :0, 'ASK_SIZE_3' :0,
                                'BID_4' :0.0,'ASK_4' :0.0, 'BID_SIZE_4' :0, 'ASK_SIZE_4' :0,
                                'BID_5' :0.0,'ASK_5' :0.0, 'BID_SIZE_5' :0, 'ASK_SIZE_5' :0,
                                'BID_6' :0.0,'ASK_6' :0.0, 'BID_SIZE_6' :0, 'ASK_SIZE_6' :0,
                                'BID_7' :0.0,'ASK_7' :0.0, 'BID_SIZE_7' :0, 'ASK_SIZE_7' :0,
                                'BID_8' :0.0,'ASK_8' :0.0, 'BID_SIZE_8' :0, 'ASK_SIZE_8' :0,
                                'BID_9' :0.0,'ASK_9' :0.0, 'BID_SIZE_9' :0, 'ASK_SIZE_9' :0,
                                'BID_10':0.0,'ASK_10':0.0, 'BID_SIZE_10':0, 'ASK_SIZE_10':0,
                                'CLOSEPRICE':0.0, 'LAST_1':0.0, 'ACC_VOLUME':0}    

            #print ("product_id_map %s\n") %(product_id_map) 
            if product_all:
                product_id_map = info_service.getInstruments(feed_id)
                output_file.write("product_id_map %s\n" % product_id_map)
                #print "product_id_map %s\n"  %(product_id_map)
                if not product_id_map:
                    raise RuntimeError, "can't retreive the product id list from the InfoService "
            # DYNAMIC SUBSCRIPTION
            if dynamic_subscription and product_id_map and snapshot_port and snapshot_host:
                output_file.write("SNAPSHOT %s:%s\n" % (snapshot_host, snapshot_port))
                snapshot = SnapshotSocketProtocol(None, new_way_snapshot)
                snapshot.connect(snapshot_host, snapshot_port)
                for p_id in product_id_map:
                    #output_file.write("%s\n" % p_id)
                    try:
                        snapshot.sendRequest(feed_id, p_id)
                        data = snapshot.recvSnapshots()
                        if data == None:
                            logger.warn("NO SNAPSHOT")
                            continue
                        crossing_time = CrossingTime()
                        client_time = time.time() * 1000
                        (output_header, output_body,output_bookUpdate ) = decodeSnapshotMessage(data, verbose_mode, timestamp_receipt_mode, timestamp_in_out_mode, timestamp_market_in_mode, timestamp_out_client_mode, product_id_map, product_name_list, product_all, crossing_time, client_time,bookList,FeedIdToName)
                        if output_body != '':
                            output_file.write("%s %s" %(output_header, output_body))

                        output_file.write("%s" %(output_bookUpdate))

                    except RuntimeError:
                        raise RuntimeError, "can't subscribe to the Snapshot for product " + product_id_map[p_id]
            # CLOSE INFO SERVICE CONNEXION
            if info_service:
                info_service.close()
                info_service = None
            # CLOSE SNAPSHOT CONNEXION
            if snapshot:
                snapshot.close()
                snapshot = None
                
                
        return [ FeedIdToName , bookList, product_id_map, list_multicast_ips ]
        

def decodeMessage(data, product_id_map,cache,ipr,alertwrapper,bookList,FeedIdToName):
    pos = 0

    output_header = ''
    output_body = ''
    # DECODE FRAME
    (version, flags, nb_msg) = struct.unpack('!bbh', data[:struct.calcsize('!bbh')])

    pos += struct.calcsize('!bbh')
    output_header = "MESSAGE(S) INCOMING - version:%d flags:%d nb_msg:%d\n" % (version, flags, nb_msg)
    # DECODE MESSAGES
    bookUpdate=""
    for i in range(0, nb_msg):
        try:
            header_index = pos
            (nb_fields, sequence_id, instrument_id) = struct.unpack('!HII', data[pos:pos + struct.calcsize('!HII')]);

            pk=Packet();
            pk.version=version
            pk.packet_type=1 # 1  = update , 0 = snapshot
            pk.flags=flags
            pk.fillfields(nb_fields);
            pk.sequence_id=sequence_id;
            
            pos += struct.calcsize('!HII')
            feed_id = (instrument_id >> 24) & 0x000000FF
            instrument_id &= 0x00FFFFFF
            if not (instrument_id in product_id_map):
                continue
            output_body += "|-message index: %d\n" %i
            output_body += "|- nb_fields:%d sequence_id:%d feed_id:%d instrument_id:%d\n" % (nb_fields, sequence_id, feed_id, instrument_id)

            pk.instrument_id=instrument_id
            pk.feed_id=feed_id

            if (feed_id not in FeedIdToName):
                logger.error("feed_id: %d out of range" %feed_id)
                break
##            if product_name_list or product_all:
##                output_body += " product_name:%s\n" % (product_id_map[instrument_id],)
    ##        else:
##                output_body += "\n"
            # DECODE FIELDS
            item_cachable=False;

            for j in range(0, nb_fields):
                (field_id, field_flags, field_value) = struct.unpack('!BBI', data[pos:pos + struct.calcsize('!BBI')]);
                pos += struct.calcsize('!BBI')

                if field_id in bookData:
                    output_body += "   |- %s = %s\n" % format_field(bookIdKey(feed_id, instrument_id), field_id, field_flags, field_value,bookList)
                    pk.fielddict[bookData[field_id][0].lower()] = field_value;
                    pk.fieldflagdict[bookData[field_id][0].lower()] = field_flags;
                    item_cachable=True;
                else:
                    # identifying by fieldids (instead of field-names) not supported.
                    output_body += "%s:%s:%s\n" %(field_id, field_flags, field_value)

            if(cache!=None) : 
                if item_cachable == True: 
                    cache.addPacket(pk);
                    # No need to check if the item is not cachable
                    if ( ipr.doesRuleApplytoCache(cache) == True ) :
                        run_callback(cache,alertwrapper);
                        #alertwrapper.sendAlert(1,"Criteria met"); 
                        print cache.dump();
            else : 
                logger.error(" No Cache configured ") ;

              
            if (nb_fields != 0):
                bookUpdate =  bookSnapshot(feed_id, instrument_id, product_id_map[instrument_id],bookList,FeedIdToName)        
        except:
            logger.error("ERROR when parsing this message: %s\n %s" %(hexdump(data, len(data)), output_body))
            traceback.print_exc(file=sys.stdout)
            sys.exit(2)

    if (output_body != ""):
        output_body += "Read length %s: Buff length:%s\n" %(pos, len(data))
        if (pos != len(data)):
            output_body += "ERROR: Package size is not consistent with the nbupdate \n"
    return (output_header, output_body, bookUpdate)

def format_field(book_id, field_id, field_flags, field_value,bookList):
    """ Format field in function of its type.
        Get the field name """

    (field_name, field_type) = bookData[field_id]
    field_formated_value = 0
    if field_type == 'QUANTITY':
        field_formated_value = field_value
    elif field_type == 'PRICE':
        field_sign = 1
        if (field_flags & 0x10) >> 4 == 1:
            field_sign = -1
        field_factor = field_flags & 0x0F
        field_formated_value = field_value * pow(10, field_factor - 8) * field_sign
        if not flag_cpt_map.has_key(field_factor):
            flag_cpt_map[field_factor] = 0
        flag_cpt_map[field_factor] += 1
    elif field_type == 'TIME_T':
        field_formated_value = formatGmTimeTHHMMSSmmm(field_value)
    elif field_type == 'TIME':
        field_formated_value = formatGmtTimeHHMMSSmmm(field_value)
    elif field_type == 'TIME_DURATION':
        field_formated_value = field_value
    else:
        field_formated_value = field_value

    if book_id in bookList : 
      bookList[book_id][field_name] = field_formated_value
    return field_name, str(field_formated_value)




def formatGmtTimeHHMMSSmmm(timestamp):
    return time.strftime("%H:%M:%S", time.gmtime(timestamp / 1000)) + ".%03d" % (timestamp % 1000)
def formatGmTimeTHHMMSSmmm(timestamp):
    return time.strftime("%Y/%m/%d:%H:%M:%S", time.gmtime(timestamp))

def formatLocalTimeHHMMSSmmm(timestamp):

    return time.strftime("%H:%M:%S", time.localtime(timestamp / 1000)) + ".%03d" % (timestamp % 1000)
def formatGmTimeHHMMSSmmmuuu(timestamp):
    return time.strftime("%Y/%m/%d:%H:%M:%S", time.gmtime(timestamp / 1000)) + ".%03d" % (timestamp % 1000) + ",%03d" % ((timestamp*1000) % 1000)

def formatLocalTimeHHMMSSmmmuuu(timestamp):
    return time.strftime("%H:%M:%S", time.localtime(timestamp / 1000)) + ".%03d" % (timestamp % 1000) + ",%03d" % ((timestamp*1000) % 1000)

def format_field_3(field_id, field_flags, field_value,bookList):
    """ Format field in function of its type.
        Get the field name """
    if not field_id in lq2_fields_map_by_id:
        return str(field_id), str(field_value)
    (field_name, field_type) = lq2_fields_map_by_id[field_id]
    field_formated_value = 0
    if field_type == 'QUANTITY':
        field_formated_value = field_value
    elif field_type == 'PRICE':
        field_sign = 1
        if (field_flags & 0x10) >> 4 == 1:
            field_sign = -1
        field_factor = field_flags & 0x0F
        field_formated_value = field_value * pow(10, field_factor - 8) * field_sign
        if not flag_cpt_map.has_key(field_factor):
            flag_cpt_map[field_factor] = 0
        flag_cpt_map[field_factor] += 1
        logger.debug("flag = %d" % field_factor)
    elif field_type == 'TIME':
        field_formated_value = formatGmTimeHHMMSSmmm(field_value)
    elif field_type == 'TIME_DURATION':
        field_formated_value = field_value
    elif field_type == 'ENUM':
        field_formated_value = "'%s'" % chr(field_value)
    else:
        field_formated_value = field_value
    return field_name, str(field_formated_value)


def decodeSnapshotMessage(data, verbose_mode, timestamp_receipt_mode, timestamp_in_out_mode, timestamp_market_in_mode, timestamp_out_client_mode, product_id_map, product_name_list, product_all, crossing_time, client_time,bookList,FeedIdToName):
    pos = 0
    output_header = ''
    output_body = ''
    # DECODE FRAME
    (version, flags, nb_msg) = struct.unpack('!bbh', data[:struct.calcsize('!bbh')])
    pos += struct.calcsize('!bbh')
    output_header = "SNAPSHOT MESSAGE(S) INCOMING - version:%d flags:%d nb_msg:%d\n" % (version, flags, nb_msg)
    if timestamp_receipt_mode:
        output_header += "|- local_timestamp = %s\n" % formatLocalTimeHHMMSSmmmuuu(client_time)
    # DECODE MESSAGES
    bookUpdate = None
    for i in range(0, nb_msg):
        try :
            (nb_fields, sequence_id, instrument_id) = struct.unpack('!HII', data[pos:pos + struct.calcsize('!HII')]);
            pos += struct.calcsize('!HII')
            feed_id = (instrument_id >> 24) & 0x000000FF
            instrument_id &= 0x00FFFFFF
            if product_id_map and not (instrument_id in product_id_map):
                continue
            output_body += "|- nb_fields:%d sequence_id:%d feed_id:%d instrument_id:%d" % (nb_fields, sequence_id, feed_id, instrument_id)
            if product_name_list or product_all:
                output_body += " product_name:%s\n" % (product_id_map[instrument_id],)
            else:
                output_body += "\n"
        #    if verbose_mode == False and timestamp_in_out_mode == False and timestamp_market_in_mode == False and timestamp_out_client_mode == False:
                continue
            # DECODE FIELDS
            for j in range(0, nb_fields):
                (field_id, field_flags, field_value) = struct.unpack('!BBI', data[pos:pos + struct.calcsize('!BBI')]);
                pos += struct.calcsize('!BBI')
                if field_id in bookData:
                    output_body += "   |- %s = %s\n" % format_field(bookIdKey(feed_id, instrument_id), field_id, field_flags, field_value,bookList)
                if verbose_mode == True:
                    output_body += "   |- %s = %s\n" % format_field_3(field_id, field_flags, field_value,bookList)
                if timestamp_in_out_mode == True or timestamp_market_in_mode == True or timestamp_out_client_mode == True:
                    crossing_time = store_timestamp_in_out(field_id, field_value, crossing_time)
            bookUpdate =  bookSnapshot(feed_id, instrument_id, product_id_map[instrument_id],bookList,FeedIdToName)
                            
            # COMPUTE DIFF TIMESTAMPS
            crossing_time = compute_crossing_time(crossing_time)
            if timestamp_in_out_mode == True:
                if crossing_time.crossing_time_fh != None:
                    output_body += "   |- FH Crossing time (ms) = %f\n" % crossing_time.crossing_time_fh
                else:
                    output_body += "   |- FH Crossing time (ms) = None\n"
            if timestamp_out_client_mode == True:
                if crossing_time.crossing_time_client != None:
                    output_body += "   |- Client Crossing time (ms) = %f\n" % crossing_time.crossing_time_client
                else:
                    output_body += "   |- Client Crossing time (ms) = None\n"
            if timestamp_market_in_mode == True:
                if crossing_time.crossing_time_market != None:
                    output_body += "   |- Market Crossing time (ms) = %f\n" % crossing_time.crossing_time_market
                else:
                    output_body += "   |- Market Crossing time (ms) = None\n"
        except ValueError:
            logger.error("ERROR when parsing this message: %s\n Output body:%s Header:%s" %(hexdump(data, len(data)), output_body, output_header))
            sys.exit(2)

    return (output_header, output_body, bookUpdate)
    


def store_timestamp_in_out(field_id, field_value, crossing_time):
    if not field_id in lq2_fields_map_by_id:
        return crossing_time
    (field_name, field_type) = lq2_fields_map_by_id[field_id]
    if field_name == 'TIMESTAMP_1':
        crossing_time.ts_in += float(field_value)
    elif field_name == 'TIMESTAMP_2':
        crossing_time.ts_out += float(field_value)
    elif field_name == 'TIMESTAMP_4':
        crossing_time.ts_in += float(field_value) / 1000.0
    elif field_name == 'TIMESTAMP_5':
        crossing_time.ts_out += float(field_value) / 1000.0
    elif field_name == 'TIMESTAMP_6':
        crossing_time.ts_market += float(field_value)
    return crossing_time

def compute_crossing_time(crossing_time):
    if not crossing_time.crossing_time_fh:
        if crossing_time.ts_in != 0 and crossing_time.ts_out != 0 :
            crossing_time.crossing_time_fh = crossing_time.ts_out - crossing_time.ts_in
    if not crossing_time.crossing_time_client:
        if crossing_time.ts_out and crossing_time.ts_client:
            crossing_time.crossing_time_client = crossing_time.ts_client - crossing_time.ts_out
    if not crossing_time.crossing_time_market:
        if crossing_time.ts_in and crossing_time.ts_market:
            crossing_time.crossing_time_market = (crossing_time.ts_in - crossing_time.ts_market) % 3600000
    return crossing_time

def bookIdKey(feed_id, product_id):
    return "LQ2.%d.%d" %(feed_id, product_id)

def bookSnapshot( feed_id, instrument_id, instrument_name,bookList,FeedIdToName ) :
    bookUpdate=None
    if bookIdKey(feed_id, instrument_id) in bookList : 
        bookUpdate =  FeedIdToName[feed_id] + '.' + instrument_name + ':: ' +  str(bookList[bookIdKey(feed_id, instrument_id)]['LAST_1']) + '|' +\
                                                        str(bookList[bookIdKey(feed_id, instrument_id)]['CLOSEPRICE']) +\
                                                        '|' + str(bookList[bookIdKey(feed_id, instrument_id)]['ACC_VOLUME']) + '\n';
        for i in range(0,21):
            newline=False
            if ('BID_EXCH_' + str(i)) in bookList[bookIdKey(feed_id, instrument_id)]:
                bookUpdate += str(bookList[bookIdKey(feed_id, instrument_id)]['BID_EXCH_' + str(i)]).rjust(3, ' ') + ' | '
                newline=True
            if ('BID_SIZE_' + str(i)) in bookList[bookIdKey(feed_id, instrument_id)]:
                bookUpdate += str(bookList[bookIdKey(feed_id, instrument_id)]['BID_SIZE_' + str(i)]).rjust(8, ' ') + ' - ' + str(bookList[bookIdKey(feed_id, instrument_id)]['BID_' + str(i)]).ljust(8, '0') + ' | ' 
                newline=True
            if ('ASK_SIZE_' + str(i)) in bookList[bookIdKey(feed_id, instrument_id)]:
                bookUpdate += str(bookList[bookIdKey(feed_id, instrument_id)]['ASK_' + str(i)]).rjust(8, ' ') + ' - ' + str(bookList[bookIdKey(feed_id, instrument_id)]['ASK_SIZE_' + str(i)]).ljust(8, '0') + ' | ' 
                newline=True
            if ('ASK_EXCH_' + str(i)) in bookList[bookIdKey(feed_id, instrument_id)]:
                bookUpdate += str(bookList[bookIdKey(feed_id, instrument_id)]['ASK_EXCH_' + str(i)]).rjust(3, ' ') 
                newline=True
            if ( newline == True ) :
                bookUpdate += '\n'
    
    return bookUpdate


main()

