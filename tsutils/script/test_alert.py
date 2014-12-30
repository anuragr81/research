#!/usr/bin/python

import alert_proteus;
import traceback,sys;

setup_file="/home/fxdev/proteus_config/alert_config.xml"

try : 
   ps = alert_proteus.ProteusSender(setup_file, "EFX.LON.FORCE.TESTALERT")
except : 
   traceback.print_exc(file=sys.stdout)
