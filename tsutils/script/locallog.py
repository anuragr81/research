
import logging;

loglevel=logging.DEBUG

loglevelmap = { 'tsutils':logging.DEBUG,
                'rulesparser':logging.DEBUG,
                'lq2decode':logging.INFO} ;
                
class Logger : 
  def __init__(self,LOGGER_NAME):
      logging.basicConfig();
      self.logger = logging.getLogger(LOGGER_NAME);
      global loglevel
      if LOGGER_NAME in loglevelmap : 
          self.logger.setLevel(loglevelmap[LOGGER_NAME])
      else:
          self.logger.setLevel(loglevel)
