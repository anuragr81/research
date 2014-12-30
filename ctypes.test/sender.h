#ifndef __SENDER_H_
#define __SENDER_H_

extern "C" {
  struct Message {
    char *input,*output;
  };
  void sendData(Message*);
}


#endif
