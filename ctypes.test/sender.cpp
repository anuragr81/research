#include "sender.h"
#include <string>
#include <string.h>
#include <iostream>

using namespace std;
void sendData(struct Message * ptr){
     std::string out="response";
     strncpy(ptr->output,out.c_str(),out.size());
}
