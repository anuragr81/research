#include
#include
#include "MatElements.h"

using namespace std;

static list _variables;
;

char * addVariable(char* name) {
_variables.push_back(Variable(name));
return name;
}

void initalizeExpression() {

}

void cleanup() {

}

void printVariables() {
for (list::const_iterator it = _variables.begin();
it != _variables.end(); ++it) {
cout <name() << " ";
}
cout << endl;
}

char* arrayElementConstIndex(char* buf, char* name, int index) {
verifyVariable(name);
std::stringstream ss;
ss << "ELEM{" << name << "[" << index << "]" << "}";
strcpy(buf, ss.str().c_str());
return buf;
}

char* arrayElementVarIndex(char* buf, char* name, char* index) {
verifyVariable(name);
std::stringstream ss;
ss << "ELEM{" << name << "[" << index << "]" << "}";
strcpy(buf, ss.str().c_str());
return buf;
}

char* structElement(char* buf, char* sname, char* field) {
verifyStructure(sname, field);
std::stringstream ss;
ss << "STRUCT{" << sname << "." << field << "}";
strcpy(buf, ss.str().c_str());
return buf;
}

char* createInteger(char* buf,char*data) {
std::stringstream ss;
ss << "CINT{" << data << "}";
strcpy(buf, ss.str().c_str());
return buf;
}

char* createDouble(char* buf,char*data) {
std::stringstream ss;
ss << "CDOUBLE{" << data << "}";
strcpy(buf, ss.str().c_str());
return buf;

}


