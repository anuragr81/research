#include  <iostream>
#include <string.h>
#include <sstream>
#include <string>
#include <list>

#include "MatElements.h"

using namespace std;

static Allocator _alloc;

//static list<Variable> _variables;

char * addVariable(char* name) {
    return _alloc.createVariable(name);
}

void initalizeExpression() {

}

void cleanup() {

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

char* createInteger(char* buf, char*data) {
    std::stringstream ss;
    ss << "CINT{" << data << "}";
    strcpy(buf, ss.str().c_str());
    return buf;
}

char* createDouble(char* buf, char*data) {
    std::stringstream ss;
    ss << "CDOUBLE{" << data << "}";
    strcpy(buf, ss.str().c_str());
    return buf;

}


char * createRow(){
    return _alloc.createRow();
}
char * addToRow(char* row,char* scalar){
    return _alloc.addToRow(row,scalar);
}


char * createMatrix(){
    return _alloc.createMatrix();
}
char * addToMatrix(char* mat,char* row){
    return _alloc.addRowToMatrix(mat,row);
}