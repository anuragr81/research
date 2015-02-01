#include  <iostream>
#include <string.h>
#include <sstream>
#include <string>
#include <list>
#include <map>

       
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

char* structElement(char* buf, char* sname, char* field) {
    verifyStructure(sname, field);
    std::stringstream ss;
    ss << "STRUCT{" << sname << "." << field << "}";
    strcpy(buf, ss.str().c_str());
    return buf;
}

char* createInteger(char* buf, char*data) {
    std::stringstream ss;
    ss << data;
    strcpy(buf, ss.str().c_str());
    return buf;
}

char* createDouble(char* buf, char*data) {
    std::stringstream ss;
    ss << data;
    strcpy(buf, ss.str().c_str());
    return buf;
}

int createRow(char* scalar){
    return _alloc.createRow(scalar);
}

int addRowToMatrix(int matrix, int row){
    return _alloc.addRowToMatrix(matrix,row);
}
int createMatrix(int row){
    return _alloc.createMatrix(row);
}

int addScalarToRow(int row, char* scalar) {
    return _alloc.addScalarToRow(row,scalar);
}

void printMatrix(int i){
    _alloc.matrix(i).print(cout);
}
char * createArgumentsList() {
    return _alloc.createArgumentsList();
}

char * addToArgumentsList(char*args, char*arg) {
    return _alloc.addToArgumentsList(args, arg);
}

char* callFunctionOrMatrix(char* name, char* args) {
    return _alloc.callFunctionOrMatrix(name, args);
}
