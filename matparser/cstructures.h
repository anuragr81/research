
#ifndef __CSTRUCTURES_H__
#define __CSTRUCTURES_H__

void initalizeExpression();

char* addVariable(char* name);

void printVariables();

int verifyStructureName(char* name);
int verifyVariable(char* name);

char* structElement(char* buf, char* sname, char* field);

char* createInteger(char*buf, char*data);
char* createDouble(char*buf, char*data);

char * createRow();
char * addToRow(char* row,char* scalar);

char * createMatrix();
char * addToMatrix(char* row,char* scalar);

char * createArgumentsList();
char * addToArgumentsList(char*args,char*arg);

char* callFunctionOrMatrix(char* name,char* args);
#endif