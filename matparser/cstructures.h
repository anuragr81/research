
#ifndef __CSTRUCTURES_H__
#define __CSTRUCTURES_H__

void initalizeExpression();

char* addVariable(char* name);

void printVariables();

int verifyStructureName(char* name);
int verifyVariable(char* name);

char* arrayElementConstIndex(char* buf, char* name, int index);
char* arrayElementVarIndex(char* buf, char* name, char* index);

char* structElement(char* buf, char* sname, char* field);

char* createInteger(char*buf, char*data);
char* createDouble(char*buf, char*data);

char * createRow();
char * addToRow(char* row,char* scalar);

char * createMatrix();
char * addToMatrix(char* row,char* scalar);

char * createArgumentsList();
char * addToArgumentsList(char*args,char*arg);

char* callFunction(char* name,char* args);
#endif