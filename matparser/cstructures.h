
#ifndef __CSTRUCTURES_H__
#define __CSTRUCTURES_H__

void initalizeExpression();

char* addVariable(char* name);

void printVariables();
void printMatrix(int i);
int verifyStructureName(char* name);
int verifyVariable(char* name);

char* structElement(char* buf, char* sname, char* field);

char* createInteger(char*buf, char*data);
char* createDouble(char*buf, char*data);

int createRow(char* scalar);
int addRowToMatrix(int matrix, int row);
int createMatrix(int row);
int addScalarToRow(int row, char* scalar);


char * createArgumentsList();
char * addToArgumentsList(char*args, char*arg);

char* callFunctionOrMatrix(char* name, char* args);
#endif