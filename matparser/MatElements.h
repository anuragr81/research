#ifndef __MAT_ELEMENTS_H__
#define __MAT_ELEMENTS_H__

#include
#include

#include
#include

extern "C"
{
char * addVariable(char* name);
void initalizeExpression();
void cleanup();
void printVariables();
char* arrayElementConstIndex(char* buf, char* name, int index);
char* arrayElementVarIndex(char* buf, char* name, char* index);
char* structElement(char* buf, char* sname, char* field);
char* createInteger(char*buf, char*data);
char* createDouble(char*buf, char*data);
}

class Variable
{
public:

Variable(std::string val) : _value(val)
{
}

std::string const & name() const
{
return _value;
}

private:
std::string _value;
};

int verifyStructure(char* name, char* field)
{
return 0;
}

int verifyVariable(char * name)
{
return 0;
}

#endif

