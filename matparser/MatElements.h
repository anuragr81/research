#ifndef __MAT_ELEMENTS_H__
#define __MAT_ELEMENTS_H__

extern "C" {
    char * addVariable(char* name);
    void initalizeExpression();
    void cleanup();
    void printVariables();
    char* arrayElementConstIndex(char* buf, char* name, int index);
    char* arrayElementVarIndex(char* buf, char* name, char* index);
    char* structElement(char* buf, char* sname, char* field);
    char* createInteger(char*buf, char*data);
    char* createDouble(char*buf, char*data);
    
    char * createRow();
    char * addToRow(char* row, char* scalar);

    char * createMatrix();
    char * addToMatrix(char* row, char* scalar);
    
    char * createArgumentsList();
    char * addToArgumentsList(char*args,char*arg);
    
    char* callFunction(char* name,char* args);

}

class Variable {
public:

    //TODO: strip the input
    Variable(std::string val) : _value(val) {
    }

    std::string const & name() const {
        return _value;
    }

private:
    std::string _value;
};

class Allocator {
public:

    char* createVariable(char* name) {
        // create new variable
        return name;
    }

    /* should have mat arrays */
    char* createRow() {
        // create new matrix
        return "row";
    }

    char * addToRow(char * name, char* scalar) {
        // add
        return name;
    }
    
    char * createMatrix(){
        return "matrix";
    }
    
    char* addRowToMatrix(char* mat,char * row){
        return mat;
    }
    
    char* createArgumentsList(){
        return "args";
    }
    
    char* addToArgumentsList(char* args,char* arg){
        return args;
    }
    
    char* callFunction(char* name,char* args){
        return "function";    
    }
    
private:
    std::list<Variable> _variables;

};

int verifyStructure(char* name, char* field) {
    return 0;
}

int verifyVariable(char * name) {
    return 0;
}

#endif

