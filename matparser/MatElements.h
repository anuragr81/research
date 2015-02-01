#ifndef __MAT_ELEMENTS_H__
#define __MAT_ELEMENTS_H__

extern "C" {
    char * addVariable(char* name);
    void initalizeExpression();
    void cleanup();
    void printVariables();
    void printMatrix(int i);

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

class Row {
public:

    Row () {
        
    }
    
    Row ( const Row & row){
        _cols = row._cols;
    }
    Row(char * scalar) {
        _cols.push_back(std::string(scalar));
    }

    void addElement(std::string const & str) {
        _cols.push_back(str);
    }
    void print(std::ostream & os) const {
        std::string prefix = "";
        for (std::list<std::string>::const_iterator it = _cols.begin();
                it!=_cols.end();++it){
            os << prefix << *it ;
            prefix = ",";
        }
    }
private:
    std::list<std::string> _cols;
};

class Matrix {
public:

    Matrix (){
    }
    
    Matrix(Row const & row) {
        this->addRow(row);
    }

    void addRow(Row const & row) {
        _rows.push_back(row);
    }
    
    void print(std::ostream & os) const {
        os << "[";
        std::string prefix = "";
        for (std::list<Row>::const_iterator it = _rows.begin();
                it!=_rows.end();++it){
            os << prefix;
            it->print(os);
            prefix = ";";
        }
        os << "]";
    }
private:
    std::list<Row> _rows;
};

using namespace std;
class Allocator {
public:

    Allocator() : _mcount(0) {
    }

    char* createVariable(char* name) {
        // create new variable
        return name;
    }

    int createRow(char* scalar) {
        Row r(scalar);
        _rows[_rcount] = r;
        cout << "added " << scalar << "to newly created row="<< _rcount<<endl;
        return _rcount++;
    }

    int addScalarToRow(int row, char* scalar) {
        _rows[row].addElement(scalar);
        cout << "added " << scalar << "to row="<< row<<endl;
        return row;
    }

    int createMatrix(int row) {
        _matrices[_mcount] = _rows[row];
        return _mcount++;
    }

    int addRowToMatrix(int matrix, int row) {
        _matrices[matrix].addRow(_rows[row]);
        return matrix;
    }

    char* createArgumentsList() {
        return "args";
    }

    char* addToArgumentsList(char* args, char* arg) {
        return args;
    }

    char* callFunctionOrMatrix(char* name, char* args) {
        return "function";
    }

public:
    Matrix const & matrix (int i) const {
        return _matrices.at(i);
    }
    
private:
    std::list<Variable> _variables;
    std::map<int, Matrix> _matrices;
    std::map<int, Row> _rows;
    int _mcount, _rcount;

};

int verifyStructure(char* name, char* field) {
    return 0;
}

int verifyVariable(char * name) {
    return 0;
}

#endif

