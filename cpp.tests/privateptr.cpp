#include <iostream>
#include <exception> 
#include <map>

using namespace std;

class A {
public:
    virtual int get_a() const = 0 ;
    virtual void set_a(int x)  = 0 ;
} ; 

class A1: public A{
    public:
        A1(): _a(0) {}
        virtual int get_a() const { cout << "A1::get_a" << endl; return _a ; } 
        virtual void set_a(int x) { _a = x; } 
    private:
        int _a ; 
};


class A2 : public A{
     public:
     A2 (int x): _n(x){}
     virtual int get_a() const { cout << "A2::get_a" << endl; return _n ; } 
     virtual void set_a(int x) { }
     private:
     int _n;
  
};

class B{
    public:
        explicit B() : _count(0) { 
        }
        void addA(A *a) {
            _la[_count++]=a;
        }

        A const & get_A(size_t i) const
        { 
            return *_la.at(i);
        }
        size_t size() const { return _count ; }

        ~B() { 
            for (size_t i = 0 ; i < _count ; ++i) {
                cout << "cleaning up item:" << i << endl;
                delete _la[i];
            }
        } 
    private:
        // disallow copying on the container
        B (const B &) ; 
        void operator = (B const &);
        std::map<int,A*> _la;
        size_t _count ; 

} ; 

int main(){
    B b ; 
    b.addA(new A1() );
    b.addA(new A2(100) );
    for (int  i = 0 ; i < b.size() ; ++i){
            cout <<  b.get_A(i).get_a() << endl;
    } 
    // b.get_A(0).set_a(-1);// won't compile
    return 0;
}
