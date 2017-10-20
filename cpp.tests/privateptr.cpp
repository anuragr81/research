#include <iostream>

class A {
    public:
        A(): _a(0) {}
        int get_a() const { return _a ; } 
        void set_a(int x) { _a = x; } 
    private:
        int _a ; 
};

class B{
    public:
        B() {
            a = new A();
        }
        ~B(){ 
            delete a;
        } 
        A * get_A() // try adding a const to the function and see why the compiler doesn't permit it
        { 
            return a; 
        }

    private:
        A * a ; 
} ; 
int main(){
    B b ; 
    using namespace std;
    cout << "A::a = " << b.get_A()->get_a() << endl;
    b.get_A()->set_a(-1);
    cout << "A::a = " << b.get_A()->get_a() << endl;
    return 0;
}
