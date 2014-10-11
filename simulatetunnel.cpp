#include <iostream>
#include <math.h>
#include "SimpleRNG.h"

float abs(float x){

    if (x>=0) {
        return x; 
    }
    else { 
        return -x; 
    }
}

/* No exceptions are thrown - for performance reasons */

using namespace std;

class Path{

    public:
        Path() :m_numBounces(0),m_factor(1),m_time(0) {
        }

//MODIFIERS:
        bool reset (float v, float alpha, float h ,float e){
            m_v=v;
            m_numBounces=0;
            m_alpha=alpha;
            m_h=h;
            m_e=e;
            m_factor=1;
            m_time=0;
            if (abs(m_alpha) <1e-6) { 
                m_alpha=1e-6; // avoids divide by zero
            }
            if (abs(m_e+1) < 1e-6) {
                m_e=(1e-6) -1; // avoids divide by zero
            }
            m_t0=m_h/(m_v*sin(m_alpha));
        }


        void bounce() {
            float curBounceTime = m_t0/m_factor;
            m_time += curBounceTime;
            m_factor *= (1-m_e/(1+m_e));
            m_numBounces++;
        }

//READERS:

        float curTime() const {
            return m_time;
        }

        int numBounces() const { 
            return m_numBounces;
        }

// utility functions not to be used in the critical path

        float x() const { 
            return v_x()*curTime();
        }

        // displacement before a bounce is that of the last reflection-point
        float y() const { 
            float y =0;
            for (unsigned int i = 1 ; i <= m_numBounces;++i){
                y+=((2*float(i%2)-1)*m_h);
            }
            return y;
        }

        float v_x() const { 
            return m_v*cos(m_alpha);
        }

        float v_y() const { 
            float v_y =m_v*sin(m_alpha);
            for (unsigned int i = 1 ; i <= m_numBounces;++i){
                v_y*=-(1-(m_e/(1+m_e)));
            }
            return v_y;
        }

        float v() const { 
            return sqrt(v_x()*v_x()+v_y()*v_y());
        }

    private:
// Attributes
        float m_v, m_alpha,m_h,m_e;
// Bounce-Tracking attributes
        float m_time,m_factor,m_t0;
        int m_numBounces;
};

void operator << ( std::ostream & os , Path const & p)  {
    cout << p.curTime() << "," << p.x()   << "," << p.y() << ","
        << p.v_x()     << ", "<< p.v_y() << "," << p.v() << endl;
}

void printHeader (std::ostream & os){
    os << "curTime,x,y,v_x,v_y,v"<<endl;
}

void calculate(float m,float v,float h, float L,float e){
    float alpha=m;
    Path p;
    p.reset(v,alpha,h,e);
    printHeader(cout);
    float totalTime= L/(v*cos(alpha));
    //cout << "cos(alpha)=" << cos(alpha) << " totaltime= " << totalTime << endl;
    while (p.curTime() < totalTime ){
        cout << p;
        p.bounce();
    }
    cout << p;
    cout << " num-bounces = " << p.numBounces() << endl;
}

int main(){
/*
    SimpleRNG r;
    cout << "values" << endl;
    for (int i = 0 ; i < 1e+6; ++i) {
       cout << r.GetExponential(1) << endl;
    }
*/

    float m=3.14159/4;
    float v=2;
    float h=4;
    float L=10;
    float e=0;
    calculate(m,v,h,L,e);;
    return 0;
}

