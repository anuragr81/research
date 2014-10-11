#include <iostream>
#include <math.h>
#include "SimpleRNG.h"

const float tol = 1e-6;

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
        Path(float v, float h,float L) :m_v(v),
        m_h(h),
        m_L(L),
        m_numBounces(0),
        m_factor(1),
        m_time(0), 
        m_alpha(0),
        m_t0(0),
        m_e(0),
        m_bounceUnit(0) {
        }

        //MODIFIERS:
        bool reset (float alpha, float e){
            m_alpha=alpha;
            m_e=e;
            m_time=0;
            m_factor=1;
            m_numBounces=0;
            if (abs(m_alpha) <tol) { 
                m_alpha=tol; // avoids divide by zero
            }
            if (abs(m_e+1) < tol) {
                m_e=(tol) -1; // avoids divide by zero
            }
            m_t0=m_h/(m_v*sin(m_alpha));
            m_bounceUnit=1;
        }


        void bounce() { // doesn't change the object-state if called before reset
            float curBounceTime = m_t0/m_factor;
            m_time += curBounceTime;
            m_factor *= (1-m_e/(1+m_e));
            m_numBounces+= m_bounceUnit;
        }

        //READERS:
        float length() const { 
           return m_L;
        }
        float velocity() const { 
           return m_v;
        }

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
        float m_v, m_alpha,m_h,m_L,m_e;
        // Bounce-Tracking attributes
        float m_time,m_factor,m_t0;
        int m_numBounces,m_bounceUnit;
};

void operator << ( std::ostream & os , Path const & p)  {
    cout << p.curTime() << "," << p.x()   << "," << p.y() << ","
        << p.v_x()     << ", "<< p.v_y() << "," << p.v() << endl;
}

void printHeader (std::ostream & os){
    os << "curTime,x,y,v_x,v_y,v"<<endl;
}

class PathSim {


    public:
        PathSim(float v,float h, float L) : m_Path(v,h,L) {
        }

        void bounceUntilExit(float alpha,float e) {
            m_Path.reset(alpha,e);
            printHeader(cout);
            float totalTime= m_Path.length()/(m_Path.velocity()*cos(alpha));
            //cout << "cos(alpha)=" << cos(alpha) << " totaltime= " << totalTime << endl;

            // we don't care about exact position of cannon-ball as we want to know
            // the number of bounces

            while (m_Path.curTime() < totalTime ){
                cout << m_Path;
                m_Path.bounce();
            }
            cout << m_Path;
            cout << " num-bounces = " << m_Path.numBounces() -1 << endl;
        }

    private:
        PathSim(const PathSim & );
        void operator = (PathSim &);
        Path m_Path;
        float m_L;
};

int main(){
    /*
       SimpleRNG r;
       cout << "values" << endl;
       for (int i = 0 ; i < 1e+6; ++i) {
       cout << r.GetExponential(1) << endl;
       }
     */

    float alpha=3.14159/4;
    float v=2;
    float h=4;
    float L=10;
    float e=0;
    PathSim psim(v,h,L);
    psim.bounceUntilExit(alpha,e);;
    return 0;
}

