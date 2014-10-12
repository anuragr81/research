#include <iostream>
#include <math.h>
#include "SimpleRNG.h"

#define LOGPRINT TRUE

const float tol = 1e-6;
const double PI= 3.141592654;

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
        Path(float v, float h,float L,float e_m, SimpleRNG & prand) :m_v(v),
        m_h(h),
        m_L(L),
        m_emul(0),
        m_em(e_m),
        m_numBounces(0),
        m_factor(1),
        m_time(0), 
        m_alpha(0),
        m_t0(0),
        m_bounceUnit(0),
        m_pRand(prand){
        }

        //MODIFIERS:
        bool reset (float alpha){
            m_alpha=alpha;
            m_emul = 1;
            m_time=0;
            m_factor=1;
            m_numBounces=0;
            if (abs(m_alpha) <tol) { 
                m_alpha=tol; // avoids divide by zero
            }
            m_t0=m_h/(m_v*sin(m_alpha));
            m_bounceUnit=1;
        }


        void bounce() {  // doesn't change the state of the object if reset is not called
            float curBounceTime = m_t0/m_factor;
            m_time   += curBounceTime;
            float e   = m_pRand.GetExponential(m_em);
            m_factor *= m_emul*(1-e/(1+e));
#ifdef LOGPRINT
            cout << " m_factor " << m_factor << endl;
#endif
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

    private:

        // Attributes
        float m_v, m_alpha,m_h,m_L,m_em;
        SimpleRNG & m_pRand;
        // Bounce-Tracking attributes
        float m_time,m_factor,m_t0;
        int m_numBounces,m_bounceUnit,m_emul;
};

void operator << ( std::ostream & os , Path const & p)  {
    cout << p.curTime() << "," << p.x()   << "," << p.y() << ","
         << p.v_x() << endl;
}

void printHeader (std::ostream & os){
    os << "curTime,x,y,v_x"<<endl;
}


class ExitResults {
    public:
        ExitResults ():m_numBounces(0),m_exitTime(0) { }

        void reset (int nBounces,float exitTime) { 
            m_numBounces = nBounces;
            m_exitTime   = exitTime;
        }

        int numBounces() const {
            return m_numBounces;
        }
        float exitTime() const { 
            return m_exitTime;
        }
    private:
        int m_numBounces;
        float m_exitTime;
};


class PathSim {

    public:
        PathSim(float v,float h, float L, float e_m, SimpleRNG & prand) : 
            m_Path(v,h,L,e_m,prand),
            m_pRand(prand) {
            }

        void bounceUntilExit(float alpha,ExitResults & res ) {
            m_Path.reset(alpha);

#ifdef LOGPRINT
            printHeader(cout);
#endif
            float totalTime= m_Path.length()/(m_Path.velocity()*cos(alpha));

            // we don't care about exact position of cannon-ball as we want to know
            // the number of bounces

            while (m_Path.curTime() < totalTime ){

#ifdef LOGPRINT
                cout << m_Path;
#endif
                m_Path.bounce();
            }
#ifdef LOGPRINT
            cout << m_Path;
#endif
            res.reset(m_Path.numBounces() -1,totalTime);
        }

    private:
        PathSim(const PathSim & );
        void operator = (PathSim &);
        Path m_Path;
        float m_L;
        SimpleRNG & m_pRand;
};

int main(){

    SimpleRNG r;

    float v=1;
    float h=5;
    float L=100;
    float e_m=.15;

    PathSim psim(v,h,L,e_m,r);

    const long unsigned int numAlphaSamples=2;

    float bouncesAverage=0;
    float squaredBouncesAverage=0;

    float exitTimeAverage = 0 ;
    float squaredExitTimeAverage = 0 ;

    ExitResults res;

    for ( int i = 1 ; i <= numAlphaSamples ; ++i)
    {
        psim.bounceUntilExit((PI/4)*r.GetUniform(),res);

        bouncesAverage=(res.numBounces()+bouncesAverage*(i-1))/i;
        squaredBouncesAverage=(res.numBounces()*res.numBounces()+squaredBouncesAverage*(i-1))/i;

        exitTimeAverage=(res.exitTime()+exitTimeAverage*(i-1))/i;
        squaredExitTimeAverage=(res.exitTime()*res.exitTime()+squaredExitTimeAverage*(i-1))/i;
    }

        cout << " average-num-bounces = " << bouncesAverage <<endl;
        cout << " variance-num-bounces/n = " << (squaredBouncesAverage - bouncesAverage*bouncesAverage)/numAlphaSamples <<endl;
        cout << " average-exitTime= " << exitTimeAverage <<endl;
        cout << " variance-exitTime/n= " << (squaredExitTimeAverage-exitTimeAverage*exitTimeAverage)/numAlphaSamples <<endl;

    return 0;
}

