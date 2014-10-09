#include <iostream>
#include <math.h>

/* No exceptions are thrown */

using namespace std;

class Path{

    public:
        Path(float v, float alpha, float h ,float e) : m_v(v), 
        m_alpha(alpha),
        m_h(h),
        m_e(e),
        m_factor(1),
        m_time(0){
           if (m_alpha==0) { 
              m_alpha=1e-6;
           }
           m_t0=m_h/(m_v*sin(m_alpha));
        }
    float curTime() const {
        return m_time;
    }
    void bounce() {
        m_time += m_t0/m_factor;
        m_factor *= (1-m_e/(1+m_e));
    }

    private:
        float m_v, m_alpha,m_h,m_e;
        float m_time,m_factor,m_t0;
};

void calculate(float m,float v,float h, float L,float e){
    float alpha=m;
    Path p(v,alpha,h,e);
    float totalTime= L/v*cos(alpha);
    while (p.curTime() < totalTime ){
        cout << " CurTime=" << p.curTime() << endl;
        cout << " TotalTime=" << totalTime << endl;
        p.bounce();
    }

}

int main(){
    float m=1;
    float v=2;
    float h=5;
    float L=100;
    float e=.1;
    calculate(m,v,h,L,e);;
    return 0;
}


