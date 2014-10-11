#ifndef SIMPLERNG_H
#define SIMPLERNG_H

class SimpleRNG
{
public:
    
    SimpleRNG();

    double GetUniform();

    double GetUniform(unsigned int& u, unsigned int& v);

    unsigned int GetUint(unsigned int& u, unsigned int& v);
        
private:
    unsigned int m_u, m_v;
};


#endif
