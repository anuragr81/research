#include <cmath>
#include <stdexcept>
#include <sstream>
#include <iostream>
#include "SimpleRNG.h"

SimpleRNG::SimpleRNG()
{
    m_u = 521288629;
    m_v = 362436069;
}

double SimpleRNG::GetUniform(unsigned int& u, unsigned int& v)
{
    unsigned int z = GetUint(u, v);
    return z*2.328306435996595e-10;
}

unsigned int SimpleRNG::GetUint(unsigned int& u, unsigned int& v)
{
    v = 36969*(v & 65535) + (v >> 16);
    u = 18000*(u & 65535) + (u >> 16);
    return (v << 16) + u;
}

double SimpleRNG::GetUniform()
{
    return GetUniform(m_u, m_v);
}

double SimpleRNG::GetExponential(double mean)
{
    return -mean*log( GetUniform() );
}

