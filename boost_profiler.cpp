#include <iostream>
#include <memory>
#include <list>
#include <map>
#include <unordered_map>

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/thread/thread.hpp>


/*
 * Key Optimizations: 
 *     1. No exceptions
 *     2. No new object creations
 *     3. 
 */

char * fastitoa(/*char * buf,*/ int i){
    return "(NA)";
}

class FieldMap{

  /* creates a heavy object - but it is pretty much 
     a message factory - that we use to generate asciis
     when needed */

  public:
    std::string const & toString() {
        // quickfix approach to maintain the 
        // string at every addition (so that toString) 
        // returns immediately is better

        m_str="";
        for (auto it = m_mapInts.begin();it!=m_mapInts.end();
                ++it){
             m_str+=fastitoa(it->second);
        }
        return m_str; 
    }

    void parseMessage(char * buf){
      //parse size 10 , parse size 11, parse size 12 etc.
      //TODO: Answer Question 3 i.e. comparison with while(loop)

    }

    explicit FieldMap(std::list<int> const & supported_fields):
      m_supportedfields(supported_fields) {
      for (auto it = supported_fields.begin();it!=supported_fields.end();
          ++it){
        m_mapInts[*it]=0;
      }
    }

    void setField(int field, int value){
#ifdef __DEBUG__
       auto it = m_supportedfields.find(field);
       if (*it == m_mapInts.end()){ 
          throw 2;
       }
#endif
      m_mapInts[field]=value; // field already exists
    }
    // avoid stale values by setting active field
    int getField(int field) {
       auto it  = m_mapInts.find(field);
#ifdef __DEBUG__
       if (*it == m_mapInts.end()){ 
          throw 2;
       }
#endif
       // no exception checks for optimization
       return it->second & m_active;
    }

  private:
    std::map<int,int> m_mapInts;
    std::list<int> m_supportedfields;
    std::string m_str;
    int m_active;
};

class FieldHashMap{
  public:
    void setField(int field, int value){
      m_mapInts[field]=value;
    }
  private:
    std::unordered_map<int,int> m_mapInts;
};

void func(){
 for (int i= 0 ; i< 1e+6 ; ++i);
}

int main(){
  using namespace std;
  boost::posix_time::ptime now  = boost::posix_time::microsec_clock::local_time();
  func();
  //boost::this_thread::sleep(boost::posix_time::millisec(500));
  boost::posix_time::ptime tick = boost::posix_time::microsec_clock::local_time();
  boost::posix_time::time_duration diff = tick- now;
  cout << "Time Spent in ms:" << diff.total_milliseconds() << endl;
  return 0;
}
