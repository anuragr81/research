
const uint32_t ascii_to_dec[] = {
     // 0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     //24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     //(48=(int)'0') ....
     //48,49,50,51,52,53,54,55,56,57,58,59
     0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0
};



bool parseDouble(int64_t  & msdec, const char *str_dig, int& dig_after_dot)
{
     msdec=0;
     uint32_t dot_true = 0x00;
     dig_after_dot = 0;
     while(str_dig) {
          uint32_t i = *str_dig;
          while (isdigit(i) ) {
               msdec = msdec * 10 + ascii_to_dec[i];
               dig_after_dot = (dot_true & ++dig_after_dot);
               str_dig++;
               i = *str_dig;
          }
          if (i == '.') {
               dot_true = 0xFFFFFFFF;
               str_dig++;
          } else  if ( i == '\0' || i == ' ' ) {
               break;
          } else {
               msdec=0;
               break;
          }
     }

     return true ;

}


bool parseInteger(int & result, char * ptr, const size_t sz)
{
     char * p_cur = ptr;
     result = 0;
     for (size_t i = 0; i < sz; ++i, p_cur++) {
          char ch = *p_cur;
          if (isdigit(ch)) {
               result = 10 * result + ascii_to_dec[(int)ch];
          }
          else if (ch == '.')
          {
              break;
          }
     }
     return (result >= 0);
}


void backTraceDump(void)
{

     using namespace abi;

     enum {
          MAX_DEPTH = 10
     };

     void *trace[MAX_DEPTH];

     Dl_info dlinfo;

     int status;
     const char *symname;
     char *demangled;

     int trace_size = backtrace(trace, MAX_DEPTH);

     printf("Call stack: \n");

     for (int i = 0; i < trace_size; ++i) {
          if (!dladdr(trace[i], &dlinfo))
               continue;

          symname = dlinfo.dli_sname;

          demangled = __cxa_demangle(symname, NULL, 0, &status);
          if (status == 0 && demangled)
               symname = demangled;

          printf("object: %s, function: %s\n", dlinfo.dli_fname, symname);

          if (demangled)
               free(demangled);
     }

}

