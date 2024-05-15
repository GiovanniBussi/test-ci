#include "plumed/tools/Subprocess.h"
#include <fstream>
#include <unistd.h>

using namespace PLMD;

int main(){
  // if processes are not properly terminated and cleaned
  // only a finite number of them will be allowed

  std::ofstream ofs("should_be_empty");


  for(unsigned i=0;i<1000;i++) {
    try {
      Subprocess sp("yes");
    } catch(...) {
      ofs<<"failed after "<<i<<"\n";
      break;
    }
  }
  return 0;
}
