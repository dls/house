#include "ipc.h"

#define src  3
#define dest 4
#define notimeout 1

extern void sender(), receiver();

void start() {
   tc(src, src, sender);
   tc(dest, dest, receiver);
   printf("sender address is %x\n", sender);
   
   stop();
}

void sender() {
   printf("I am the sender\n");
   int i;
   int tag;
   int us[2];
   struct Msg m;
   struct IPCRet x;

   for (i = 0; i<10; i+=2) {
     us[0] = i;
     us[1] = i+1;
     m.untyped = us;
     tag = setMsg(&m, 2, 0);
     send(dest,notimeout,tag,&x);
     printf("I just sent %d and %d\n", i, i+1);
   }

   stop();
}

void receiver() {
   int total = 0;
   struct IPCRet x;

   printf("I am the receiver\n");
   for (;;) {
      recv(src,notimeout,&x);
      total += x.mr1 + x.mr2;
      printf("I just received %d and %d from %d, total is now %d\n", 
              x.mr1, x.mr2, x.from, total);
   }
   stop();
}

