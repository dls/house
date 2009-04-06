#include "ipc.h"

/*
void root() {
   forkDomain("sample");
   threadExit();
}

*/

#define sync 2
#define src  3
#define dest 4
#define notimeout 1

extern void lock(), sender(), receiver();

void start() {
   tc(sync, sync, lock);
   tc(src, src, sender);
   tc(dest, dest, receiver);
   stop();
}

void lock() {
   struct IPCRet x;

   for (;;) {
     recv(-1, notimeout, &x);
     sendEmpty(x.from, notimeout);
   }
}

#define LOCK(x) sendEmpty(sync,notimeout); x; recvEmpty(sync,notimeout)

void sender() {
   int i;
   int tag;
   int us[2];
   struct Msg m;
   struct IPCRet x;

   LOCK(printf("I am the sender\n"));

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

   LOCK(printf("I am the receiver\n"));
   for (;;) {
      recv(src, notimeout, &x);
      total += x.mr1 + x.mr2;
      printf("I just received %d and %d from %d, total is now %d\n", 
              x.mr1, x.mr2, x.from, total);
   }

   stop();
}

