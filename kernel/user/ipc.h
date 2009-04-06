
#define MAPITEM   0x8
#define GRANTITEM 0xA

struct Msg {
  int* untyped;
  //int* typed;
};

struct IPCRet {
  int from;
  int mr0;
  int mr1;
  int mr2;
};

struct Typed {
  int mi;
  int miPlus1;
};

