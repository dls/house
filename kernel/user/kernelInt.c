
#include "kernelInt.h"
#include "regs.h"

void KernelInterface(struct KIntRet* x) {
  int base_addr, version, flags, id;

  asm volatile (
    "int $0x80"
    : _EAX (base_addr), _ECX (version), _EDX (flags), _ESI (id)
    :
    : "memory");

  x->base_addr   = base_addr;
  x->api_version = version;
  x->api_flags   = flags;
  x->kernel_id   = id;
}

