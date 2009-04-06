#define IN_STG_CODE 0
#include "RtsAPI.h"
#include "Stg.h"
#ifdef __cplusplus
extern "C" {
#endif
 
HsBool KernelziUserSpace_d2n6i(StgStablePtr the_stableptr, void* original_return_addr, HsWord32 a1)
{
SchedulerStatus rc;
HaskellObj ret;
HsBool cret;
rts_lock();
rc=rts_evalIO(rts_apply((HaskellObj)runIO_closure,rts_apply((StgClosure*)deRefStablePtr(the_stableptr),rts_mkWord32(a1))) ,&ret);
rts_checkSchedStatus("KernelziUserSpace_d2n6i",rc);
cret=rts_getBool(ret);
rts_unlock();
return cret;
}
 
HsWord32 KernelziUserSpace_d2n6j(StgStablePtr the_stableptr, void* original_return_addr, HsWord32 a1, HsWord32 a2)
{
SchedulerStatus rc;
HaskellObj ret;
HsWord32 cret;
rts_lock();
rc=rts_evalIO(rts_apply((HaskellObj)runIO_closure,rts_apply(rts_apply((StgClosure*)deRefStablePtr(the_stableptr),rts_mkWord32(a1)),rts_mkWord32(a2))) ,&ret);
rts_checkSchedStatus("KernelziUserSpace_d2n6j",rc);
cret=rts_getWord32(ret);
rts_unlock();
return cret;
}
#ifdef __cplusplus
}
#endif

