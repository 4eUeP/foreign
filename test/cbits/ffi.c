#include <HsFFI.h>
#include <Rts.h>

#include <pthread.h>

HsInt sum_first_unsafe(StgArrBytes** bufs, HsInt len) {
  HsInt res = 0;
  for (HsInt ix = 0; ix < len; ix++) {
    res = res + ((HsInt*)(bufs[ix]->payload))[0];
  }
  return res;
}

HsInt sum_first_safe(HsInt** bufs, HsInt len) {
  HsInt res = 0;
  for (HsInt ix = 0; ix < len; ix++) {
    res = res + bufs[ix][0];
  }
  return res;
}

typedef struct sum_first_callback_args {
  HsInt** bufs;
  HsInt len;
  HsStablePtr mvar;
  HsInt cap;
  HsInt* value_out;
} sum_first_callback_args;

void* sum_first_callback(void* args_) {
  sum_first_callback_args* args = (sum_first_callback_args*)args_;

  HsInt res = 0;
  for (HsInt ix = 0; ix < args->len; ix++) {
    res = res + args->bufs[ix][0];
  }
  *(args->value_out) = res;
  hs_try_putmvar(args->cap, args->mvar);
}

void async_sum_first(HsInt** bufs, HsInt len, HsStablePtr mvar, HsInt cap,
                     HsInt* value_out) {
  sum_first_callback_args* args = malloc(sizeof(sum_first_callback_args));
  args->bufs = bufs;
  args->len = len;
  args->mvar = mvar;
  args->cap = cap;
  args->value_out = value_out;

  pthread_t p;
  pthread_create(&p, NULL, sum_first_callback, (void*)args);
}
