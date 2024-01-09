#define _GNU_SOURCE
#include <string.h>
#include <sys/syscall.h>
#include <linux/random.h>
#include <unistd.h>

ssize_t getrandom(void *buf, size_t buflen, unsigned int flags) {
    memset(buf, 0, buflen);
    return buflen;
}
