#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>

#include <linux/if_tun.h>
#include <linux/if.h>

#include "opentun.h"

#define DEVTUN "/dev/net/tun"

int config_tun(int fd,int flags,int async)
{
  int ret;
  struct ifreq ifr;
  memset(&ifr, 0, sizeof(ifr));
  ifr.ifr_flags = flags | IFF_NO_PI;
  strncpy(ifr.ifr_name, "tun%d", IFNAMSIZ);
  ret = ioctl(fd, TUNSETIFF, (void *) &ifr);
  if (ret != 0) {
    perror("ioctl");
    close(fd);
    return -1;
  }
  fprintf(stderr,"Connected fd %d to host network interface: %s\n",
	  fd, ifr.ifr_name);
  if(async) 
    if(fcntl(fd, F_SETFL, O_NONBLOCK|O_ASYNC)<0) {
      perror("fcntl");
      return -1;
    }
  return 0;
}

int tun_alloc(int flags)
{
  int fd,ret;
  
  fd = open(DEVTUN, O_RDWR);
  if (fd < 0) {
    perror(DEVTUN);
    return -1;
  }
  ret=config_tun(fd,flags,0);
  if(ret<0) {
    fprintf(stderr,"config_tun failed\n");
    return -1;
  }
  return fd;
}
