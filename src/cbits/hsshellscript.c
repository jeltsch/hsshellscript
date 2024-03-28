/* Common place for the C parts of HsShellScript. */

#include <errno.h>
#include <fcntl.h>
#include <glob.h>
#include <limits.h>
#include <mntent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

/*#include "HsShellScript/Commands.chs.h"
#include "HsShellScript/ProcErr.chs.h"
#include "HsShellScript/Misc.chs.h"
*/

/* Commands.chs */

char* hsshellscript_get_realpath(char* path)
{
  static char tmp[PATH_MAX+1];
  return realpath(path, tmp);
}

char* hsshellscript_get_readlink(char* path)
{
  static char tmp[PATH_MAX+1];
  int count = readlink(path, tmp, PATH_MAX);
  if (count == -1) return 0;
  tmp[count] = 0;
  return tmp;
}


/* Misc.chs */

int hsshellscript_open_nonvariadic(const char *pathname, int flags, mode_t mode)
{
  open(pathname, flags, mode);
}

int do_glob(void* buf0, const char* pattern)
{
  glob_t* buf = (glob_t*) buf0;
  int ret;
  buf->gl_pathv = 0;

  ret = glob(pattern, GLOB_ERR, 0, buf);

  switch (ret) {
    case 0:            return 0;
    case GLOB_ABORTED: return 1;
    case GLOB_NOSPACE: return 2;
    case GLOB_NOMATCH: return 3;
  }
}


/* ProcErr.chs */


/* Save all file descriptor flags in an array */
int* c_save_fdflags(void)
{
  int  maxfds = sysconf(_SC_OPEN_MAX);
  int* flags  = calloc(maxfds, sizeof(int));
  int  fd;

  for (fd = 0; fd < maxfds; fd++)
     /* Saves -1 for invalid fds */
     flags[fd] = fcntl(fd, F_GETFL);

  return flags;
}

/* Restore all file descriptor flags from the array, and free it */
void c_restore_fdflags(int* flags)
{
  int  maxfds = sysconf(_SC_OPEN_MAX);
  int  fd;

  for (fd = 0; fd < maxfds; fd++)
     if (flags[fd] != -1)
        fcntl(fd, F_SETFL, flags[fd]);

  free(flags);
}

/* Duplicate a file descriptor, allocating the new one at min or above */
int c_fcntl_dupfd(int fd, int min)
{
  return fcntl(fd, F_DUPFD, min);
}

/* Prepare all file descriptors for a subsequent exec */
void c_prepare_fd_flags_for_exec(void)
{
  int maxfds = sysconf(_SC_OPEN_MAX);
  int fd, flags;

  /* Set fds 0-2 to blocking mode */
  for (fd = 0; fd < 3; fd++) {
     flags = fcntl(fd, F_GETFL);
     fcntl(fd, F_SETFL, flags & ~O_NONBLOCK);
  }

  /* Set all other fds to close-on-exec */
  for (fd = 3; fd < maxfds; fd++) {
     flags = fcntl(fd, F_GETFL);
     fcntl(fd, F_SETFL, flags | FD_CLOEXEC);
  }
}

/* Set a file descriptor to "close on exec" mode. Returns the old flags. */
int c_close_on_exec(int fd)
{
  int old_flags;
  old_flags = fcntl(fd, F_GETFL);
  fcntl(fd, F_SETFL, old_flags | FD_CLOEXEC);
  return old_flags;
}

/* Set the flags of a file descriptor. Returns the old flags. */
int c_set_flags(int fd, int new_flags)
{
  int old_flags;
  old_flags = fcntl(fd, F_GETFL);
  fcntl(fd, F_SETFL, new_flags);
  return old_flags;
}

// Thanks to Jan-Benedict Glaw for this
int c_terminal_width(int fd)
{
  int res;
  struct winsize size;

  res = ioctl (fd, TIOCGWINSZ, &size);

  if (res == -1) return -1;
  else return size.ws_col;
}
