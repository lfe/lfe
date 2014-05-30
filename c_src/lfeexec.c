/*
 * Copyright (c) 2008-2014 Robert Virding
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#define DEFAULT_PROGNAME "erl"

static char **Eargv = NULL;	/* Argument array for erl call */
static int Eargc = 0;		/* Argument count */

void error(char *format, ...);

int main(int argc, char **argv) {
  char *emu;			/* Emulator */
  char pa[1024];		/* Path */
  char *arg;
  int i;

  /* The erl program and the ebin directory */
  emu = DEFAULT_PROGNAME;

  sprintf(pa, "%s/ebin", getenv("LFE_ROOTDIR"));

  /* Allocate and initialise the erl argument array. */
  Eargv = malloc(sizeof(*argv) * (argc + 10));
  Eargc = 0;
  Eargv[Eargc++] = emu;		/* The program we are going to run */

  /*
   * Collect all +flags and -flags. We follow the same handling as in
   * 'erl' EXCEPT that as soon as we reach a "plain" argument all the
   * rest also become "plain" arguments and will be prefixed with
   * -extra.  These arguments are then handed over as is the LFE boot
   * to do as it pleases.
   */

  i = 1;
  while (i < argc) {
    arg = argv[i];
    if (strcmp(arg, "-extra") == 0 || strcmp(arg, "--") == 0) {
      /* We're explicitly done. */
      i += 1;
      break;
    }
    else if (arg[0] == '-' || arg[0] == '+') {
      /* We have a flag, collect its arguments as well. */
      Eargv[Eargc++] = arg;
      for (i += 1; i < argc; i++) {
	arg = argv[i];
	if (arg[0] == '-' || arg[0] == '+')
	  break;
	else
	  Eargv[Eargc++] = arg;
      }
    }
    else			/* Plain argument. */
      break;
  }

  /* Add path, call to lfe_init and -extra for arguments. */
  Eargv[Eargc++] = "-pa";
  Eargv[Eargc++] = pa;
  if (i < argc) {
    Eargv[Eargc++] = "-noshell";
  }
  Eargv[Eargc++] = "-user";
  Eargv[Eargc++] = "lfe_init";
  Eargv[Eargc++] = "-extra";

  while (i < argc)		/* Add the rest to the stack */
    Eargv[Eargc++] = argv[i++];
  Eargv[Eargc] = NULL;		/* Terminate the stack */

  execvp(emu, Eargv);		/* Start erl */
  error("Error %d executing \'%s\'.", errno, emu);
}

void error(char* format, ...)
{
    char sbuf[1024];
    va_list ap;

    va_start(ap, format);
    snprintf(sbuf, sizeof(sbuf), format, ap);
    va_end(ap);
    fprintf(stderr, "lfeexec: %s\n", sbuf);
    exit(1);
}
