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

#define YES 1
#define NO 0

#define DEFAULT_PROGNAME "erl"

static char **Eargv = NULL;	/* Argument array for erl call */
static int Eargc = 0;		/* Argument count */

#ifdef __WIN32__
#define QUOTE(s) possibly_quote(s)
#else
#define QUOTE(s) s
#endif

#define PUSH(s) Eargv[Eargc++] = QUOTE(s)

/* Local functions. */

static void error(char *format, ...);
static char *emalloc(size_t size);
#ifdef __WIN32__
static char* possibly_quote(char *arg);
#endif

int main(int argc, char **argv) {
  char *emu;			/* Emulator */
  char *rootdir;		/* $LFE_ROOTDIR */
  char *path;			/* Path */
  char *arg;
  int i;
  int eval = 0;			/* Are we LFE 'eval'ing? */

  /* The erl program and the ebin directory */
  emu = DEFAULT_PROGNAME;

  rootdir = getenv("LFE_ROOTDIR");
  if (rootdir == NULL) {
      error("LFE_ROOTDIR envionment variable is not set");
  }
  path = emalloc(strlen(rootdir) + 6);
  sprintf(path, "%s/ebin", rootdir);

  /* Allocate and initialise the erl argument array. */
  Eargv = (char **)emalloc(sizeof(*argv) * (argc + 16));
  Eargc = 0;
  PUSH(emu);			/* The program we are going to run */

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
    else if (strcmp(arg, "-eval") == 0 || strcmp(arg, "-lfe_eval") == 0) {
      eval = 1;
      i += 1;
      break;
    }
    else if (arg[0] == '-' || arg[0] == '+') {
      /* We have a flag, collect its arguments as well. */
      if (strcmp(arg, "-erl_eval") == 0)
	PUSH("-eval");
      else
	PUSH(arg);
      for (i += 1; i < argc; i++) {
	arg = argv[i];
	if (arg[0] == '-' || arg[0] == '+')
	  break;
	else
	  PUSH(arg);
      }
    }
    else			/* Plain argument. */
      break;
  }

  /*
   * Add the middle bits to the end, the path and a -noshell flag if
   * there are more arguments.
   */

  PUSH("-pa");
  PUSH(path);
  if (i < argc) {
    PUSH("-noshell");
  }
  PUSH("-user");
  PUSH("lfe_init");		/* will call lfe_init:start() */
  PUSH("-extra");

  /* Check if we are 'eval'ing and add -lfe_eval flag */
  if (eval) {
    PUSH("-lfe_eval");
  }
  /* Add the rest to the stack and terminate it. */
  while (i < argc) {
    PUSH(argv[i++]);
  }
  Eargv[Eargc] = NULL;

  i = 0;
  while ((arg = Eargv[i++])) {	/* To keep the compiler quiet */
    printf("|%s| ", arg);
  }
  printf("\n");

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

static char*
emalloc(size_t size)
{
  char *p = malloc(size);
  if (p == NULL)
    error("Insufficient memory");
  return p;
}

#ifdef __WIN32__
static char*
possibly_quote(char* arg)
{
  int must_quote = NO;
  int qlen = 0;			/* Quoted string length */
  char* s;
  char* new_arg;

  if (arg == NULL)		/* We are now safer */
    return arg;

  /*
   * Scan the string to find out if it needs quoting and return
   * the original argument if not.
   */

  for (s = arg; *s; s++, qlen++) {
    switch(*s) {
    case ' ':
      must_quote = YES;
      continue;
    case '"':
      must_quote = YES;
      qlen++;
      continue;
    case '\\':
      if(s[1] == '"')
	qlen++;
      continue;
    default:
      continue;
    }
  }

  if (!must_quote) {
    return arg;
  }

  /*
   * Insert the quotes and put a backslash in front of every quote
   * inside the string.
   */

  s = new_arg = emalloc(qlen+2+1);
  for (*s++ = '"'; *arg; arg++, s++) {
    if (*arg == '"' || (*arg == '\\' && arg[1] == '"')) {
      *s++ = '\\';
    }
    *s = *arg;
  }
  if (s[-1] == '\\') {
    *s++ ='\\';
  }
  *s++ = '"';
  *s = '\0';
  return new_arg;
}
#endif
