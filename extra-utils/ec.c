#include <process.h>

#include "string_view.h"

#define SIZE_ARGS 1020
#define EXTRA_ARGS 5

int main(int argc, char** argv)
{
    char* nargs[SIZE_ARGS + EXTRA_ARGS + 1] = {};

    int pos = 0;
    nargs[pos++] = "emacsclientw.exe";
    nargs[pos++] = "-n";
    nargs[pos++] = "-c";
    nargs[pos++] = "-a";
    nargs[pos++] = "\"\"";

    int delim = argc - 1 < SIZE_ARGS ? argc - 1: SIZE_ARGS;


    for (int i = 0; i < delim; i++)
    {
        String_View sv = {0, 0, 0};
        String_View_PutChr(&sv, '"');
        String_View_PutStrEscQuotes(&sv, argv[i+1], strlen(argv[i+1]));
        String_View_PutStr(&sv, "\"\0", 2);
        nargs[i + EXTRA_ARGS] = sv.Buffer;
    }

    nargs[delim + EXTRA_ARGS] = 0;
    execvp(nargs[0], nargs);
}
