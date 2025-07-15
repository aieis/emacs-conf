#include <string.h>
#include <stdlib.h>

typedef struct String_View
{
    char* Buffer;
    int Size;
    int HeapSize;
} String_View;

void String_View_PutChr(String_View* sv, char c)
{
    if (sv->Size + 1 >= sv->HeapSize)
    {
        sv->HeapSize = (sv->HeapSize + 1) * 2;
        sv->Buffer = realloc(sv->Buffer, sv->HeapSize);
    }

    sv->Buffer[sv->Size++] = c;
}

void String_View_PutStr(String_View* sv, const char* str, int size)
{
    if (sv->Size + size >= sv->HeapSize)
    {
        sv->HeapSize = (sv->HeapSize + size) * 2;
        sv->Buffer = realloc(sv->Buffer, sv->HeapSize);
    }

    memcpy(sv->Buffer + sv->Size, str, size);
    sv->Size += size;
}

void String_View_PutStrEscQuotes(String_View* sv, const char* str, int size)
{
    int escaping = 0;
    for (int i = 0; i < size; i++)
    {
        if (str[i] == '"' && !escaping)
        {
            String_View_PutChr(sv, '\\');
            String_View_PutChr(sv, '"');
        }
        else
        {
            String_View_PutChr(sv, str[i]);
        }

        escaping = !escaping && str[i] == '\\';
    }
}
