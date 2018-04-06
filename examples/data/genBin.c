#include <stdio.h>

/* Reads ascii call detail records from stdin.
 * Writes scampRec binary call detail records on stdout.
 */

typedef struct
{
  unsigned char uc;
  signed char sc;
  unsigned short us;
  signed short ss;
  signed short ss2;
  unsigned int ui;
  signed int si;
  unsigned long long ull;
  signed long long sll;
} binRec_t;



int readAscii(FILE *in, binRec_t *r){
  int success =  fscanf(in, "%hhu %hhd %hu %hd %hd %u %d %llu %lld\n",
		&(r->uc), &(r->sc), 
		&(r->us), &(r->ss), &(r->ss2),
                &(r->ui), &(r->si),
		&(r->ull),&(r->sll)); 
  return success;
}

void writeBinary(FILE *out, binRec_t *r){
  fwrite(r, sizeof(binRec_t), 1, out);
}

int main(){
  binRec_t current;
  while (readAscii(stdin, &current) != EOF)
    writeBinary(stdout, &current);
  return 0;
}
