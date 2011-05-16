#include <stdio.h>

/* Reads ascii call detail records from stdin.
 * Writes scampRec binary call detail records on stdout.
 */

typedef struct
{
  int onpa, obase;
  int dnpa, dbase;
  int con;
  int dur;  
} scampRec_t;



int readAscii(FILE *in, scampRec_t *r){
  int success =  fscanf(in, "%d %d %d %d %d %d\n",
		&(r->onpa), &(r->obase), 
                &(r->dnpa), &(r->dbase),
		&(r->con),  &(r->dur)); 
  return success;
}

void writeBinary(FILE *out, scampRec_t *r){
  fwrite(r, sizeof(scampRec_t), 1, out);
}

int main(){
  scampRec_t current;
  while (readAscii(stdin, &current) != EOF)
    writeBinary(stdout, &current);
  return 0;
}
