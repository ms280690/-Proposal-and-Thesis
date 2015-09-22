#include <stdlib.h>
#include <stdio.h>


struct node
{
  struct node* p;
  int  c;
  struct node* s;
};

struct node startmarker;
struct node endmarker;
struct node* chain;

struct node* newref()
{
  struct node* n;
  n=(struct node*) malloc(sizeof(struct node));
  n->c=0;
  return(n);
}

struct node* nextref(int unit)
{
  struct node* ref=chain;
  chain=chain->s;
  return(ref);
}

struct node* constructref(struct node* par, int cost, struct node* suc)
{
  struct node* n=newref();
  n->p=par;
  n->c=cost;
  n->s=suc;
  return(n);
}

void collapse(struct node* n)
{
  (n->p)->s=n->s;
  (n->s)->p=n->p;
  (n->s)->c+=n->c+1;
  free(n);
}

void close_ref(struct node* n)
{
  (n->p)->s=n->s;
  (n->s)->p=n->p;
  (n->s)->c+=n->c;
  free(n);
}

void inconly(struct node* n)
{
  n->c++;
}

void expand(struct node* r,int max)
{
  r->c++;
  struct node* last=r->s;
  struct node* r1=r;
  struct node* r2;

  int i=0;
  while(i<max)
    {
      r2=newref();
      r1->s=r2;
      r2->p=r1;
      r1=r2;
      i++;
    }
  r2->s=last;
  last->p=r2;

  chain=r->s;
}

struct node* init()
{
  startmarker.c=0;
  endmarker.c=0;
  endmarker.s=0;
  startmarker.p=0;

  struct node* mainR=constructref(&startmarker,0,&endmarker);
  startmarker.s=mainR;
  endmarker.p=mainR;

  return(mainR);
}

void finalize(char *stepfilename)
{
  struct node n=startmarker;
  FILE *stepfile;
  stepfile=fopen(stepfilename,"w");
  fprintf(stepfile,"[");
  while(n.s)
    {
      n=*(n.s);
      fprintf(stepfile,"%i",n.c);
      if(n.s){fprintf(stepfile,",");};
    }
  fprintf(stepfile,"]");
  fclose(stepfile);
}

void print_oracle()
{
  int i=0;
  struct node n=startmarker;
  printf("[");
  while(n.s && i<100)
    {
      n=*(n.s);
      printf("%i",n.c);
      if(n.s){printf(",");};
      i++;
    }
  if (n.s) {printf("...");} else {printf("]");} 
  
  n=endmarker;
  i=0;
  printf("[");

  while(n.p && i<100)
    {
      printf("%i",n.c);
      n=*(n.p);
      if(n.p){printf(",");};
      i++;
    }
  if (n.p) {printf("...");} else {printf("]");} 
  printf("\n");

}

/*
main() { 
   struct node* ref = init(); 

   expand(ref,1); 

   struct node* r1 = nextref(1); 
   inconly(ref); 
   collapse(ref); 
   close_ref(r1); 

   finalize("test.steps"); 
 } 
*/
