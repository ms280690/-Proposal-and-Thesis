struct node;
struct node startmarker;
struct node endmarker;
struct node chain;

struct node* init();
struct node* nextref(int unit);

void collapse(struct node* n);
void close_ref(struct node* n);
void inconly(struct node* n);
void expand(struct node* r,int max);

void finalize(char *stepfilename);





