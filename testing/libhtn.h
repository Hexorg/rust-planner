typedef struct _domain Domain;
typedef struct _stringVec Plan;

Domain* load_domain(char* filepath);
char** plan(Domain* domain, int* plan_size);