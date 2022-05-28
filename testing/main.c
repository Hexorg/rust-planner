#include<stdio.h>
#include "libhtn.h"

int main() {
    Domain* domain = load_domain("htn-problems/testing.htn");
    int plan_size = 0;
    char** _plan = plan(domain, &plan_size);
    printf("Gotten a plan with %d entries.\n", plan_size);
    for (int i=0; i<plan_size; i++) {
        printf("%s\n", _plan[i]);
    }
}