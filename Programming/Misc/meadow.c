#include <stdio.h>

void meadows(int men);

int main(){
    meadows(4);
};

void meadows(int men){
    for(; men > 0; men--){
        if(men == 1){
            printf("1 man went to mow\nWent to mow a meadow\nOne man and his dog, Spot\nWent to mow a meadow\n");
        } else if(men > 1){
            printf("%d men went to mow\nWent to mow a meadow\n", men);
            for(int menleft = men - 1; menleft > 1; menleft--){
                printf("%d men, ", menleft);
            };
            printf("1 man and his dog, Spot\nWent to mow a meadow\n\n");
        } else if(men < 1){
            printf("ERROR, INSUFFICIENT MEN\n");
        }
    }
}
