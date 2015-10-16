#include <stdio.h>

int sumThreeFive(int i);

int main() {
    sumThreeFive(1000);
};

int sumThreeFive(int i){
    int sum = 0;
    for (int c = 1; c < i; c = c + 1){
        if (c % 3 == 0 || c % 5 == 0){
            sum = sum + c;
        }
    }
    printf("The sum of all multiples of 3 or 5 under %d is %d.\n", i, sum);
    return sum;
};
